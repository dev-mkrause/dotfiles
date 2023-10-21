(define-module (mk system base)
  #:use-module (srfi srfi-1)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu packages mozilla)
  #:use-module (nongnu system linux-initrd))

(use-service-modules cups desktop networking ssh xorg docker virtualization pm)
(use-package-modules linux cups wm fonts)

(operating-system
 (kernel linux-lts)
 (firmware (list linux-firmware))
 (locale "de_DE.utf8")
 (timezone "Europe/Berlin")
 (keyboard-layout (keyboard-layout "de" #:options '("ctrl:nocaps")))
 (host-name "cachy")

 (users (cons* (user-account
                (name "mkrause")
                (comment "Marvin Krause")
                (group "users")
                (home-directory "/home/mkrause")
                (supplementary-groups '("wheel" "netdev" "audio" "video" "docker" "kvm")))
               %base-user-accounts))

 (packages (append (map specification->package
                        (list "nss-certs"
                              "hunspell"
                              "hunspell-dict-de"
                              "emacs"
                              "firefox"
                              "git"
                              "openssh"
                              "htop"
                              "podman"

                              "sway"
                              "xorg-server-xwayland"
                              "swayidle"
                              "swaybg"
                              "wofi"
                              "pavucontrol"
                              "alacritty"))
                   %base-packages))

 (services
  (append (list

              (service console-font-service-type
                 (map (lambda (tty)
                        ;; Use a larger font for HIDPI screens
                        (cons tty (file-append
                                   font-terminus
                                   "/share/consolefonts/ter-132n")))
                      '("tty1" "tty2" "tty3")))


           (service greetd-service-type
                    (greetd-configuration
                     (greeter-supplementary-groups (list "video" "input"))
                     (terminals
                      (list
                       ;; TTY1 is the graphical login screen for Sway
                       (greetd-terminal-configuration
                        (terminal-vt "1")
                        (terminal-switch #t)
                        (default-session-command
                          (greetd-agreety-session
                           (command (file-append sway "/bin/sway"))
                           (command-args '()))))

                       ;; Set up remaining TTYs for terminal use
                       (greetd-terminal-configuration (terminal-vt "2"))
                       (greetd-terminal-configuration (terminal-vt "3"))))))

           ;; Power and thermal management services
           (service thermald-service-type)
           (service tlp-service-type
                    (tlp-configuration
                     (cpu-boost-on-ac? #t)
                     (wifi-pwr-on-bat? #t)))

           ;; Enable Docker containers and virtual machines
           (service docker-service-type)
           (service libvirt-service-type
                    (libvirt-configuration
                     (unix-sock-group "libvirt")
                     (tls-port "16555")))

           ;; Enable printing and scanning
           (service sane-service-type)
           (service cups-service-type
                    (cups-configuration
                     (web-interface? #t)
                     (extensions
                      (list cups-filters))))

           (service screen-locker-service-type
                    (screen-locker-configuration
                     (using-pam? #t)
                     (using-setuid? #f)
                     (name "swaylock")
                     (program (file-append swaylock "/bin/swaylock")))))


          (modify-services %desktop-services
                           (delete mingetty-service-type) ;; Greetd https://guix.gnu.org/manual/en/html_node/Base-Services.html
                           (delete login-service-type) ;; Greetd
                           (delete console-font-service-type) ;; Greetd
                           (delete gdm-service-type)
                           (elogind-service-type config =>
                                                 (elogind-configuration (inherit config)
                                                                        (handle-lid-switch-external-power 'suspend)))

                           (guix-service-type config => (guix-configuration
                                                         (inherit config)
                                                         (substitute-urls
                                                          (append (list "https://substitutes.nonguix.org")
                                                                  %default-substitute-urls))
                                                         (authorized-keys
                                                          (append (list (local-file "./nonguix-signing-key.pub"))
                                                                  %default-authorized-guix-keys)))))))


 (bootloader (bootloader-configuration
              (bootloader grub-efi-bootloader)
              (targets (list "/boot/efi"))
              (keyboard-layout keyboard-layout)))

 (file-systems (cons* (file-system
                       (mount-point "/home")
                       (device (uuid
                                "bf66f3c8-3d41-4887-ada2-a8a574b997e5"
                                'btrfs))
                       (type "btrfs"))
                      (file-system
                       (mount-point "/")
                       (device (uuid
                                "4d6c2662-8e6e-4150-8209-bfe9f09aca75"
                                'btrfs))
                       (type "btrfs"))
                      (file-system
                       (mount-point "/boot/efi")
                       (device (uuid "C31C-4679"
                                     'fat32))
                       (type "vfat"))
                      %base-file-systems))
 (swap-devices (list (swap-space
                      (target "/swapfile")))))
