;; Diese Datei mit Ihrer Persönlichen Umgebung kann an 'guix home reconfigure'
;; übergeben werden, um den Inhalt Ihres Profils nachzubilden. Sie ist
;; "symbolisch", gibt also nur die Namen der Pakete an. Um genau das gleiche
;; Profil herauszubekommen, müssen Sie auch die verwendeten Kanäle nachbilden,
;; wie "guix describe" sie anzeigt. Siehe den Abschnitt "Guix nachbilden"
;; im Handbuch.

(use-modules (gnu home)
             (gnu packages)
             (gnu packages bash)
             (gnu packages backup)
             (gnu services)
             (guix gexp)
             (gnu home services shells)
             (gnu home services mcron)
             (gnu home services))

(home-environment
 ;; Unterhalb ist die Liste der Pakete, die in Ihrem Persönlichen Profil
 ;; in ~/.guix-home/profile verfügbar sein werden.
 (packages (specifications->packages
            (list
             "emacs"
             "ripgrep"
             "fd"
             "emacs-vterm"
             "emacs-pdf-tools"
             "emacs-all-the-icons"
             "texinfo"

             "texlive-scheme-basic"
             "texlive-biblatex"
             "texlive-collection-latexrecommended"
             "texlive-collection-fontsrecommended"
             "texlive-pdfextra"
             "texlive-capt-of"
             "texlive-ulem"
             "texlive-beamer"
             "texlive-wrapfig"
             "texlive-hyperref"
             "texlive-microtype"
             "texlive-pgf"
             "texlive-latexmk"

             "unzip"
             "restic"
             "flameshot"
             "gnucash"
             "direnv"

             "steam"
             "signal-desktop"

             "awscli"


             "flatpak"
             "flatpak-xdg-utils"
             "xdg-desktop-portal-gtk"
             "xdg-dbus-proxy"

             "docker-compose"

             "font-iosevka"
             "font-iosevka-aile"
             "font-fira-code")))

 ;; Unterhalb ist die Liste Ihrer Persönlichen Dienste.  Um nach
 ;; verfügbaren Diensten zu suchen, führen Sie den Befehl
 ;; 'guix home search SCHLÜSSELWORT' in einem Terminal aus.
 (services
  (list

   (simple-service 'mkrause-mcron-home-services home-mcron-service-type
                   ;; Every day at 12:15 and 19:15.
                   ;; Requires ~/.restic.env for setting AWS Users credentials and region alonog with RESTIC_PASSWORD and RESTIC_REPOSITORY environment variables.
                   (list #~(job '(next-minute-from (next-hour '(12 19)) '(15))
                                (string-append #$bash "/bin/bash -c 'source $HOME/.restic.env && " (string-append #$restic "/bin/restic backup $HOME/Dokumente'")))))

   (simple-service 'my-env-vars
                   home-environment-variables-service-type `(("XDG_DATA_DIRS" . "/var/lib/flatpak/exports/share:$HOME/.local/share/flatpak/exports/share:$XDG_DATA_DIRS")))

   (service home-bash-service-type
            (home-bash-configuration



             (aliases '(("grep" . "grep --color=auto") ("ll" . "ls -l")
                        ("ls" . "ls -p --color=auto")
                        ("dotfiles" .  "git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME")))
             (bashrc (list (local-file
                            "/home/mkrause/.config/guix/home/.bashrc"
                            "bashrc")))
             (bash-profile (list (local-file
                                  "/home/mkrause/.config/guix/home/.bash_profile"
                                  "bash_profile"))))))))
