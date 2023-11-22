{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "mkrause";
  home.homeDirectory = "/home/mkrause";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.05"; # Please read the comment before changing.
  targets.genericLinux.enable = true;
  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with pkgs; [
    hledger
    ripgrep
    git

    clojure
    cargo

    signal-desktop
    discord
    brave
    spotify

    # Sway Packages
    brightnessctl

    (pkgs.nerdfonts.override { fonts = [ "FiraCode" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. If you don't want to manage your shell through Home
  # Manager then you have to manually source 'hm-session-vars.sh' located at
  # either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/mkrause/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
#    EDITOR = "emacsclient -c -a emacs";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs.bash = {
    enable = true;
  };

  programs.nushell = {
    enable = true;
  };

  programs.direnv.enable = true;
  programs.direnv.enableBashIntegration = true;
  programs.direnv.enableNushellIntegration = true;

  programs.wofi.enable = true;

  programs.foot = {
    enable = true;

    settings = {
      main = {
        term = "xterm-256color";

        font = "Fira Code:size=9";
        dpi-aware = "yes";
      };

      mouse = {
        hide-when-typing = "yes";
      };
    };
  };

  programs.emacs.enable = true;
  services.emacs = {
    enable = true;
    defaultEditor = true;
    package = pkgs.emacs29;
  };
}
