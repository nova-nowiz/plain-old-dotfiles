{ config, pkgs, lib, ... }:

let
  home-manager = builtins.fetchGit {
    url = "https://github.com/rycee/home-manager.git";
    rev = "abfb4cde51856dbee909f373b59cd941f51c2170";
  };
in
{
    imports = [
      (import "${home-manager}/nixos")
    ];

    home-manager.users = {
      narice = {
        # Let Home Manager install and manage itself.
        programs.home-manager.enable = true;

        home.packages = with pkgs; [ # I don't want my packages to be built using the nixos chennel...
          wget
          git
          tridactyl-native
          fzf
          ranger
          neovim
          discord
          ripcord
          exa
          ytop
          dust
          fd
          ripgrep
          ripgrep-all
          korganizer
          hunspell
          hunspellDicts.en-us
          hunspellDicts.fr-any
          pandoc
          emacs
          htop
          keepassxc
          killall
          rhythmbox
          w3m
          wev
          steam
          lutris
          minecraft
          teams
          aseprite
          akonadi
        ];

        nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [ # I don't want this to be mixed with my nixos stuff
          "discord"
          "ripcord"
          "steam"
          "steam-original"
          "steam-runtime"
          "minecraft-launcher"
          "teams"
          "aseprite"
        ];
      };
    };
}
