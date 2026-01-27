{
  description = "hiroakit dotfiles";

  nixConfig = {
    experimental-features = [ "nix-command" "flakes" ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    darwin.url = "github:LnL7/nix-darwin";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nixpkgs, darwin, home-manager, ... }:
    let
      hostConfig = import ./nix/hosts.nix;
      username = hostConfig.username;
      lib = nixpkgs.lib;
      devcontainerHosts = [
        {
          username = "vscode";
          darwinHost = "devcontainer";
          darwinSystem = hostConfig.darwinSystem;
          wslHost = "devcontainer";
          wslSystem = hostConfig.wslSystem;
        }
        # GitHub Codespaces often uses the "codespace" user (Dotfiles feature runs as the current user).
        {
          username = "codespace";
          darwinHost = "devcontainer";
          darwinSystem = hostConfig.darwinSystem;
          wslHost = "devcontainer";
          wslSystem = hostConfig.wslSystem;
        }
      ];

      mkPkgs = system:
        import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };

      mkHomeWith = { system, hostCfg }:
        home-manager.lib.homeManagerConfiguration {
          pkgs = mkPkgs system;
          extraSpecialArgs = { inherit inputs; hostConfig = hostCfg; };
          modules = [ ./nix/home.nix ];
        };

      mkHome = system:
        mkHomeWith { inherit system; hostCfg = hostConfig; };

      mkDarwin = system:
        darwin.lib.darwinSystem {
          inherit system;
          pkgs = mkPkgs system;
          specialArgs = { inherit inputs hostConfig; };
          modules = [
            ./nix/darwin.nix
            home-manager.darwinModules.home-manager
            {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = { inherit inputs hostConfig; };
            home-manager.users.${username} = { ... }: {
              imports = [ ./nix/home.nix ];
              home.username = username;
              home.homeDirectory = "/Users/${username}";
            };
            }
          ];
        };
    in
    {
      darwinConfigurations.${hostConfig.darwinHost} =
        mkDarwin hostConfig.darwinSystem;

      homeConfigurations."${username}@${hostConfig.wslHost}" =
        mkHome hostConfig.wslSystem;
      homeConfigurations =
        {
          "${username}@${hostConfig.wslHost}" = mkHome hostConfig.wslSystem;
        }
        // lib.listToAttrs (map
          (dc:
            {
              name = "${dc.username}@${dc.wslHost}";
              value = mkHomeWith { system = dc.wslSystem; hostCfg = dc; };
            })
          devcontainerHosts);

      checks = lib.genAttrs
        (lib.unique [ hostConfig.wslSystem hostConfig.darwinSystem ])
        (system:
          if system == hostConfig.darwinSystem
          then { darwin-config = (mkDarwin system).system; }
          else {
            home-wsl = (mkHome system).activationPackage;
            home-devcontainer-vscode =
              (mkHomeWith { system = system; hostCfg = builtins.elemAt devcontainerHosts 0; }).activationPackage;
            home-devcontainer-codespace =
              (mkHomeWith { system = system; hostCfg = builtins.elemAt devcontainerHosts 1; }).activationPackage;
          });
    };
}
