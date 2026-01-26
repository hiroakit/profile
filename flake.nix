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
      devcontainerHost = {
        username = "vscode";
        darwinHost = "devcontainer";
        darwinSystem = hostConfig.darwinSystem;
        wslHost = "devcontainer";
        wslSystem = hostConfig.wslSystem;
      };

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
              home-manager.users.${username} = import ./nix/home.nix;
            }
          ];
        };
    in
    {
      darwinConfigurations.${hostConfig.darwinHost} =
        mkDarwin hostConfig.darwinSystem;

      homeConfigurations."${username}@${hostConfig.wslHost}" =
        mkHome hostConfig.wslSystem;
      homeConfigurations."${devcontainerHost.username}@${devcontainerHost.wslHost}" =
        mkHomeWith { system = devcontainerHost.wslSystem; hostCfg = devcontainerHost; };

      checks = lib.genAttrs
        (lib.unique [ hostConfig.wslSystem hostConfig.darwinSystem ])
        (system:
          if system == hostConfig.darwinSystem
          then { darwin-config = (mkDarwin system).system; }
          else {
            home-wsl = (mkHome system).activationPackage;
            home-devcontainer =
              (mkHomeWith { system = system; hostCfg = devcontainerHost; }).activationPackage;
          });
    };
}
