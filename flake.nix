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

      mkPkgs = system:
        import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };

      mkHome = system:
        home-manager.lib.homeManagerConfiguration {
          pkgs = mkPkgs system;
          extraSpecialArgs = { inherit inputs hostConfig; };
          modules = [ ./nix/home.nix ];
        };
    in
    {
      darwinConfigurations.${hostConfig.darwinHost} = darwin.lib.darwinSystem {
        system = hostConfig.darwinSystem;
        pkgs = mkPkgs hostConfig.darwinSystem;
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

      homeConfigurations.${username}@${hostConfig.wslHost} =
        mkHome hostConfig.wslSystem;
    };
}
