{ config, pkgs, inputs, ... }:

{
  # By default, Home Manager uses a private pkgs instance. Use the system-level nixpkgs
  home-manager.useGlobalPkgs = true;
  # Magic: https://discourse.nixos.org/t/users-users-name-packages-vs-home-manager-packages/22240
  home-manager.useUserPackages = true;

  home-manager.users.acc = { pkgs, ... }: {
    home.packages = [ pkgs.git ];
    programs.bash.enable = true;

    # The state version is required and should stay at the version you *originally* installed.
    home.stateVersion = "24.11";
  };

  home-manager.users.ha = { pkgs, ... }: {
    imports = [ inputs.quadlet-nix.homeManagerModules.quadlet ];
    # This is crucial to ensure the systemd services are (re)started on config change
    systemd.user.startServices = "sd-switch";

    virtualisation.quadlet = {
      networks = {
        ha = {};
      };
      containers = {
        zwave-js-ui = {
          autoStart = true;
          serviceConfig = {
            Restart = "always";
          };
          containerConfig = {
            image = "registry.hub.docker.com/zwavejs/zwave-js-ui:latest";
            publishPorts = [ "0.0.0.0:8091:8091" ];
            userns = "keep-id";
            networks = [ "ha" ];
            # devices = [ "/dev/ZWAVE:/dev/zwave" ];
            volumes = [ "/home/ha/zwave-js/store:/usr/src/app/store" ];
            addGroups = [ "keep-groups" ];
          };
        };
      };
    };

    # The state version is required and should stay at the version you *originally* installed.
    home.stateVersion = "24.11";
  };
}
