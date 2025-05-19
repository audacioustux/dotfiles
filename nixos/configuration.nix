# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ config, lib, pkgs, ... }:
let
  home-manager = builtins.fetchTarball "https://github.com/nix-community/home-manager/archive/release-24.11.tar.gz";
in
{
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      (import "${home-manager}/nixos")
    ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    consoleLogLevel = 3;
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      timeout = 3;
    };
    initrd = {
      luks.devices = {
        cryptroot = {
          device = "/dev/disk/by-uuid/afb56c19-9adf-42f6-b3dd-3a0df7e5b9f3";
        };
      };
    };
  };

  networking.hostName = "audacioustux"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager = {
    enable = true;  # Easiest to use and most distros use this by default.
    wifi.powersave = true;
  };
  networking.nameservers = [
    "1.1.1.1"
    "1.0.0.1"
    "8.8.8.8"
    "8.8.4.4"
  ];

  # Set your time zone.
  time.timeZone = "Asia/Dhaka";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkb.options in tty.
  # };

  fonts = {
    enableDefaultPackages = true;
    packages = with pkgs; [
      font-awesome
      noto-fonts
      noto-fonts-emoji
      nerdfonts
    ];
    fontconfig = {
      defaultFonts = {
        emoji = ["Noto Color Emoji"];
	monospace = ["JetBrainsMono Nerd Font"];
	serif = ["Noto Serif" "Noto Serif Bengali"];
	sansSerif = ["Noto Sans" "Noto Sans Bengali"];
      };
    };
  };

  # Enable the X11 windowing system.
  # services.xserver.enable = true;

  # Configure keymap in X11
  # services.xserver.xkb.layout = "us,bd";
  # services.xserver.xkb.variant = "intl,probhat";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # autologin to primary user
  services.greetd = {
    enable = true;
    vt = 2;
    settings = {
      initial_session = {
        command = "uwsm start -S hyprland-uwsm.desktop";
	user = "audacioustux";
      };
      default_session = {
        command = "${pkgs.greetd.tuigreet}/bin/tuigreet --asterisks --remember --time";
        user = "greeter";
      };
    };
  };

  # environment variables
  environment.sessionVariables = { 
    LIBVA_DRIVER_NAME = "iHD"; # Force intel-media-driver
    NIXOS_OZONE_WL = "1";
  };

  hardware = {
    opentabletdriver.enable = true;
    graphics = {
      enable = true;
      extraPackages = with pkgs; [
        intel-media-driver # LIBVA_DRIVER_NAME=iHD
        intel-vaapi-driver # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
        libvdpau-va-gl
      ];
    };
    bluetooth = {
      enable = true; # Enable the Bluetooth stack.
      powerOnBoot = true; # Power on the Bluetooth adapter automatically on boot.
      settings = {
        # Configure general Bluetooth settings.
        General = {
          # Enable common Bluetooth profiles (Audio Source/Sink, Media control, Serial Port).
          Enable = "Source,Sink,Media,Socket";
          # Enable experimental Bluetooth features if needed.
          Experimental = true;
        };
      };
    };
  };

  # Enable RealtimeKit daemon, allowing PipeWire/PulseAudio to request real-time scheduling for lower audio latency.
  security.rtkit.enable = true;

  # Enable sound.
  # hardware.pulseaudio.enable = true;
  # OR
  services.pipewire = {
    enable = true;
    alsa = {
      enable = true;
      support32Bit = true;
    };
    pulse.enable = true;
    wireplumber.enable = true;
  };

  # Enable GVFS (GNOME Virtual File System) for features like trash support, mounting external media, etc. in GTK apps.
  services.gvfs.enable = true;
  # Enable Tumbler D-Bus service for generating thumbnails for file managers.
  services.tumbler.enable = true;
  # Enable udev, the device manager for Linux.
  services.udev.enable = true;
  # Enable envfs FUSE filesystem for managing environment variables (less common).
  services.envfs.enable = true;
  # Enable D-Bus message bus system, crucial for inter-process communication in modern desktops.
  services.dbus.enable = true;

  # Enable automatic weekly TRIM command for SSDs to maintain performance.
  services.fstrim = {
    enable = true;
    interval = "weekly";
  };

  # Enable touchpad support (enabled default in most desktopManager).
  services.libinput.enable = true;

  # Enable the Blueman applet/manager for Bluetooth devices.
  services.blueman.enable = true;

  # Enable fwupd service for updating device firmware (requires UEFI Secure Boot disabled or shimmed).
  services.fwupd.enable = true;

  # Enable UPower service for monitoring power devices (battery, AC adapter) and managing power states.
  services.upower.enable = true;

  programs = {
    zsh = {
      enable = true;
      enableCompletion = true;
      enableBashCompletion = true;
      autosuggestions.enable = true;
      syntaxHighlighting.enable = true;
      ohMyZsh = {
        enable = true;
	theme = "minimal";
        plugins = [ "git" "zoxide" "fzf" ];
      };
      shellAliases = {
        ll = "ls -l";
	rm = "rm -i";
      };
      histSize = 100000;
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users = {
    users.audacioustux = {
      isNormalUser = true;
      extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
      shell = pkgs.zsh;
      packages = with pkgs; [
        # browser
        microsoft-edge
	# file manager
	nautilus
	yazi
        # terminal
        alacritty
        # games
        unstable.zeroad
        # desktop
        dunst
        rofi-wayland
        brightnessctl
	glib
        libnotify
	inotify-tools
        libappindicator
	libsecret
        ueberzugpp
	hyprpaper
	hypridle
	hyprlock
	hyprcursor
	hyprpicker
	waybar
	pavucontrol # PulseAudio Volume Control
    	pamixer # Command-line mixer for PulseAudio
    	bluez # Bluetooth support
    	bluez-tools # Bluetooth tools
	wl-clipboard
        # utils
	jq
        bat
        ffmpeg
        zoxide
        ripgrep
        fzf
	awscli2
	git-credential-manager
	git-remote-codecommit
	broot
	devbox
	octave
	docker-client
	kubectl
	sops
	# utils - cloud
	flyctl
        # tui
        btop
        fastfetch
        vscode
        # cursor
        banana-cursor
	# screenshot
	grim
	slurp
        # apps
        obs-studio
	loupe
	slack
	vlc
	notion
	krita
	inkscape
	blender
	teams-for-linux
	dbeaver-bin
	# secrets manager
	keepassxc
	# language runtime
	deno
      ];
    };
  };

  environment.shells = with pkgs; [ zsh ];
  system.userActivationScripts.zshrc = "touch .zshrc";

  nixpkgs = {
    config = {
      allowUnfree = true;
      packageOverrides = pkgs: {
        unstable = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz") {};
        intel-vaapi-driver = pkgs.intel-vaapi-driver.override { enableHybridCodec = true; };
      };
    };
  };

  # Hyprland
  programs.hyprland = {
    enable = true;
    withUWSM = true;
  };

  # programs.firefox.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    neovim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  programs.git = {
    enable = true;
    lfs.enable = true;
    config = {
      init = {
        defaultBranch = "main";
      };
      safe.directory = "/etc/nixos";
      user = {
        name = "audacioustux";
        email = "tanjimhossain.pro@gmail.com";
      };
    };
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = true;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  system.copySystemConfiguration = true;

  # Compressed memory
  zramSwap = {
    enable = true;
    priority = 100;
    memoryPercent = 30;
    swapDevices = 1;
    algorithm = "zstd";
  };

  # Power management
  powerManagement.enable = true;
  services.thermald.enable = true;
  services.tlp = {
    enable = true;
    settings = {
      CPU_SCALING_GOVERNOR_ON_AC = "performance";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";

      CPU_ENERGY_PERF_POLICY_ON_BAT = "power";
      CPU_ENERGY_PERF_POLICY_ON_AC = "performance";

      CPU_MIN_PERF_ON_AC = 0;
      CPU_MAX_PERF_ON_AC = 100;
      CPU_MIN_PERF_ON_BAT = 0;
      CPU_MAX_PERF_ON_BAT = 20;

      STOP_CHARGE_THRESH_BAT0 = 80;
    };
  };

  # Service to monitor disk health using S.M.A.R.T. Currently disabled.
  services.smartd = {
    enable = true;
    # Automatically detect devices to monitor.
    autodetect = true;
  };

  # Auto upgrade
  system.autoUpgrade = {
    enable = true;
    operation = "boot"; # If you don't want to apply updates immediately, only after rebooting, use `boot` option in this case
    dates = "daily";
  };

  # Nix package manager
  nix = {
    settings = {
      auto-optimise-store = true;
    };
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 14d";
    };
  };

  # This option defines the first version of NixOS you have installed on this particular machine,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
  # to actually do that.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "24.11"; # Did you read the comment?

  # Home manager
  home-manager.users.audacioustux = { pkgs, ... }: {
    home.file.".vscode/argv.json".text = builtins.toJSON {
      enable-crash-reporter = false;
      password-store = "gnome-libsecret";
    };

    /* The home.stateVersion option does not have a default and must be set */
    home.stateVersion = "24.11";
  };

}

