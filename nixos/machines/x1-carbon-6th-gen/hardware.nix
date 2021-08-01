# A good source of information about how to fix the issues still
# standing with kernel 4.6.11 is the following wiki page:
# https://wiki.archlinux.org/index.php/Lenovo_ThinkPad_X1_Carbon_(Gen_6). The
# TrackPoint and TouchPad issues there seem to have been fixed already.
#
# Enable the lower-power S3 suspend state by upgrading the BIOS to version >= 1.30,
# then manually selecting Linux in the power management section.
{ config, pkgs, lib, modulesPath, ... }:
{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  networking.hostName = "wand-x1";

  boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usbhid" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ "i915" ];
  boot.kernelModules = [ "kvm-intel" "acpi_call" "tp_smapi"];
  boot.kernelPackages = pkgs.linuxPackagesFor (pkgs.linux_5_4.override {
    argsOverride = {
      version = "5.4.107";
    };
  });
  boot.extraModulePackages = with config.boot.kernelPackages; [ acpi_call tp_smapi ];
  boot.blacklistedKernelModules = lib.optionals (!config.hardware.enableRedistributableFirmware) [
    "ath3k"
  ];

  boot.cleanTmpDir = false;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # SMART
  services.smartd.enable = true;
  services.smartd.notifications.x11.enable = true;

  # ssd
  boot.kernel.sysctl = {
    "vm.swappiness" = lib.mkDefault 1;
  };

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/f77a2026-788a-4fe9-9331-9be1e4ac3bca";
      fsType = "ext4";
      options = [ "noatime" "discard" ];
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/F088-98CB";
      fsType = "vfat";
      options = [ "noatime" "discard" ];
    };

  swapDevices = [ ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  # high-resolution display
  hardware.video.hidpi.enable = lib.mkDefault true;
  hardware.pulseaudio.enable = true;
  hardware.trackpoint.enable = lib.mkDefault true;
  hardware.trackpoint.emulateWheel = lib.mkDefault config.hardware.trackpoint.enable;
  hardware.trackpoint.device = "TPPS/2 Elan TrackPoint";

  # cpu/intel
  hardware.cpu.intel.updateMicrocode =
    lib.mkDefault config.hardware.enableRedistributableFirmware;
  
  hardware.opengl.extraPackages = with pkgs; [
    vaapiIntel
    vaapiVdpau
    libvdpau-va-gl
    intel-media-driver
  ];
  
  services.xserver.libinput.enable = lib.mkDefault true;

  services.fstrim.enable = lib.mkDefault true;

  services.tlp.enable = lib.mkDefault true;

  services.throttled.enable = lib.mkDefault true;

  # Fingerprint reader: login and unlock with fingerprint (if you add one with `fprintd-enroll`)
  services.fprintd.enable = true;
}
