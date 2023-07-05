---
layout:     post
title:      "Setup a Home Server"
date:       2023-06-28 11:13:18 +0900
categories: linux
---

A server is just a computer.
And it is usually quite easy to install an OS.
So who want read this?

Well, it is not that simple to enable some feature after install.
And unfortunately, some installer do not support those features.
And also, it is quite frustrating to find missing drivers or misconfiguration.
So I decide to perform a hybrid installation.

Ready? Here we go.

## Our Goal

After the installation, we expect a Linux server running Debian,
with

- full disk encryption,
- non-interactive passwordless start, and
- ssh enabled.

A passwordless startup with encrypted disk is somewhat like
BitLocker of Windows or FileVault of macOS.
The key is that the TPM firmware store some data securely and would only make
the data available if the boot image is trusted.

### Is TPM Safe?
Well, it depends.
Nothing can help you if the OS itself is vulnerable or if you use a weak
password.
And also, it seems some UEFI firmwares do not change PCR7 depending on
signature found.
If it is your case, you will have to use something different methods,
or back to using a password.

## Installation

### Do the Preparation
Boot with the installation disk,
and select `Advanced options ... -> Expert install`.

Perform `Detect and mount installation media` and
`Load installer components from installation media`.

Check the following items and continue.

- `crypto-dm-modules`, to setup disk encryption
- `fdisk-udeb`, to partition disk
- `network-console`, we would type a lot,
  it would be better to have it so that we can copy-paste some texts,
  but you can go on without it.
- `rescue-mode`, required to access the tools selected

Perform `Detect network hardware` and `Configure the network`.

### Enable SSH (optional)
Perform `Continue installation remotely using SSH`.
Type the password and then open an SSH session to it.

Choose `Start installer (expert mode)` from your SSH session.

### Configure the Installer, Part 1

Now, you can configure the locale, keyboard, users and passwords, and clock.

Perform `Detect disks`, and do not continue the `Partition disks`.

### Partition Disk

Choose `Execute a shell`.

Now you have a shell as a working Linux system, but lack of some features.

Use `fdisk -l` to find your disks.
NVME disks usually have the name like `/dev/nvmeNnN`, where `N` is a number.
Note that devices like `/dev/nvmeN` are NOT disks.
SATA disks usually have the name like `/dev/sdA`, where `A` is a letter.
If the information given by `fdisk` is not enough for you to identify
the disks, you can use `udevadm info /dev/nvme0n1` to find more information.

We suppose you are going to install Debian on `/dev/nvme0n1`.

Use `fdisk /dev/nvme0n1` to partition the disk.

We need it has a GPT partition table, with 2 partitions.

- A partition with about 1GiB, with partition type set to EFI System.
  The boot image finally produced will be less than 50MiB size at the end of
  the installation.
  You can make the partition smaller if you are very sure it is enough.
  We suppose this partition has the name `/dev/nvme0n1p1`,
- A partition to hold Linux file systems. You can use all remaining spaces.
  We suppose this partition has the name `/dev/nvme0n1p2`,

Use `fdisk -l /dev/nvme0n1` to verify partitions.

### Setup Disk Encryption

Now we are going to encrypt `/dev/nvme0n1p2`.
The step is not required,
but the purpose of this article is going to setup a safe server.
Skipping the step would give you a running server but meaningless to do it
manually.
If you **really** want to skip the step,
use `/dev/nvme0n1p2` directly instead of `/dev/mapper/linux_crypt`.

#### Use LUKS 2 with Default

A LUKS partition stores additional metadata at the beginning of the partition.
It can store the encrypted keyfile there to allow you change password later.
Also, it can be recognized as a LUKS partition by other tools,
preventing accidently demaging the data.
If you are not sure what to do, this solution is a reasonable choice.

Execute `cryptsetup luksFormat /dev/nvme0n1p2` to create a LUKS volume.
It will ask the password, make sure you never forgot it,
or no one can help you.
And use a strong password, or the encryption would be meaningless.

Execute `cryptsetup open /dev/nvme0n1p2 linux_crypt` to open the volume.

Now the created encrypted volume is available at `/dev/mapper/linux_crypt`,
You can also use other name than `linux_crypt`.

#### Use Detached LUKS Header

**Warning!**
A LUKS volume with detached header looks no difference with random data or
zeroes,
make sure you do not overwrite it accidently.
And there is no way to tell if you have use the right header to decrypt the
disk.

The LUKS metadata actually saves the keyfile,
and leaks the information that there are something interesting stored here,
to potential attackers.
We can save the LUKS metadata elsewhere to make it even safer.

LUKS header is a 16MiB block of data, you can save it anywhere.
Some useful choice can be

- A partition larger than 16MiB on a USB disk.
  This solution also enables two-factor authentication.
  It is nearly impossible for anyone without the USB disk,
  or precisely, the 16MiB data, to decrypt the data.
- A file on a USB disk file system.
  This solution is similar to the previous,
  but you have to mount the file system before you can decrypt it.
- Also, you can put it somewhere on the same disk,
  but it might not be useful to have a detached header.

We assume you are use `/dev/sdd1` as the detached header.

Execute
`cryptsetup luksFormat /dev/nvme0n1p2` to
create a LUKS volume.
It will ask the password, make sure you never forgot it,
or no one can help you.
And use a strong password.
We add the offset with 32768 sectors, or 16MiB,
in case we want to attach the header to it.

Execute
`cryptsetup open --header=/dev/sdd1 /dev/nvme0n1p2 linux_crypt`
to open the volume.

Now the created encrypted volume is available at `/dev/mapper/linux_crypt`,
You can also use other name than `linux_crypt`.

### Setup LVM (optional)

There are already plenty of articles about pros and cons for LVM.
We are not going to discuss why you should use it or not.
The step is not required.
If you decide to skip the step,
use `/dev/mapper/linux_crypt` instead of `/dev/linux-vg/root-lv`.

Execute `pvcreate /dev/mapper/linux_crypt` to create a physical volume.

Execute `vgcreate linux-vg /dev/mapper/linux_crypt` to create a volume group
with physical volume created before.

Execute `lvcreate -L 8GiB linux-vg -n root-lv` to create a logical volume.
Or execute `lvcreate -l 100%FREE linux-vg root-lv` to create a logical volume
to fill all free spaces.

Now the created logical volume should be available at `/dev/linux-vg/root-lv`

### Install the System

Execute `mkfs.fat -F 32 /dev/nvme0n1p1` to format the EFI partition.

You can create additional partition for swap,
but make sure to encrypt them also.

Execute `exit` to exit the shell, back to the main menu.

Select `Partition disks -> Manual`.
Since we have already set everything up,
we just have to tell it the mount point.

Select the `root-lv` volume we have already created,
Set it as
```
Use as:      Ext4 journaling file system
Mount point: /
```

Select `Finish partitioning and write changes to disk`.

### Configure the Installer, Part 2

Install the system as usual, but do not install GRUB, we do not need it.

Select `Continue without boot loader`.

### Create an EFI Executable from Linux Kernel

Go back to the shell with `Execute a shell`.

Now the root volume should be mounted at `/target`,
and the EFI volumen should be mounted at `/target/boot/efi`.
Use `mount` to verify that they are mounted correctly.

Execute the following commands to mount the special directories.

```
mount --rbind /dev  /target/dev
mount --rbind /sys  /target/sys
mount --rbind /proc /target/proc
```

And then execute `chroot /target /bin/bash`.

Now we have a shell in our new system.
You can install some handy tools if you want.
Note that all the following operations are executed in a `chroot` environment,
you should prepend `/target` to some paths if you are not.

#### Create an SSL Certificate

This step can be done on another machine.

Execute the following commands to generate SSL keys.
```
openssl req -new -x509 -newkey rsa:2048 -subj "/CN=Your Name Here DB/" 
        -keyout DB.key -out DB.crt -days 3650 -nodes -sha256
openssl x509 -in DB.crt -out DB.cer -outform DER
```

Copy `DB.key`, `DB.crt`, `DB.cer` to the machine if you generated on another
machine.

Make sure that only `root` can access `DB.key`.

#### Create Bootable Image

Create a file named `/etc/kernel/postinst.d/zz-update-efistub` with the 
following contents.

```
#!/bin/sh

objcopy \
    --add-section .osrel="/usr/lib/os-release" --change-section-vma .osrel=0x20000 \
    --add-section .cmdline="/cmdline" --change-section-vma .cmdline=0x30000 \
    --add-section .linux="/vmlinuz" --change-section-vma .linux=0x40000 \
    --add-section .initrd="/initrd.img" --change-section-vma .initrd=0x3000000 \
    /usr/lib/systemd/boot/efi/linuxx64.efi.stub /boot/efi/EFI/BOOT/BOOTX64.EFI

sbsign \
    --key /root/secureboot/keys/DB.key \
    --cert /root/secureboot/keys/DB.crt \
    --output /boot/efi/EFI/BOOT/BOOTX64.EFI \
    /boot/efi/EFI/BOOT/BOOTX64.EFI
```

The `--key` and `--cert` should point to your generated key files.

Then create a file named `/cmdline` with following contents.

```
root=/dev/linux-vg/root-lv rootfstype=ext4 add_efi_memmap panic=0 ro quiet
```

Now we also need the script to be executed when the initramfs got updated.

Execute the following commands.

```
mkdir -p /etc/initramfs/post-update.d
ln -s ../../kernel/postinst.d/zz-update-efistub /etc/initramfs/post-update.d/zz-update-efistub
chmod +x /etc/kernel/postinst.d/zz-update-efistub
mkdir -p /boot/efi/EFI/BOOT
```

Now we need to install the commands required by the scripts.
Execute
`apt-get install binutils systemd-boot-efi sbsigntool lvm2 cryptsetup-initramfs openssh-server`.

Execute the script we just created.

Now copy the `DB.cer` to the EFI partition,
so we can install it with UEFI firmware.

#### Create `crypttab`

We need to tell Linux to decrypt the partition.

Use `blkid /dev/nvme0n1p2` to find `PARTUUID` of it.

Add the following line to `/etc/crypttab`

```
linux_crypt PARTUUID=xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx none luks
```

If you enabled detached header,
you should also find the `PARTUUID` of `/dev/sdd1`.

And replace the line with

```
linux_crypt PARTUUID=xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx none luks,header=/dev/disk/by-partuuid/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
```

Execute `update-initramfs -u -k all` to regenerate initramfs.

Exit the shell. We need to exit it twice because we are in chroot environment.

### Configure the Installer, Part 3

Choose `Finish the installation` from the installer menu and complete it.
And then the machine will reboot.

### Load the Certificate to UEFI

If you enabled, you should indeed,
the machine would refuse to boot the EFI image we created.
We need to tell it to trust our certificate.

The methods varies from machine to machine.
But in general, you should

- find a UEFI configuration named like `Secure Boot`,
- switch the secure boot mode from `Standard` to `Custom`,
- load certificate,
- select the `DB.cer` we stored in the EFI partition,
- save the changes and
- reboot.

### First Boot

If you used detached LUKS header,
make sure you always have your USB device plugged.

You will have to type your password at first time.
Then we could do everything else with SSH.

### Setup TPM

Install the following packages with `apt-get`

- `tpm2-initramfs-tool`
- `xxd`

Create a file named `/etc/initramfs-tools/tpm2-cryptsetup` and make it
executable.

```
#!/bin/sh

[ "$CRYPTTAB_TRIED" -lt "1" ] && exec tpm2-initramfs-tool unseal --pcrs 0,1,2,7

stty -echo
echo -n "Please enter the passphrase for $CRYPTTAB_NAME ($CRYPTTAB_SOURCE): " >&2
read pass
echo >&2
stty echo
echo -n $pass
```

Create another file named `/etc/initramfs-tools/hooks/tpm2-initramfs-tool` and
make it executable.

```
. /usr/share/initramfs-tools/hook-functions

copy_exec /usr/lib/x86_64-linux-gnu/libtss2-tcti-device.so.0
copy_exec /usr/bin/tpm2-initramfs-tool

copy_exec /etc/initramfs-tools/tpm2-cryptsetup
```

Execute `head -c 64 /dev/random | xxd -p -c999 | tr -d '\n' > /root/luks-key`
to generate a strong key.

Add the key with
`cryptsetup luksAddKey /dev/nvme0n1p2 /root/luks-key`.
If you have a detached header, you should execute against the header device.

Execute the following command to store the key in TPM.

```
tpm2-initramfs-tool seal --data $(cat /root/luks_key) --pcrs 0,1,2,7
```

Edit the `/etc/crypttab` with

```
linux_crypt PARTUUID=xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx unseal luks,keyscript=/etc/initramfs-tools/tpm2-cryptsetup
```
Execute `update-initramfs -u -k all` to regenerate initramfs.

And reboot then it will boot without asking you password.

## Acknowledgements and References

This article would be impossible without the following articles, thanks a lot.

- Philippe Daouadi,
  [***The ultimate guide to Full Disk Encryption with TPM and Secure Boot 
  (with hibernation support!)***](https://blastrock.github.io/fde-tpm-sb.html)
- Contributors of ArchWiki,
  [dm-crypt/Specialties on ArchWiki](https://wiki.archlinux.org/title/Dm-crypt/Specialties)
- Contributors of ArchWiki,
  [LVM on ArchWiki](https://wiki.archlinux.org/title/LVM)
