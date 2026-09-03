# -*- mode: ruby -*-
#
# Local OpenBSD VM for building/testing rpki-prover: debugging a build
# failure, checking rsync flag compatibility, running the daemon against
# real repositories, etc.
#
# Requires Vagrant plus a provider it can drive (VirtualBox is the default
# below; libvirt/QEMU also works, pass --provider=libvirt to `vagrant up`).
# Neither Vagrant nor a hypervisor is installed by this repo -- install
# whichever you already use, or VirtualBox if unsure.
#
# Usage:
#   vagrant up          # boots + provisions (installs ghc/cabal/rsync/etc as root)
#   vagrant rsync        # pushes the project tree into the VM (see note below)
#   vagrant ssh -c 'cd /vagrant && ./build-openbsd.sh'
#   vagrant ssh -c 'cd /vagrant && ./scripts/smoke-test-openbsd.sh "$(cabal list-bin rpki-prover:exe:rpki-prover)"'
#   vagrant destroy   # when done
#
# Note on synced folders: OpenBSD guests don't have VirtualBox Guest
# Additions, so the native vboxsf shared folder won't work. This forces
# Vagrant's "rsync" sync type instead, which shells out to the host's
# rsync. It only syncs on `vagrant rsync` (see rsync__auto below) -- it is
# NOT live. Re-run `vagrant rsync` after editing files on the host.
#
# rsync__auto is disabled on purpose: Vagrant's built-in "install rsync into
# the guest if missing" step runs as the unprivileged `vagrant` user with no
# doas/sudo escalation, so it 500s with "pkg_add must be run as root" if
# rsync isn't already there -- which it never is, on a fresh box. Our own
# shell provisioner below installs rsync as part of its dependency list
# (running as root, like all Vagrant shell provisioners by default), so by
# the time you run `vagrant rsync` by hand after `vagrant up`, rsync is
# already present and Vagrant's capability check skips the broken install
# path entirely.
#
# IMPORTANT, one-time setup: as of writing, "generic/openbsd7" is pinned to
# OpenBSD 7.4 across every published box version, and OpenBSD only keeps
# packages live on its mirrors for the current + previous release -- so a
# fresh 7.4 box has NO installable packages at all (pkg_add fails with
# "no such dir"). There's no reliable long-term archive for old releases
# either, so the fix is to upgrade the box once, then keep the upgraded
# image instead of the pristine one:
#
#   vagrant up                 # boots the pristine (7.4) box
#   vagrant ssh
#     doas sysupgrade          # one release at a time -- repeat until
#     # (VM reboots itself; `vagrant ssh` back in, `uname -r`, repeat)
#     # `uname -r` reads a current release (7.8 as of writing -- check
#     # what's actually live on the mirrors first, see DEVELOPER.md)
#   vagrant halt
#   vagrant package --output openbsd-current.box
#   vagrant box add openbsd-current-local openbsd-current.box
#
# Then either edit BOX_NAME below, or leave it and run:
#   RPKI_OPENBSD_BOX=openbsd-current-local vagrant up
# The env var takes precedence so this file doesn't need a personal box
# name hardcoded into it -- everyone doing this setup picks their own.
BOX_NAME = ENV.fetch("RPKI_OPENBSD_BOX", "generic/openbsd7")

Vagrant.configure("2") do |config|
  config.vm.box = BOX_NAME

  config.vm.synced_folder ".", "/vagrant", type: "rsync",
    rsync__auto: false,
    rsync__exclude: [".git/", "dist-newstyle/", "dist/", ".stack-work/"]

  config.vm.provider "virtualbox" do |vb|
    vb.memory = 8192
    vb.cpus = 4
  end

  config.vm.provider "libvirt" do |lv|
    lv.memory = 8192
    lv.cpus = 4
  end

  config.vm.provision "shell", path: "vagrant/provision-openbsd.sh"
end
