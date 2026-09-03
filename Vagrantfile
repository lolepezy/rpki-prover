# -*- mode: ruby -*-
#
# Local OpenBSD VM for building/testing rpki-prover: debugging a build
# failure, checking rsync flag compatibility, running the daemon against
# real repositories, etc.
#
# Requires Vagrant plus a provider it can drive. The box below
# (DefinedNet/openbsd78) only publishes a libvirt build, so
# `vagrant up --provider=libvirt` is effectively required unless you swap
# in a different box that also has a VirtualBox build (the "virtualbox"
# provider block further down is harmless but unused until then). Neither
# Vagrant nor a hypervisor is installed by this repo.
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
# Box choice, and why it's NOT the obvious "generic/openbsd7": that box is
# pinned to OpenBSD 7.4 across every published version, and OpenBSD only
# keeps packages (and, it turns out, install/upgrade sets) live on its
# mirrors for the current + previous release. A fresh 7.4 box therefore has
# no installable packages at all ("no such dir"), and can't be fixed with
# an in-place `sysupgrade` either: the normal one-hop-at-a-time upgrade
# needs 7.5's sets, which are equally gone from the mirrors, and 7.4's
# `sysupgrade` binary predates the `-R <version>` flag that would let it
# target a later release directly. It's a genuine dead end, not just an
# inconvenience -- don't re-attempt it if some other box goes stale the
# same way; skip straight to finding/building a current one instead (see
# below).
#
# DefinedNet/openbsd78 is what's actually used below: real OpenBSD 7.8, a
# native libvirt build, no upgrade dance required. It's a much smaller/less
# established publisher than "generic", so if it goes stale or disappears,
# check https://portal.cloud.hashicorp.com/vagrant/discover for a
# replacement (needs a libvirt build, and to actually be a still-supported
# release -- check https://ftp.openbsd.org/pub/OpenBSD/ for which point
# releases currently have packages before trusting a box's version number).
#
# Override without editing this file:
#   RPKI_OPENBSD_BOX=some/other-box vagrant up
BOX_NAME = ENV.fetch("RPKI_OPENBSD_BOX", "DefinedNet/openbsd78")

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
