{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let

  inherit (nixpkgs) pkgs ;

  devpkgs = import (builtins.fetchTarball https://github.com/rlupton20/alt-nixpkgs/archive/master.tar.gz) {};

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  treeThreads = devpkgs.haskellLibraries.treeThreads;
  concurrentStack = devpkgs.haskellLibraries.concurrentStack;
  haskellEtcd = devpkgs.haskellLibraries.haskellEtcd;
  klashnikov = haskellPackages.callPackage ./klashnikov.nix { tree-threads = treeThreads;
                                                          concurrent-stack = concurrentStack;
                                                          haskell-etcd = haskellEtcd; };

in

  if pkgs.lib.inNixShell then klashnikov.env else klashnikov
