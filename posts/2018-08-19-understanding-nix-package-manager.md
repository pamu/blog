---
  title: Understanding nix package manager
  author: Nagarjuna Pamu
---

### After reading this blog post, you will understand

1. Why Nix? What benefits does it give developers

2. How nix package manager works?

3. How can we use nix for Haskell development and be productive?


### Resources

- Blog posts

  - [Ocharles blog](https://ocharles.org.uk/blog/posts/2014-02-04-how-i-develop-with-nixos.html)

  - [Most popular nix tutorial](https://github.com/Gabriel439/haskell-nix)

- Talks

  - [Setting up a simple CI server using Haskell and Nix (BAHUG talk by Gabriel Gonzalez)](https://www.youtube.com/watch?v=NQJVNvxgDqg)


### What is NIX?

Nix is a package management system built around its own custom purely functional expression language called as nix expression language.

Nix does not support windows

nixpkgs (nix package database)

100% of packages on hackage are available on nix haskell pkgs

nix pkgs support mutiple versions of GHC and also GHCJS

Hydra (used to download pre-built binaries of haskell packages)

NixOS (Linux distribution built from ground up using nix)

Nix Ops can be used to deploy NixOS servers (deployment to azure, ec2, gce and also into virtual box instances is supported)

Nix achieves reproducibility of builts using purely functional approach
And also Atomic updates

- Reproducibility
- Atomicity
- Isolation
- Rollback

looks like ACID transactions story 

Nix

Five simple types
- string
- path
- integer
- boolean 
- null


${...} (... can be string | path | derivation)