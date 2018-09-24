---
  title: Getting started with nix
  author: Nagarjuna Pamu
---

Nix has been the current buzz word in the software development industry.
Lot of people are talking about Nix and also Nix has been very actively used in
the Haskell development for package management and also as an alternative to
Stack built tool. Another very important use of nix is via NixOps. NixOps is used
for automatic and convenient deployment of services on popular cloud service
providers.

When someone says "nix". It might mean a lot of things

- nix package manager
- nix expression language
- nix os
- nix ops

__What is nix?__

_Nix is a package manager that makes package management reliable and reproducible._

Nix does this by being functional. Nix describes a package using nix expression language.

Typically a nix package description looks like below psuedo code. Notice that package description contains source code snapshot and all required information to build the package.
This is the trick that nix uses for producing reproducible and reliable builds.

```bash
function <environment>:
  meta-data (contains name and version)
  source code snapshot
  built tools and compiler flags
```

Though above definition gives some idea about the nix. But Lets understand nix using an example. In this example lets create a nix package using nix expression language then, create a derivation from the nix expression and finally build the package.

to be continued ...
