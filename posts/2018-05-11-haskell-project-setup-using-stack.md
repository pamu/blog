---
   title: Haskell project setup using stack.
   author: Nagarjuna Pamu 
---

### Requirements
- Stack
    - [Here](https://docs.haskellstack.org/en/stable/README/) are instructions to install stack.

### Setting up new Haskell project using Stack

```bash
    stack new <project name>
```
Lets name the project `haskworks`. So the command to setup the haskell project called `haskworks` becomes

```bash
    stack new haskworks
```

Executing the above project generates a folder named `haskworks` which contains source code and build files.

### Haskell project structure

```bash
[haskworks] tree                                                               
.
├── ChangeLog.md
├── LICENSE
├── README.md
├── Setup.hs
├── app
│   └── Main.hs
├── haskworks.cabal
├── package.yaml
├── src
│   └── Lib.hs
├── stack.yaml
└── test
    └── Spec.hs

3 directories, 10 files
```

_Here are the important files and folders to note_

- Folders
    1. app
        - Code for executable (contains `Main.hs`)
    2. src
        - Library code. Exposes modules as a part of library. Generally it is a good practice to separate code into executable code and library code. Library code can be separated for re-use in another project 
    3. test
        - Folder for all tests contains `main` function for indicating the starting point of the tests. 

- Files
    1. package.yaml
    2. haskworks.cabal
        - As package.yaml is present haskworks.cabal file is auto generated from the package.yaml file contents.
        - When stack build or install is launched .cabal file is auto generated from the package.yaml
        - `package.yaml` file exists for convinience. Only cabal file with `package.yaml` file can also be used for building the haskell project.
        - Hpack (which stack uses internally) interprets `package.yaml` file and generates `haskworks.cabal` file
        - Here are the important points why Hpack configuration `package.yaml` is used
            - The guiding design principles for Hpack are:
                - Don't require the user to state the obvious, make sensible assumptions by default
                - Give the user 100% control when needed
                - Don't require the user to repeat things, facilitate DRYness
    3. stack.yaml
        - `haskworks.cabal` is the real configuration file which is required for
            - specifying library dependencies and their versions.
                - Note that every library has a version.
                - Some library version combinations may not be compatible with each other.
                - For example, consider a fictitious case where Pandoc-0.0.1 may not be compatible with text-0.0.2.
                - So given Pandoc-0.0.1, there will be version(s) for text which will be compatible
                - Also note that this pandoc compatible text may not be compatible with another library, lets say, bytestring. So now, use text whic is compatible with both pandoc and bytestring.
                - The above case becomes like the constraints solver case.
                - Stack (which interprets stack.yaml) solves this problem with help of resolver.
                - Resolver is a repository which contains are all compatible library versions. So that user can safely mention the library names and ignoring the versions.
                - Just mention the resolver which you are interested in and all right compatible versions of library are used automatically by stack.
                - Stack also manages multiple projects but, cabal does not have that capability.

### haskworks.cabal and package.yaml files 


```yaml
name:                haskworks
version:             0.1.0.0
github:              "githubuser/haskworks"
license:             BSD3
author:              "Author name here"
maintainer:          "example@example.com"
copyright:           "2018 Author name here"

extra-source-files:
- README.md
- ChangeLog.md

# Metadata used when publishing your package
# synopsis:            Short description of your package
# category:            Web

# To avoid duplicated efforts in documentation and dealing with the
# complications of embedding Haddock markup inside cabal files, it is
# common to point users to the README.md file.
description:         Please see the README on GitHub at <https://github.com/githubuser/haskworks#readme>

dependencies:
- base >= 4.7 && < 5

library:
  source-dirs: src

executables:
  haskworks-exe:
    main:                Main.hs
    source-dirs:         app
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - haskworks

tests:
  haskworks-test:
    main:                Spec.hs
    source-dirs:         test
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - haskworks
```

### Stack.yaml

```yaml
resolver: lts-11.8

packages:
- .

```

### building, running, installing and testing Haskell code

```bash
    stack build
```

```bash
    stack exec <executable_name>
```

```bash
    stack install
```

moves to the executable binary to `~/.local/bin`

```bash
    stack test
```


### Rapid compiler feeback mode


```bash
    stack build --fast --file-watch
```

