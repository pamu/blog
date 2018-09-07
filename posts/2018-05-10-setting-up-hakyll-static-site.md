---
title : Setting up hakyll static site
author: Nagarjuna Pamu
---

### Requirements

1. Git
2. Stack

### Setup instructions on Ubuntu.

1.  Git

    ```bash
      sudo apt-get install git
    ```

2. Stack

    ```bash
      wget -qO- https://get.haskellstack.org/ | sh
    ```

### Ensure `~/.local/bin` is on path

```bash
  export PATH=$PATH:~/.local/bin
```

### Stack installed?

```bash
  [blog] stack --version                                            master  ✗ ✭ ✱
  Version 1.6.5 x86_64 hpack-0.20.0
```


### Git clone hakyll template

[Here](https://github.com/pamu/blog) is the template to clone

```bash
  git clone git@github.com:pamu/blog.git
  cd blog
  stack install  
```

### Launch blog

```bash
  site watch
```

Now open `http://localhost:8000` on your browser to see the static site.
Add markdown files to posts folder of the blog and see the posts added on the blog.
For example:  In the `blog` folder, add a sample post

```
  cd posts
  touch 2018-05-10-sample.md
```

### Building static site

```bash
  site clean
  site build
```

Static site will be generated in the `docs` folder

### Use Github.com to deploy the static site

More info available [here](https://pages.github.com/)

1. Create a repo `username.github.io` on github
2. Push docs folder contents to the repo
3. See the site on `username.github.io`
