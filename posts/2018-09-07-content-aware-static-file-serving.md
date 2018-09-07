---
  title: Content aware static file serving
  author: Nagarjuna Pamu
---

### Watchman

This blog post is about `watchman`. The code for the project is at
[here](https://github.com/pamu/watchman).

Watchman is capable of

1. Serving static content similar to `python -m SimpleHTTPServer 8000`

2. Reload and automatically when content changes.

Content aware re-serving/reloading is convenient when you are developing
web/html based project.



### How to use watchman

Launch `watchman watch` in the directory which you want to serve over http.
Content changes are detected and auto served. No need to reload.

If you want only serving the static content and are not interested in the
watching the changes to the content. Then use `watchman server`. The contents
of the directory in which the command is executed is served.


### How to build from source

Watchman is a Haskell stack project. Checkout code from
_[https://github.com/pamu/watchman](https://github.com/pamu/watchman)_


#### Install stack

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

    or

```bash
wget -qO- https://get.haskellstack.org/ | sh
```

#### Download the code or use git to clone code repo

```bash
git clone https://github.com/pamu/watchman.git
```

    or

```bash
git clone git@github.com:pamu/watchman.git
```

#### Go to project folder

```bash
cd watchman
```

#### Stack build


```bash
stack build
```

#### Stack install

in the same project root folder (inside watchman directory)

```bash
stack install
```

The above command will create a executable called `watchman` and put the executable in
`.local/bin` folder in your home directory.

#### Export PATH

```bash
export PATH="~/.local/bin/:$PATH"
```

Now you can access `watchman` from you command line.
