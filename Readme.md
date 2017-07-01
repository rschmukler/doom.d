## About

This is my emacs config as a plugin for the amazing [hlissner/.emacs.d](https://github.com/hlissner/.emacs.d).

## Installation

First ensure that you have installed doom:

```
git clone https://github.com/hlissner/.emacs.d ~/.emacs.d
cd ~/.emacs.d
cp init.example.el init.el  # maybe edit init.el
make install
```

Then add this module as a submodule:

```
git submodule add "https://github.com/rschmukler/doom-emacs-module.git" ./modules/private/$USER
```
