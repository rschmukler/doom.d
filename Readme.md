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

Then install the module

```
git clone https://github.com/rschmukler/doom-emacs-module.git
cd doom-emacs-module
ln -s $PWD ~/.emacs.d/modules/private/$USER
```
