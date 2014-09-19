flycheck-dmd-dub
================

[![Build Status](https://travis-ci.org/atilaneves/flycheck-dmd-dub.svg?branch=master)](https://travis-ci.org/atilaneves/flycheck-dmd-dub)

Emacs lisp to read dependency information from [dub](https://github.com/D-Programming-Language/dub)
and add syntax highlighting via flycheck that resolves package dependencies.

Usage
-----

Add this to your `.emacs` / `init.el`:

`(add-hook 'd-mode-hook 'flycheck-dmd-dub-set-include-path)`


Installation
------------

Install from [MELPA](http://melpa.milkbox.net) or [MELPA Stable](http://melpa-stable.milkbox.net/) with:

    M-x package-install RET flycheck-dmd-dub.
