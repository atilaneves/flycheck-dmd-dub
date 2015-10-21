flycheck-dmd-dub
================

[![Build Status](https://travis-ci.org/atilaneves/flycheck-dmd-dub.svg?branch=master)](https://travis-ci.org/atilaneves/flycheck-dmd-dub)

Emacs lisp to read dependency information from [dub](https://github.com/D-Programming-Language/dub)
and add syntax highlighting via flycheck that resolves package dependencies.

Basically tells emacs/flycheck how to call dmd with the right `-I` and `-J` flags.

Usage
-----

Add this to your `.emacs` / `init.el`:

`(add-hook 'd-mode-hook 'flycheck-dmd-dub-set-variables)`


Installation
------------

Install from [MELPA](https://melpa.org) or [MELPA Stable](https://stable.melpa.org/) with:

    M-x package-install RET flycheck-dmd-dub.
