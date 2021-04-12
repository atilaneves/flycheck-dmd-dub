flycheck-dmd-dub
================

[![Actions Status](https://github.com/atilaneves/flycheck-dmd-dub/workflows/CI/badge.svg)](https://github.com/atilaneves/flycheck-dmd-dub/actions)
[![Coverage Status](https://coveralls.io/repos/github/atilaneves/flycheck-dmd-dub/badge.svg?branch=master)](https://coveralls.io/github/atilaneves/flycheck-dmd-dub?branch=master)
[![MELPA](https://melpa.org/packages/flycheck-dmd-dub-badge.svg)](https://melpa.org/#/flycheck-dmd-dub)


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
