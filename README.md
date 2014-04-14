flycheck-dmd-dub
================

Emacs lisp to read dependency information from dub and add syntax
highlighting via flycheck that resolves dependencies.

Usage: `(add-hook 'd-mode-hook 'flycheck-dmd-dub-set-include-path)`

I've had problems with the most recent version (v0.2) after
installing. Deleting the flycheck-dmd-dub.elc file seems to fix it,
but I don't know why.
