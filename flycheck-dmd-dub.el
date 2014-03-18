;;; flycheck-dmd-dub --- elisp to read package.json from dub and add syntax highlighting that resolves dependencies
;;; Commentary:
;; Author: Atila Neves
;; URL: http://github.com/atilaneves/flycheck-dmd-dub

(provide 'flycheck-dmd-dub)

;;; Code:
(require 'json)


(defun dub-dep-to-suffix(dep)
  "From dub dependency to suffix for the package directory. Expects what
  follows the colon in a dub.json file such as '~master' or '>=1.2.3' and
  returns the suffix to compose the directory name with."
  (cond
   ((equal dep "~master") "-master") ; e.g. "cerealed": "~master" -> cerealed-master
   ((equal (substring dep 1 2) "=") (concat "-" (substring dep 2))) ;>= or ==
   (t nil)))

(defun get-dub-package-dirs(dub-json-file)
  (let ((dependencies (cdr (assq 'dependencies (json-read-file dub-json-file)))))
    (delq nil (mapcar (lambda(module) (concat "~/.dub/packages/" (car module) (dub-dep-to-suffix (cdr module))))
          (mapcar (lambda(x) (cons (symbol-name (car x)) (cdr x))) dependencies)))))

(add-hook 'd-mode-hook
  (lambda()
    (let* ((basedir (locate-dominating-file default-directory "package.json"))
           (jsonfile (concat basedir "package.json")))
      (when basedir
        (setq flycheck-dmd-include-path (get-dub-package-dirs jsonfile))))))

;;; flycheck-dmd-dub ends here
