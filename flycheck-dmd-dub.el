;;; flycheck-dmd-dub --- elisp to read package.json from dub and add syntax highlighting that resolves dependencies
;;; Commentary:

(provide 'flycheck-dmd-dub)

;;; Code:
(require 'json)


(defun dub-dep-to-suffix(dep)
  (cond
   ((equal dep "~master") "-master")
   ((equal (substring dep 1 2) "=")
    (concat "-" (substring dep 2)))
   (t nil)))

(defun get-dub-package-dirs(dub-json-file)
  (let ((deps (cdr (assq 'dependencies (json-read-file dub-json-file)))))
    (delq nil (mapcar (lambda(module) (concat "~/.dub/packages/" (car module) (dub-dep-to-suffix (cdr module))))
          (mapcar (lambda(x) (cons (symbol-name (car x)) (cdr x))) deps)))))

(add-hook 'd-mode-hook
  (lambda()
    (let* ((basedir (locate-dominating-file default-directory "package.json"))
           (jsonfile (concat basedir "package.json")))
      (when basedir
        (setq flycheck-dmd-include-path (get-dub-package-dirs jsonfile))))))

;;; flycheck-dmd-dub ends here
