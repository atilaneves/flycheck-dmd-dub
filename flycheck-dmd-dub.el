;;; flycheck-dmd-dub --- elisp to read package.json from dub and add syntax highlighting that resolves dependencies
;;; Commentary:
;; Author: Atila Neves
;; URL: http://github.com/atilaneves/flycheck-dmd-dub

(provide 'flycheck-dmd-dub)

;;; Code:
(require 'json)


(defun dub-pkg-version-to-suffix(version)
  "From dub dependency to suffix for the package directory. Expects what
  follows the colon in a dub.json file such as '~master' or '>=1.2.3' and
  returns the suffix to compose the directory name with."
  (cond
   ((equal version "~master") "-master") ; e.g. "cerealed": "~master" -> cerealed-master
   ((equal (substring version 1 2) "=") (concat "-" (substring version 2))) ;>= or ==
   (t nil)))


(defun dub-pkgs-dir()
  "Returns the directory where dub stores packages"
  (if (eq system-type 'windows-nt)
      (concat (getenv "APPDATA") "\\dub\\packages\\")
    "~/.dub/packages/"))


(defun dub-pkg-to-dir-name(pkg)
  "Returns the directory name for a dub package dependency such as 'cerealed': '~master'"
  (let ((pkg-name (car pkg))
        (pkg-suffix (dub-pkg-version-to-suffix (cdr pkg))))
    (concat (dub-pkgs-dir) pkg-name pkg-suffix)))


(defun stringify-car(lst)
  "Transforms the car of the list into a string representation of its symbol"
  (cons (symbol-name (car lst)) (cdr lst)))


(defun get-dub-package-dirs(dub-json-file)
  (let* ((symbol-dependencies (cdr (assq 'dependencies (json-read-file dub-json-file))))
         (dependencies (mapcar 'stringify-car symbol-dependencies)))
    (delq nil (mapcar 'dub-pkg-to-dir-name dependencies))))


(defun get-project-dir()
  "Locates the project directory by searching up for either package.json or dub.json"
  (let ((package-json-dir (locate-dominating-file default-directory "dub.json"))
        (dub-json-dir (locate-dominating-file default-directory "package.json")))
    (or dub-json-dir package-json-dir)))


(defun get-jsonfile-name(basedir)
  "Returns the name of the json file to read given the base directory"
  (if (file-exists-p (concat basedir "dub.json"))
      (concat basedir "dub.json")
    (concat basedir "package.json")))


(add-hook 'd-mode-hook
  (lambda()
    (let* ((basedir (get-project-dir))
           (jsonfile (get-jsonfile-name  basedir)))
      (when basedir
        (setq flycheck-dmd-include-path (get-dub-package-dirs jsonfile))))))

;;; flycheck-dmd-dub ends here
