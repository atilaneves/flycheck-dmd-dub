;;; flycheck-dmd-dub.el --- Automatically sets flycheck-dmd-include-paths from dub package information

;; Copyright (C) 2014 Atila Neves

;; Author:  Atila Neves <atila.neves@gmail.com>
;; Version: 0.1
;; Package-Requires ((flycheck "0.17"))
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package reads the dub package file, either dub.json or package.json,
;; and automatically sets flycheck-dmd-include-paths so that flycheck
;; syntax checking knows to include the dependent packages.
;; URL: http://github.com/atilaneves/flycheck-dmd-dub

;;; Code:

(require 'json)


(defun fldd--dub-pkg-version-to-suffix(version)
  "From dub dependency to suffix for the package directory. Expects what
  follows the colon in a dub.json file such as '~master' or '>=1.2.3' and
  returns the suffix to compose the directory name with."
  (cond
   ((equal version "~master") "-master") ; e.g. "cerealed": "~master" -> cerealed-master
   ((equal (substring version 1 2) "=") (concat "-" (substring version 2))) ;>= or ==
   (t nil)))


(defun fldd--dub-pkgs-dir()
  "Returns the directory where dub stores packages"
  (if (eq system-type 'windows-nt)
      (concat (getenv "APPDATA") "\\dub\\packages\\")
    "~/.dub/packages/"))


(defun fldd--dub-pkg-to-dir-name(pkg)
  "Returns the directory name for a dub package dependency such as 'cerealed': '~master'"
  (let ((pkg-name (car pkg))
        (pkg-suffix (fldd--dub-pkg-version-to-suffix (cdr pkg))))
    (concat (fldd--dub-pkgs-dir) pkg-name pkg-suffix)))


(defun fldd--stringify-car(lst)
  "Transforms the car of the list into a string representation of its symbol"
  (cons (symbol-name (car lst)) (cdr lst)))


(defun fldd--get-dub-package-dirs(dub-json-file)
  (let* ((symbol-dependencies (cdr (assq 'dependencies (json-read-file dub-json-file))))
         (dependencies (mapcar 'fldd--stringify-car symbol-dependencies)))
    (delq nil (mapcar 'fldd--dub-pkg-to-dir-name dependencies))))


(defun fldd--get-project-dir()
  "Locates the project directory by searching up for either package.json or dub.json"
  (let ((package-json-dir (locate-dominating-file default-directory "dub.json"))
        (dub-json-dir (locate-dominating-file default-directory "package.json")))
    (or dub-json-dir package-json-dir)))


(defun fldd--get-jsonfile-name(basedir)
  "Returns the name of the json file to read given the base directory"
  (if (file-exists-p (concat basedir "dub.json"))
      (concat basedir "dub.json")
    (concat basedir "package.json")))


(add-hook 'd-mode-hook
  (lambda()
    (let* ((basedir (fldd--get-project-dir))
           (jsonfile (fldd--get-jsonfile-name basedir)))
      (when basedir
        (setq flycheck-dmd-include-path (fldd--get-dub-package-dirs jsonfile))))))

(provide 'flycheck-dmd-dub)
;;; flycheck-dmd-dub ends here
