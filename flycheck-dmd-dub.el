;;; flycheck-dmd-dub.el --- Sets flycheck-dmd-include-paths from dub package information

;; Copyright (C) 2014 Atila Neves

;; Author:  Atila Neves <atila.neves@gmail.com>
;; Version: 0.4
;; Package-Requires ((flycheck "0.17") (ert "0"))
;; Keywords: languages
;; URL: http://github.com/atilaneves/flycheck-dmd-dub

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

;; This package uses "dub describe" to obtain a list of all
;; dependencies of a D dub project and sets the variable flycheck-dmd-include-paths
;; so that flycheck syntax checking knows how to call the compiler
;; and pass it include flags to find the dependencies

;; Usage:
;;
;;      (add-hook 'd-mode-hook 'flycheck-dmd-dub-set-include-path)

;;; Code:

(require 'json)
(require 'flycheck)
(require 'ert)


(defun fldd--dub-pkg-version-to-suffix (version)
  "From dub dependency to suffix for the package directory.
VERSION is what follows the colon in a dub.json file such as
'~master' or '>=1.2.3' and returns the suffix to compose the
directory name with."
  (cond
   ((equal version "~master") "-master") ; e.g. "cerealed": "~master" -> cerealed-master
   ((equal (substring version 1 2) "=") (concat "-" (substring version 2))) ;>= or ==
   (t nil)))

(ert-deftest test-fldd--dub-pkg-version-to-suffix ()
  "Test getting the suffix from the package version"
  (should (equal (fldd--dub-pkg-version-to-suffix "~master") "-master"))
  (should (equal (fldd--dub-pkg-version-to-suffix ">=1.2.3") "-1.2.3"))
  (should (equal (fldd--dub-pkg-version-to-suffix "==2.3.4") "-2.3.4")))


(defun fldd--dub-pkgs-dir ()
  "Return the directory where dub packages are found."
  (if (eq system-type 'windows-nt)
      (concat (getenv "APPDATA") "\\dub\\packages\\")
    "~/.dub/packages/"))


(defun fldd--dub-pkg-to-dir-name (pkg)
  "Return the directory name for a dub package dependency.
PKG is a package name such as 'cerealed': '~master'."
  (let ((pkg-name (car pkg))
        (pkg-suffix (fldd--dub-pkg-version-to-suffix (cdr pkg))))
    (concat (fldd--dub-pkgs-dir) pkg-name pkg-suffix)))

(ert-deftest test-fldd--dub-pkg-to-dir-name ()
  "Test that the directory name from a dub package dependency is correct."
  (if (not (eq system-type 'windows-nt))
      (progn
        (should (equal (fldd--dub-pkg-to-dir-name '("cerealed" . "~master")) "~/.dub/packages/cerealed-master"))
        (should (equal (fldd--dub-pkg-to-dir-name '("cerealed" . ">=3.4.5")) "~/.dub/packages/cerealed-3.4.5")))))


(defun fldd--pkg-to-dir-names (pkg)
  "Return a directory name for the assoc list PKG."
  (let ((import-paths (cdr (assq 'importPaths pkg)))
        (path (cdr (assq 'path pkg))))
    (mapcar (lambda (p) (expand-file-name p path)) import-paths)))

(ert-deftest test-fldd--pkg-to-dir-names ()
  "Test that a correct dir name is return for one package."
  (should (equal (fldd--pkg-to-dir-names '((importPaths . ["source"]) (path . "/usr/bin")))
                '("/usr/bin/source")))
  (should (equal (fldd--pkg-to-dir-names '((importPaths . ["."]) (path . "/usr/bin")))
                 '("/usr/bin")))
  (should (equal (fldd--pkg-to-dir-names '((importPaths . ["." "source"]) (path . "/foo/bar")))
                 '("/foo/bar" "/foo/bar/source"))))


(defun fldd--flatten(x)
  (cond ((null x) nil)
        ((listp x) (append (fldd--flatten (car x)) (fldd--flatten (cdr x))))
        (t (list x))))


(defun fldd--pkgs-to-dir-names (pkgs)
  "Return a list of dir names for assoc list PKGS."
  ;car of cdr since taking the cdr creates a list with the vector
  (fldd--flatten (mapcar #'fldd--pkg-to-dir-names (cdr pkgs))))

(ert-deftest test-fldd--pkgs-to-dir-names ()
  "Test that getting all directories for all packages works."
  (should (equal (fldd--pkgs-to-dir-names
                  '(packages . [((importPaths . ["src" "tests"]) (path . "/foo/bar"))
                               ((importPaths . ["lefoo"]) (path . "/usr/bin"))]))
                 '("/foo/bar/src" "/foo/bar/tests" "/usr/bin/lefoo"))))


(defun fldd--get-dub-package-dirs-json (json)
  "Return the directories where the packages are for this JSON assoclist."
  (let ((packages (assq 'packages json)))
    (fldd--pkgs-to-dir-names packages)))

(ert-deftest test-fldd--get-dub-package-dirs-json ()
  "Test getting the package directories from a json object"
  (should (equal (fldd--get-dub-package-dirs-json (json-read-from-string "{}")) nil))
  (should (equal (fldd--get-dub-package-dirs-json (json-read-from-string "{\"packages\": []}")) nil))
  (should (equal (fldd--get-dub-package-dirs-json
                  (json-read-from-string
                   "{\"packages\": [{ \"path\": \"/foo/bar\", \"importPaths\": [\".\"]}] } "))
                 '("/foo/bar")))
    (should (equal (fldd--get-dub-package-dirs-json
                  (json-read-from-string
                   "{\"packages\": [
                        { \"path\": \"/foo/bar/source\", \"importPaths\": [\".\"]},
                        { \"path\": \"/blug/dlag/\", \"importPaths\": [\"source\"]}
                   ]}"))
                 '("/foo/bar/source" "/blug/dlag/source")))
)


(defun fldd--get-project-dir ()
  "Locates the project directory by searching up for either package.json or dub.json."
  (let ((package-json-dir (locate-dominating-file default-directory "dub.json"))
        (dub-json-dir (locate-dominating-file default-directory "package.json")))
    (or dub-json-dir package-json-dir)))


(defun fldd--get-dub-package-dirs ()
  "Get package directories."
  (let ((default-directory (fldd--get-project-dir)))
    (fldd--get-dub-package-dirs-json (json-read-from-string (shell-command-to-string "dub describe")))))


;;;###autoload
(defun flycheck-dmd-dub-set-include-path ()
  "Set `flycheck-dmd-include-path' from dub info if available."
  (let* ((basedir (fldd--get-project-dir)))
      (when basedir
        (setq flycheck-dmd-include-path (fldd--get-dub-package-dirs)))))


(provide 'flycheck-dmd-dub)
;;; flycheck-dmd-dub ends here
