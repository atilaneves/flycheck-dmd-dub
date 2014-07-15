;;; flycheck-dmd-dub-tests.el --- Tests for flycheck-dmd-dub

;; Copyright (C) 2014  Atila Neves

;; Author:  <aalvesne@atilacisco3>
;; Keywords:

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

;;

;;; Code:

; make sure all packages are found (flycheck-dmd-dub and flycheck)
(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))
(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'ert)
(require 'flycheck-dmd-dub)


(ert-deftest test-fldd--dub-pkg-version-to-suffix ()
  "Test getting the suffix from the package version"
  (should (equal (fldd--dub-pkg-version-to-suffix "~master") "-master"))
  (should (equal (fldd--dub-pkg-version-to-suffix ">=1.2.3") "-1.2.3"))
  (should (equal (fldd--dub-pkg-version-to-suffix "==2.3.4") "-2.3.4")))


(ert-deftest test-fldd--dub-pkg-to-dir-name ()
  "Test that the directory name from a dub package dependency is correct."
  (if (not (eq system-type 'windows-nt))
      (progn
        (should (equal (fldd--dub-pkg-to-dir-name '("cerealed" . "~master")) "~/.dub/packages/cerealed-master"))
        (should (equal (fldd--dub-pkg-to-dir-name '("cerealed" . ">=3.4.5")) "~/.dub/packages/cerealed-3.4.5")))))


(ert-deftest test-fldd--pkg-to-dir-names ()
  "Test that a correct dir name is return for one package."
  (should (equal (fldd--pkg-to-dir-names '((importPaths . ["source"]) (path . "/usr/bin")))
                '("/usr/bin/source")))
  (should (equal (fldd--pkg-to-dir-names '((importPaths . ["."]) (path . "/usr/bin")))
                 '("/usr/bin")))
  (should (equal (fldd--pkg-to-dir-names '((importPaths . ["." "source"]) (path . "/foo/bar")))
                 '("/foo/bar" "/foo/bar/source"))))


(ert-deftest test-fldd--pkgs-to-dir-names ()
  "Test that getting all directories for all packages works."
  (should (equal (fldd--pkgs-to-dir-names
                  '(packages . [((importPaths . ["src" "tests"]) (path . "/foo/bar"))
                               ((importPaths . ["lefoo"]) (path . "/usr/bin"))]))
                 '("/foo/bar/src" "/foo/bar/tests" "/usr/bin/lefoo"))))


(ert-deftest test-fldd--get-dub-package-dirs-json ()
  "Test getting the package directories from a json string."
  (should (equal (fldd--get-dub-package-dirs-json "{}") nil))
  (should (equal (fldd--get-dub-package-dirs-json "{\"packages\": []}") nil))
  (should (equal (fldd--get-dub-package-dirs-json
                  "{\"packages\": [{ \"path\": \"/foo/bar\", \"importPaths\": [\".\"]}] } ")
                 '("/foo/bar")))
  (should (equal (fldd--get-dub-package-dirs-json
                  "{\"packages\": [
                        { \"path\": \"/foo/bar/source\", \"importPaths\": [\".\"]},
                        { \"path\": \"/blug/dlag/\", \"importPaths\": [\"source\"]}
                   ]}")
                 '("/foo/bar/source" "/blug/dlag/source")))
  (should (equal (fldd--get-dub-package-dirs-json
                  "The following changes will be performed:\nFetch vibe-d >=0.7.17, userWide\n{}") nil))
  (should (equal (fldd--get-dub-package-dirs-json
                  "The following changes will be performed:\nFetch vibe-d >=0.7.17, userWide
{\"packages\": [{ \"path\": \"/foo/bar\", \"importPaths\": [\".\"]}] } ")
                 '("/foo/bar")))
)

(provide 'flycheck-dmd-dub-test)
;;; flycheck-dmd-dub-test.el ends here
