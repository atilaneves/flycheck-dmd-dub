;;; flycheck-dmd-dub-test-pure.el --- Tests for flycheck-dmd-dub

;; Copyright (C) 2014  Atila Neves

;; Author:  <atila.neves@gmail.com>
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
  "Test that a correct dir name is returned for one package."
  (should (equal (fldd--pkg-to-dir-names '((importPaths . ["source"]) (path . "/usr/bin")))
                '("/usr/bin/source")))
  (should (equal (fldd--pkg-to-dir-names '((importPaths . ["."]) (path . "/usr/bin")))
                 '("/usr/bin")))
  (should (equal (fldd--pkg-to-dir-names '((importPaths . ["." "source"]) (path . "/foo/bar")))
                 '("/foo/bar" "/foo/bar/source"))))

(ert-deftest test-fldd--pkg-to-string-import-paths ()
  "Test that a correct string import path is returned for one package."
  (should (equal (fldd--pkg-to-string-import-paths '((stringImportPaths . ["source"]) (path . "/usr/bin")))
                 '("/usr/bin/source")))
  (should (equal (fldd--pkg-to-string-import-paths '((stringImportPaths . ["."]) (path . "/usr/bin")))
                 '("/usr/bin")))
  (should (equal (fldd--pkg-to-string-import-paths '((stringImportPaths . ["." "source"]) (path . "/foo/bar")))
                 '("/foo/bar" "/foo/bar/source"))))


(ert-deftest test-fldd--pkgs-to-dir-names ()
  "Test that getting all directories for all packages works."
  (should (equal (fldd--pkgs-to-dir-names
                  '(packages . [((importPaths . ["src" "tests"]) (path . "/foo/bar"))
                               ((importPaths . ["lefoo"]) (path . "/usr/bin"))]))
                 '("/foo/bar/src" "/foo/bar/tests" "/usr/bin/lefoo"))))

(ert-deftest test-fldd--pkgs-to-string-imports ()
  "Test that getting all string import directories for all packages works."
  (should (equal (fldd--pkgs-to-string-import-paths
                  '(packages . [((stringImportPaths . ["src" "tests"]) (path . "/foo/bar"))
                                ((stringImportPaths . ["lefoo"]) (path . "/usr/bin"))]))
                 '("/foo/bar/src" "/foo/bar/tests" "/usr/bin/lefoo"))))


(ert-deftest test-fldd--get-dub-package-dirs-output ()
  "Test getting the package directories from a json string."
  (should (equal (fldd--get-dub-package-dirs-output "{}") nil))
  (should (equal (fldd--get-dub-package-dirs-output "{\"packages\": []}") nil))
  (should (equal (fldd--get-dub-package-dirs-output
                  "{\"packages\": [{ \"path\": \"/foo/bar\", \"importPaths\": [\".\"]}] } ")
                 '("/foo/bar")))
  (should (equal (fldd--get-dub-package-dirs-output
                  "{\"packages\": [
                        { \"path\": \"/foo/bar/source\", \"importPaths\": [\".\"]},
                        { \"path\": \"/blug/dlag/\", \"importPaths\": [\"source\"]}
                   ]}")
                 '("/foo/bar/source" "/blug/dlag/source")))
  (should (equal (fldd--get-dub-package-dirs-output
                  "The following changes will be performed:\nFetch vibe-d >=0.7.17, userWide\n{}") nil))
  (should (equal (fldd--get-dub-package-dirs-output
                  "The following changes will be performed:\nFetch vibe-d >=0.7.17, userWide
{\"packages\": [{ \"path\": \"/foo/bar\", \"importPaths\": [\".\"]}] } ")
                 '("/foo/bar")))
  (should (equal (fldd--get-dub-package-dirs-output
                  "Invalid source/import path: /foo/bar/path
                  {\"packages\": [
                        { \"path\": \"/foo/bar/source\", \"importPaths\": [\".\"]},
                        { \"path\": \"/blug/dlag/\", \"importPaths\": [\"source\"]}
                   ]}")
                 '("/foo/bar/source" "/blug/dlag/source")))
  )

(ert-deftest test-fldd--get-dub-package-string-imports-output ()
  "Test getting the package directories from a json string."
  (should (equal (fldd--get-dub-package-string-import-paths-output "{}") nil))
  (should (equal (fldd--get-dub-package-string-import-paths-output "{\"packages\": []}") nil))
  (should (equal (fldd--get-dub-package-string-import-paths-output
                  "{\"packages\": [{ \"path\": \"/foo/bar\", \"stringImportPaths\": [\".\"]}] } ")
                 '("/foo/bar")))
  (should (equal (fldd--get-dub-package-string-import-paths-output
                  "{\"packages\": [
                        { \"path\": \"/foo/bar/source\", \"stringImportPaths\": [\".\"]},
                        { \"path\": \"/blug/dlag/\", \"stringImportPaths\": [\"source\"]}
                   ]}")
                 '("/foo/bar/source" "/blug/dlag/source")))
  (should (equal (fldd--get-dub-package-string-import-paths-output
                  "The following changes will be performed:\nFetch vibe-d >=0.7.17, userWide\n{}") nil))
  (should (equal (fldd--get-dub-package-string-import-paths-output
                  "The following changes will be performed:\nFetch vibe-d >=0.7.17, userWide
{\"packages\": [{ \"path\": \"/foo/bar\", \"stringImportPaths\": [\".\"]}] } ")
                 '("/foo/bar")))
  (should (equal (fldd--get-dub-package-string-import-paths-output
                  "Invalid source/import path: /foo/bar/path
                  {\"packages\": [
                        { \"path\": \"/foo/bar/source\", \"stringImportPaths\": [\".\"]},
                        { \"path\": \"/blug/dlag/\", \"stringImportPaths\": [\"source\"]}
                   ]}")
                 '("/foo/bar/source" "/blug/dlag/source")))
  )


(provide 'flycheck-dmd-dub-test-pure)
;;; flycheck-dmd-dub-test-pure.el ends here
