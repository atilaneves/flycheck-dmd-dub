;;; flycheck-dmd-dub-file-test.el --- Tests for flycheck-dmd-dub

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

(defmacro with-sandbox (&rest body)
  "Evaluate BODY in an empty temporary directory."
  `(let* ((root-sandbox-path (expand-file-name "sandbox" fldd-test-path))
          (default-directory root-sandbox-path))
     (setq flycheck-dmd-include-path nil)
     (when (f-dir? root-sandbox-path)
       (f-delete root-sandbox-path :force))
     (f-mkdir root-sandbox-path)
     ,@body))

(defun equal-paths (path1 path2)
  "If PATH1 is the same as PATH2."
  (equal (directory-file-name (expand-file-name (file-truename path1)))
         (directory-file-name (expand-file-name (file-truename path2)))))

(defun write-json-file (name path)
  "Write the NAME dub file in PATH."
  (let ((path (expand-file-name name path)))
    (f-write-text "{
\"name\": \"test_project\",
\"targetType\": \"none\",
\"stringImportPaths\": [\"stringies\", \"otherstringies\"],
\"dependencies\": { \"cerealed\": \"~master\" }
}\n" 'utf-8 path))
  )

(defun write-sdl-file (name path)
  "Write the NAME dub file in PATH."
  (let ((path (expand-file-name name path)))
    (f-write-text "name \"test_project\"
targetType \"none\"
stringImportPaths \"stringies\" \"otherstringies\"
dependency \"cerealed\" version=\"~master\"

" 'utf-8 path))
  )

(defvar fldd--sandbox-path (expand-file-name "sandbox" fldd-test-path))

(ert-deftest test-fldd-set-include-path-package-json ()
  "Tests that calling the real-life function with a DUB project sets the variable(s) correctly"
  (with-sandbox fldd--sandbox-path
                (write-json-file "package.json" fldd--sandbox-path)
                (flycheck-dmd-dub-set-include-path)
                (should (not (equal flycheck-dmd-include-path nil)))
                (should (equal-paths (car flycheck-dmd-include-path) "~/.dub/packages/cerealed-master/src"))))

(ert-deftest test-fldd-set-include-path-dub-json ()
  "Tests that calling the real-life function with a DUB project sets the variable(s) correctly"
  (with-sandbox fldd--sandbox-path
                (write-json-file "dub.json" fldd--sandbox-path)
                (flycheck-dmd-dub-set-include-path)
                (should (equal-paths (car flycheck-dmd-include-path) "~/.dub/packages/cerealed-master/src"))))

(ert-deftest test-fldd-set-include-path-dub-sdl ()
  "Tests that calling the real-life function with a DUB project sets the variable(s) correctly"
  (with-sandbox fldd--sandbox-path
                (write-sdl-file "dub.sdl" fldd--sandbox-path)
                (flycheck-dmd-dub-set-include-path)
                (should (equal-paths (car flycheck-dmd-include-path) "~/.dub/packages/cerealed-master/src"))))


(ert-deftest test-fldd-set-include-path-wrong-file ()
  "Tests that calling the real-life function with a DUB project sets the variable(s) correctly"
  (with-sandbox fldd--sandbox-path
                (write-json-file "foo.json" fldd--sandbox-path)
                (flycheck-dmd-dub-set-include-path)
                (should (equal flycheck-dmd-include-path nil))))

;; (ert-deftest test-fldd-set-flags ()
;;   "Tests that calling the real-life function with a DUB project sets the flags correctly"
;;   (with-sandbox fldd--sandbox-path
;;                 (write-json-file "dub.json" fldd--sandbox-path)
;;                 (flycheck-dmd-dub-set-variables)
;;                 (should (equal (length flycheck-dmd-include-path) 1))
;;                 (should (equal-paths (car flycheck-dmd-include-path) "~/.dub/packages/cerealed-master"))
;;                 (should (equal (length flycheck-dmd-flags) 1))
;;                 (should (equal-paths (car flycheck-dmd-flags) (expand-file-name "stringies" fldd--sandbox-path)))
;;                 (should (equal-paths (car (cdr flycheck-dmd-flags)) (expand-file-name "otherstringies" fldd--sandbox-path)))))



(provide 'flycheck-dmd-dub-file-test)

;;; flycheck-dmd-dub-file-test.el ends here
