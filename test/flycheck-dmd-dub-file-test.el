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
  (equal (directory-file-name (file-truename path1))
         (directory-file-name (file-truename path2))))

(defun write-dub-file (name path)
  "Write the NAME dub file in PATH."
  (f-write-text "{
\"name\": \"test_project\",
\"targetType\": \"executable\",
\"dependencies\": { \"cerealed\": \"~master\" }
}" 'utf-8 (expand-file-name name path)))


(ert-deftest test-fldd-set-include-path-package-json ()
  "Tests that calling the real-life function with a DUB project sets the variable(s) correctly"
  (let ((sandbox-path (expand-file-name "sandbox" fldd-test-path))
        (expected "~/.dub/packages/cerealed-master"))
    (with-sandbox sandbox-path
      (write-dub-file "package.json" sandbox-path)
      (flycheck-dmd-dub-set-include-path)
      (should (equal (length flycheck-dmd-include-path) 1))
      (should (equal-paths (car flycheck-dmd-include-path) expected)))))

(ert-deftest test-fldd-set-include-path-dub-json ()
  "Tests that calling the real-life function with a DUB project sets the variable(s) correctly"
  (let ((sandbox-path (expand-file-name "sandbox" fldd-test-path))
        (expected "~/.dub/packages/cerealed-master"))
    (with-sandbox sandbox-path
      (write-dub-file "dub.json" sandbox-path)
      (flycheck-dmd-dub-set-include-path)
      (should (equal (length flycheck-dmd-include-path) 1))
      (should (equal-paths (car flycheck-dmd-include-path) expected)))))

(ert-deftest test-fldd-set-include-path-wrong-file ()
  "Tests that calling the real-life function with a DUB project sets the variable(s) correctly"
  (let ((sandbox-path (expand-file-name "sandbox" fldd-test-path)))
    (with-sandbox sandbox-path
      (write-dub-file "foo.json" sandbox-path)
      (flycheck-dmd-dub-set-include-path)
      (should (equal flycheck-dmd-include-path nil)))))



(provide 'flycheck-dmd-dub-file-test)

;;; flycheck-dmd-dub-file-test.el ends here
