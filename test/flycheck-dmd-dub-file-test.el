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
     (when (f-dir? root-sandbox-path)
       (f-delete root-sandbox-path :force))
     (f-mkdir root-sandbox-path)
     ,@body))

(defun equal-paths (path1 path2)
  "If PATH1 is the same as PATH2."
  (equal (directory-file-name (file-truename path1))
         (directory-file-name (file-truename path2))))

(ert-deftest test-fldd-get-project-dir ()
  "Tests that calling the real-life function with a DUB project sets the variable(s) correctly"
  (let ((sandbox-path (expand-file-name "sandbox" fldd-test-path)))
    (with-sandbox sandbox-path
      (f-touch (expand-file-name "dub.json" sandbox-path))
      (should (equal-paths (fldd--get-project-dir) sandbox-path)))

    (with-sandbox sandbox-path
      (f-touch (expand-file-name "package.json" sandbox-path))
      (should (equal-paths (fldd--get-project-dir) sandbox-path)))

    (with-sandbox sandbox-path
      (should (equal (fldd--get-project-dir) nil)))))


(ert-deftest test-fldd-set-include-path ()
  "Tests that calling the real-life function with a DUB project sets the variable(s) correctly"
  (let ((sandbox-path (expand-file-name "sandbox" fldd-test-path))
        (expected "~/.dub/packages/cerealed-master"))
    (with-sandbox sandbox-path
      (f-write-text "{
\"name\": \"test_project\",
\"targetType\": \"executable\",
\"dependencies\": { \"cerealed\": \"~master\" }
}" 'utf-8 (expand-file-name "dub.json" sandbox-path))
      (flycheck-dmd-dub-set-include-path)
      (should (equal (length flycheck-dmd-include-path) 1))
      (should (equal-paths (car flycheck-dmd-include-path) expected)))))
(provide 'flycheck-dmd-dub-file-test)

;;; flycheck-dmd-dub-file-test.el ends here
