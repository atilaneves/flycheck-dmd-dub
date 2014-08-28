;;; flycheck-dmd-dub-test.el --- Tests for flycheck-dmd-dub

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

(require 'f)

(defvar fldd-test-path (f-dirname load-file-name))
(defvar fldd-root-path (f-parent fldd-test-path))
(add-to-list 'load-path fldd-root-path)

(require 'ert)
(require 'flycheck-dmd-dub)

(provide 'flycheck-dmd-dub-test)
;;; flycheck-dmd-dub-test.el ends here
