;;; smart-align.el --- Smart align current block

;; Filename: smart-align.el
;; Description: Smart align current block
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2019, Andy Stewart, all rights reserved.
;; Created: 2019-06-23 22:25:05
;; Version: 0.1
;; Last-Updated: 2019-06-23 22:25:05
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/smart-align.el
;; Keywords:
;; Compatibility: GNU Emacs 26.1.92
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Smart align current block.
;;
;; In normal way, we need do below steps when align block:
;; 1. Select the region.
;; 2. Type M-x align-regexp RET
;; 3. Type = and hit enter.
;;
;; I hate select region by hand,
;; too many cursor movement operations are too inefficient.
;;
;; So i develop this plugin that you only need do `smart-align' in cursor.
;; This plugin will found block bound around cursor,
;; then call `align-regexp' with = regexp.
;;

;;; Installation:
;;
;; Put smart-align.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'smart-align)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET smart-align RET
;;

;;; Change log:
;;
;; 2019/06/23
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require


;;; Code:

(defun smart-align ()
  (interactive)
  (with-demoted-errors
      "Something wrong when align."
    (let ((align-start
           (save-excursion
             (backward-up-list)
             (point)
             ))
          (align-end
           (save-excursion
             (up-list)
             (point))))
      (align-regexp align-start align-end "\\(\\s-*\\)\\(=\\|:\\)" 1 1))))

(provide 'smart-align)

;;; smart-align.el ends here
