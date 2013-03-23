;;; mode-icons.el --- Show icons for modes

;; Copyright (C) 2012  Tom Willemsen

;; Author: Tom Willemsen <thomas@aethon.nl>
;; Keywords: multimedia
;; Version: 0.1.0

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

(defconst mode-icons--load-file-name load-file-name
  "Where mode-icons was loaded from.")

(defun mode-icons-get-icon-file (name)
  (concat (file-name-directory mode-icons--load-file-name)
          "/icons/" name))

(defvar mode-icons
  `(("Emacs-Lisp" . (image :type xpm
                           :file ,(mode-icons-get-icon-file "emacs.xpm")
                           :ascent center))
    ("Python" . (image :type xpm
                       :file ,(mode-icons-get-icon-file "python.xpm")
                       :ascent center))
    ("Scheme" . (image :type xpm
                       :file ,(mode-icons-get-icon-file "scheme.xpm")
                       :ascent center))
    ("Lisp" . (image :type xpm
                     :file ,(mode-icons-get-icon-file "cl.xpm")
                     :ascent center))
    ("PHP" . (image :type xpm
                    :file ,(mode-icons-get-icon-file "php.xpm")
                    :ascent center))
    ("HTML" . (image :type xpm
                     :file ,(mode-icons-get-icon-file "html.xpm")
                     :ascent center))
    ("Org" . (image :type xpm
                    :file ,(mode-icons-get-icon-file "org.xpm")
                    :ascent center)))
  "Icons for major modes.")

(defun set-mode-icon (mode)
  (setq mode (format-mode-line mode))
  (let ((icon-spec (assoc mode mode-icons)))
    (when icon-spec
      (setq mode-name (propertize mode 'display (cdr icon-spec))))))

;;;###autoload
(defun set-current-mode-icon ()
  (set-mode-icon mode-name))

(provide 'mode-icons)
;;; mode-icons.el ends here
