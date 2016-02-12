;;; mode-icons.el --- Show icons for modes -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Tom Willemse

;; Author: Tom Willemse <tom@ryuslash.org>
;; Keywords: multimedia
;; Version: 0.2.1
;; URL: http://ryuslash.org/projects/mode-icons.html
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

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

;; This package provides a globalized minor mode that replaces the
;; major mode name in your mode-line and places like Ibuffer with an
;; icon.  Currently the following programming modes are supported,
;; along with some other modes:
;;
;; - CSS
;; - Coffee
;; - Emacs-Lisp
;; - HTML
;; - Haml
;; - JavaScript
;; - Lisp
;; - nXML
;; - PHP
;; - Python
;; - Ruby
;; - Sass/Scss
;; - Scheme
;; - Shell-script
;; - Slim
;; - Snippet
;; - Web
;; - Yaml
;;
;; To enable this minor mode add the following line to your init file:
;;
;;     (mode-icons-mode)

;;; Code:

(require 'cl-lib)

(defgroup mode-icons nil
  "Provide icons for major modes."
  :group 'editing-basics
  :group 'convenience)

(defconst mode-icons--directory
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory)
  "Where mode-icons was loaded from.")

(defun mode-icons-get-icon-file (icon)
  "Get the location of ICON.

ICON should be a file name with extension.  The result is the
absolute path to ICON."
  (concat mode-icons--directory "/icons/" icon))

(defmacro mode-icons-define-font (font)
  "Define FONT for `mode-icons'."
  `(progn
     (defvar ,(intern (format "mode-icons-font-spec-%s" font))
       (font-spec :name ,(format "%s" font)))
     (defvar ,(intern (format "mode-icons-font-%s" font))
       (find-font ,(intern (format "mode-icons-font-spec-%s" font))))))

(mode-icons-define-font "github-octicons")
(mode-icons-define-font "font-mfizz")
(mode-icons-define-font "FontAwesome")

(defcustom mode-icons
  `(("CSS" "css" xpm)
    ("Coffee" "coffee" xpm)
    ("Compilation" "compile" xpm)
    ("Emacs-Lisp" "emacs" xpm)
    ("Lisp Interaction" "emacs" xpm)
    ("HTML" "html" xpm)
    ("Haml" "haml" xpm)
    ("Image[imagemagick]" "svg" xpm)
    ("Inf-Ruby" "infruby" xpm)
    ("JavaScript" "js" xpm)
    ("Lisp" "cl" xpm)
    ("nXML" "xml" xpm)
    ("Org" "org" xpm)
    ("PHP" "php" xpm)
    ("PHP/l" "php" xpm)
    ("Projectile Rails Server" "rails" xpm)
    ("Python" "python" xpm)
    ("Ruby" "ruby" xpm)
    ("ESS\\[S\\]" "R" xpm)
    ("ESS\\[SAS\\]" "sas" xpm)
    ("ESS\\[BUGS\\]" #xf188 FontAwesome)
    ("iESS" "R" xpm)
    ("SCSS" "sass" xpm)
    ("Sass" "sass" xpm)
    ("Scheme" "scheme" xpm)
    ("Shell-script" "bash" xpm)
    ("Slim" "slim" xpm)
    ("Snippet" "yas" xpm)
    ("Term" "term" xpm)
    ("Web" "html" xpm)
    ("XML" "xml" xpm)
    ("YAML" "yaml" xpm)
    ("YASnippet" "yas" xpm)
    (" yas" "yas" xpm)
    (" hs" "hs" xpm)
    ("Markdown" #xf0c9 github-octicons)
    ("Scala" #xf15b font-mfizz)
    ("Magit" #xf1d2 FontAwesome)
    (" Pulls" #xf092 FontAwesome)
    ("Zip-Archive" #xf1c6 FontAwesome)
    ("ARev" #xf021 FontAwesome)
    ("Calc\\(ulator\\)?" #xf1ec FontAwesome)
    ("Debug.*" #xf188 FontAwesome)
    ("Debug.*" #xf188 FontAwesome)
    ("Calendar" #xf073 FontAwesome)
    ("Help" #xf059 FontAwesome)
    ("WoMan" #xf05a FontAwesome)
    ("C/l" #xf107 font-mfizz)
    ("Custom" #xf013 FontAwesome)
    ("\\`Go\\'" "go" xpm)
    (" Rbow" "rainbow" xpm)
    (" Golden" "golden" xpm) ;; Icon created by Arthur Shlain from Noun Project
    ("BibTeX" "bibtex" xpm)
    ("C[+][+]/l" #xf10c font-mfizz)
    ("C[#]/l" #xf10d font-mfizz)
    ("Elixir" #xf115 font-mfizz)
    ("Erlang" #xf116 font-mfizz)
    ("Haskell" #xf126 font-mfizz)
    ("Clojure" #xf10a font-mfizz)
    ("Java/l" #xf12b font-mfizz)
    ("C?Perl" #xf148 font-mfizz)
    ("Octave" "octave" xpm)
    ("AHK" "autohotkey" xpm)
    ;; Diminished modes
    ("\\(?:ElDoc\\|Anzu\\|SP\\|Guide\\|PgLn\\|Undo-Tree\\|Ergo.*\\|,\\|Isearch\\|Ind\\|Fly\\)" nil nil)
    )
  "Icons for major and minor modes.

Each specificatioun is a list with the first element being the
name of the major mode.  The second the name of the icon file,
without the extension.  And the third being the type of icon."
  :type '(repeat
          (list (string :tag "Regular Expression")
                (choice
                 (string :tag "Icon Name")
                 (integer :tag "Font Glyph Code")
                 (const :tag "Suppress" nil))
                (choice
                 (const :tag "text" nil)
                 (const :tag "Octicons" github-octicons)
                 (const :tag "Fizzed" font-mfizz)
                 (const :tag "Font Awesome" FontAwesome)
                 (const :tag "png" png)
                 (const :tag "gif" gif)
                 (const :tag "jpeg" jpeg)
		 (const :tag "jpg" jpg)
                 (const :tag "xbm" xbm)
                 (const :tag "xpm" xpm))))
  :group 'mode-icons)
 
(defun mode-icons-get-icon-display (icon type)
  "Get the value for the display property of ICON having TYPE.

ICON should be a string naming the file of the icon, without its
extension.  Type should be a symbol designating the file type for
the icon."
  (let ((icon-path (mode-icons-get-icon-file
                    (concat icon "." (symbol-name type)))))
    `(image :type ,(or (and (eq type 'jpg) 'jpeg) type) :file ,icon-path :ascent center)))

(defcustom mode-icons-minor-mode-base-text-properties
  '('help-echo nil
               'mouse-face 'mode-line-highlight
               'local-map mode-line-minor-mode-keymap)
  "List of text propeties to apply to every minor mode."
  :type '(repeat sexp)
  :group 'mode-icons)

(defcustom mode-icons-major-mode-base-text-properties
  '('help-echo "Major mode\nmouse-1: Display major mode menu\nmouse-2: Show help for major mode\nmouse-3: Toggle minor modes"
               'mouse-face 'mode-line-highlight
               'local-map mode-line-major-mode-keymap)
  "List of text propeties to apply to every major mode."
  :type '(repeat sexp)
  :group 'mode-icons)

(defvar mode-icons-powerline-p nil)
(defun mode-icons-need-update-p ()
  "Determine if the mode-icons need an update."
  (not (or (and (boundp 'rich-minority-mode) rich-minority-mode)
           (member 'sml/pos-id-separator mode-line-format)
           (string-match-p "powerline" (prin1-to-string mode-line-format)))))

(defvar mode-icons-font-register-alist nil
  "Alist of characters supported.")

(defun mode-icons-supported-font-p (char font &optional dont-register)
  "Determine if the CHAR is supported in FONT.
When DONT-REGISTER is non-nil, don't register the font.
Otherwise, register the font for use in the mode-line and
everywhere else."
  (when (and (or (integerp char)
                 (and (stringp char) (= 1 (length char))))
             (boundp (intern (format "mode-icons-font-spec-%s" font)))
             (symbol-value (intern (format "mode-icons-font-spec-%s" font))))
    (let* ((char (or (and (integerp char) char)
                     (and (stringp char) (= 1 (length char))
                          (aref (vconcat char) 0))))
           (found-char-p (assoc char mode-icons-font-register-alist))
           (char-font-p (and found-char-p (eq (cdr found-char-p) font))))
      (cond
       (char-font-p t)
       (found-char-p t)
       (t ;; not yet registered.
        (set-fontset-font t (cons char char) (symbol-value (intern (format "mode-icons-font-spec-%s" font))))
        (push (cons char font) mode-icons-font-register-alist)
        t)))))

(defun mode-icons-supported-p (icon-spec)
  "Determine if ICON-SPEC is suppored on your system."
  (or
   (and (or (eq (nth 2 icon-spec) nil) (eq (nth 1 icon-spec) nil)) t)
   (mode-icons-supported-font-p (nth 1 icon-spec) (nth 2 icon-spec) t)
   (and (eq (nth 2 icon-spec) 'jpg) (image-type-available-p 'jpeg))
   (and (image-type-available-p (nth 2 icon-spec)))))

(defun mode-icons-propertize-mode (mode icon-spec)
  "Propertize MODE with ICON-SPEC.

MODE should be a string, the name of the mode to propertize.
ICON-SPEC should be a specification from `mode-icons'."
  (let (tmp)
    (cond
     ((get-text-property 0 'mode-icons-p mode)
      mode)
     ((not (nth 1 icon-spec))
      "")
     ((and (stringp (nth 1 icon-spec)) (not (nth 2 icon-spec)))
      (propertize mode 'display (mode-icons-get-icon-display (nth 1 icon-spec) (nth 2 icon-spec))
                  'mode-icons-p t))
     ((mode-icons-supported-font-p (nth 1 icon-spec) (nth 2 icon-spec))
      ;; (propertize mode 'display (nth 1 icon-spec) 'mode-icons-p t)
      ;; Use `compose-region' because it allows clicable text.
      (with-temp-buffer
        (insert mode)
        (compose-region (point-min) (point-max) (or (and (integerp (nth 1 icon-spec))
                                                         (make-string 1 (nth 1 icon-spec)))
                                                    (nth 1 icon-spec)))
        (put-text-property (point-min) (point-max) 'mode-icons-p t)
        (buffer-string)))
     (t (propertize mode 'display (mode-icons-get-icon-display (nth 1 icon-spec) (nth 2 icon-spec)) 'mode-icons-p t)))))

(defun mode-icons-get-icon-spec (mode)
  "Get icon spec for MODE based on regular expression."
  (catch 'found-mode
    (dolist (item mode-icons)
      (when (and (mode-icons-supported-p item)
		 (string-match-p (car item) mode))
        (throw 'found-mode item)))
    nil))

(defun mode-icons-get-mode-icon (mode)
  "Get the icon for MODE, if there is one."
  (let* ((mode-name (format-mode-line mode))
         (icon-spec (mode-icons-get-icon-spec mode-name)))
    (if icon-spec
        (mode-icons-propertize-mode mode-name icon-spec)
      mode-name)))

(defvar mode-icons-cached-mode-name nil
  "Cached mode name to restore when disabling mode-icons.")

(defun mode-icons-set-mode-icon (mode)
  "Set the icon for MODE."
  (unless mode-icons-cached-mode-name
    (set (make-local-variable 'mode-icons-cached-mode-name)
         mode-name)
    (setq mode-name (mode-icons-get-mode-icon mode))))

(defun mode-icons-major-mode-icons-undo ()
  "Undo the `mode-name' icons."
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when mode-icons-cached-mode-name
        (setq mode-name mode-icons-cached-mode-name
              mode-icons-cached-mode-name nil)))))

(defun mode-icons-major-mode-icons ()
  "Apply mode name icons on all buffers."
  (dolist (b (buffer-list))
    (with-current-buffer b
      (mode-icons-set-current-mode-icon))))

(defun mode-icons-set-current-mode-icon ()
  "Set the icon for the current major mode."
  (mode-icons-set-mode-icon mode-name))

(defvar mode-icons-set-minor-mode-icon-alist nil)

(defun mode-icons-set-minor-mode-icon-undo ()
  "Undo minor modes."
  (let (minor)
    (dolist (mode mode-icons-set-minor-mode-icon-alist)
      (setq minor (assq (car mode) minor-mode-alist))
      (when minor
        (setcdr minor (cdr mode)))))
  (setq mode-icons-set-minor-mode-icon-alist nil)
  (force-mode-line-update))

(defcustom mode-icons-separate-images-with-spaces t
  "Separate minor-mode icons with spaces."
  :type 'boolean
  :group 'mode-icons)

(defun mode-icons-set-minor-mode-icon ()
  "Set the icon for the minor modes."
  (let (icon-spec mode-name minor)
    (dolist (mode minor-mode-alist)
      (unless (assq (car mode) mode-icons-set-minor-mode-icon-alist)
        (setq mode-name (format-mode-line mode)
              icon-spec (mode-icons-get-icon-spec mode-name))
        (when icon-spec
          (setq minor (assq (car mode) minor-mode-alist))
          (when minor
            (or (assq (car mode) mode-icons-set-minor-mode-icon-alist)
                (push (copy-sequence minor) mode-icons-set-minor-mode-icon-alist))
            (setq mode-name (replace-regexp-in-string "^ " "" mode-name)
                  mode-name (mode-icons-propertize-mode mode-name icon-spec))
            (if (string= "" mode-name)
                (setcdr minor (list ""))
              (setcdr minor (list (concat (or (and mode-icons-separate-images-with-spaces " ") "")
                                          mode-name)))))))))
  (force-mode-line-update))

(defun mode-icons--generate-major-mode-item ()
  "Give rich strings needed for `major-mode' viewing."
  (eval `(propertize ,mode-name ,@mode-icons-major-mode-base-text-properties)))

(defun mode-icons--generate-minor-mode-list ()
  "Extracts all rich strings necessary for the minor mode list."
  (delete " " (delete "" (mapcar (lambda(mode)
                                   (concat " " (eval `(propertize ,mode ,@mode-icons-minor-mode-base-text-properties))))
                                 (split-string (format-mode-line minor-mode-alist))))))

;; Based on rich-minority by Artur Malabarba
(defvar mode-icons--backup-construct nil)
(defvar mode-icons--mode-line-construct
  '(:eval (mode-icons--generate-minor-mode-list))
  "Construct used to replace `minor-mode-alist'.")

(defvar mode-icons--major-backup-construct nil)
(defvar mode-icons--major-construct
  '(:eval (mode-icons--generate-major-mode-item))
  "Construct used to replace `mode-name'.")

(defun mode-icons-fix (&optional enable)
  "Fix mode-icons."
  (if enable
      (let ((place (or (member 'minor-mode-alist mode-line-modes)
                       (cl-member-if
                        (lambda (x) (and (listp x)
                                    (equal (car x) :propertize)
                                    (equal (cadr x) '("" minor-mode-alist))))
                        mode-line-modes)))
            (place-major (cl-member-if
                          (lambda(x)
                            (and (listp x)
                                 (equal (car x) :propertize)
                                 (equal (cadr x) '("" mode-name))))
                          mode-line-modes)))
        (when place
          (setq mode-icons--backup-construct (car place))
          (setcar place mode-icons--mode-line-construct))
        (when place-major
          (setq mode-icons--major-backup-construct (car place-major))
          (setcar place-major mode-icons--major-construct)))
    (let ((place (member mode-icons--mode-line-construct mode-line-modes))
          (place-major (member mode-icons--major-backup-construct mode-line-modes)))
      (when place
        (setcar place mode-icons--backup-construct))
      (when place-major
        (setcar place-major mode-icons--major-backup-construct)))))

;;;###autoload
(define-minor-mode mode-icons-mode
  "Replace the name of the current major mode with an icon."
  :global t
  (if mode-icons-mode
      (progn
        (add-hook 'after-change-major-mode-hook 'mode-icons-set-current-mode-icon)
        (add-hook 'after-change-major-mode-hook 'mode-icons-set-minor-mode-icon)
        (mode-icons-fix t)
        (mode-icons-set-minor-mode-icon)
        (mode-icons-major-mode-icons))
    (remove-hook 'after-change-major-mode-hook 'mode-icons-set-minor-mode-icon)
    (remove-hook 'after-change-major-mode-hook 'mode-icons-set-current-mode-icon)
    (mode-icons-set-minor-mode-icon-undo)
    (mode-icons-major-mode-icons-undo)
    (mode-icons-fix)))

(provide 'mode-icons)
;;; mode-icons.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
