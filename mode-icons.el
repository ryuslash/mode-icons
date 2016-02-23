;;; mode-icons.el --- Show icons for modes -*- lexical-binding: t; -*-

;; Copyright (C) 2013, 2016  Tom Willemse
;;               2016  Matthew L. Fidler

;; Author: Tom Willemse <tom@ryuslash.org>
;; Keywords: multimedia
;; Version: 0.3.0
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
;; among others:
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
;;
;; As of version 0.3.0 this project includes some icons which use icon
;; fonts instead of images.  These fonts are:
;;
;; - Font Awesome, found at URL `http://fontawesome.io/'.
;; - GitHub Octicons, found at URL `https://octicons.github.com/'.
;; - Font Mfizz, found at URL `http://fizzed.com/oss/font-mfizz'.
;;
;; You should have these installed if you want to use these icons,
;; otherwise you may get strange glyphs in your mode-line instead of
;; an icon.

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

(defmacro mode-icons-save-buffer-state (&rest body)
  "Eval BODY,
then restore the buffer state under the assumption that no significant
modification has been made in BODY.  A change is considered
significant if it affects the buffer text in any way that isn't
completely restored again.  Changes in text properties like `face' or
`syntax-table' are considered insignificant.  This macro allows text
properties to be changed, even in a read-only buffer.

This macro should be placed around all calculations which set
\"insignificant\" text properties in a buffer, even when the buffer is
known to be writeable.  That way, these text properties remain set
even if the user undoes the command which set them.

This macro should ALWAYS be placed around \"temporary\" internal buffer
changes \(like adding a newline to calculate a text-property then
deleting it again\), so that the user never sees them on his
`buffer-undo-list'.

However, any user-visible changes to the buffer \(like auto-newlines\)
must not be within a `ergoemacs-save-buffer-state', since the user then
wouldn't be able to undo them.

The return value is the value of the last form in BODY.

This was stole/modified from `c-save-buffer-state'"
  `(let* ((modified (buffer-modified-p)) (buffer-undo-list t)
          (inhibit-read-only t) (inhibit-point-motion-hooks t)
          before-change-functions after-change-functions
          deactivate-mark
          buffer-file-name buffer-file-truename ; Prevent primitives checking
                                        ; for file modification
          )
     (unwind-protect
         (progn ,@body)
       (and (not modified)
            (buffer-modified-p)
            (set-buffer-modified-p nil)))))

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
    ("Info" #xf05a FontAwesome)
    ("Narrow" #xf066 FontAwesome)
    (read-only #xf023 FontAwesome)
    (writable #xf09c FontAwesome)
    (save #xf0c7 FontAwesome)
    (saved "" nil)
    (apple #xf179 FontAwesome)
    (win #xf17a FontAwesome)
    ;; FIXME: use lsb_release to determine Linux variant and choose appropriate icon
    (unix #xf166 font-mfizz) ;; Use ubuntu, since I think it is the most common.
    (undecided #xf128 FontAwesome)
    ("Text\\'" #xf0f6 FontAwesome)
    ("\\` ?company\\'" #xf1ad FontAwesome)
    ;; Diminished modes
    ("\\(?:ElDoc\\|Anzu\\|SP\\|Guide\\|PgLn\\|Undo-Tree\\|Ergo.*\\|,\\|Isearch\\|Ind\\|Fly\\)" nil nil)
    )
  "Icons for major and minor modes.

Each specificatioun is a list with the first element being the
name of the major mode.  The second the name of the icon file,
without the extension.  And the third being the type of icon."
  :type '(repeat
          (list (choice
                 (string :tag "Regular Expression")
                 (const :tag "Read Only Indicator" read-only)
                 (const :tag "Writable Indicator" writable)
                 (const :tag "Saved" saved)
                 (const :tag "Save" save)
                 (const :tag "Apple" apple)
                 (const :tag "Windows" win)
                 (const :tag "Unix" unix))
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

(defcustom mode-icons-narrow-text-properties
  '('local-map
    '(keymap
      (mode-line keymap
                 (mouse-2 . mode-line-widen)))
    'mouse-face 'mode-line-highlight 'help-echo "mouse-2: Remove narrowing from buffer")
  "List of text propeties to apply to narrowing buffer indicator."
  :type '(repeat sexp)
  :group 'mode-icons)

(defcustom mode-icons-read-only-text-properties
  '('mouse-face 'mode-line-highlight 'local-map
                '(keymap
                  (mode-line keymap
                             (mouse-1 . mode-line-toggle-read-only)))
                'help-echo 'mode-line-read-only-help-echo)
  "List of text propeties to apply to read-only buffer indicator."
  :type '(repeat sexp)
  :group 'mode-icons)

(defcustom mode-icons-modified-text-properties
  '('mouse-face 'mode-line-highlight
                'local-map
                '(keymap
                  (mode-line keymap
                             (mouse-1 . mode-icons-save-buffer)
                             (mouse-3 . mode-line-toggle-modified)))
                'help-echo 'mode-icons-modified-help-echo)
  "List of text propeties to apply to read-only buffer indicator."
  :type '(repeat sexp)
  :group 'mode-icons)

(defun mode-icons-save-buffer (event)
  "Save buffer from mode line.
Use EVENT to determine location."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (call-interactively (key-binding (where-is-internal 'save-buffer global-map t)))
    (force-mode-line-update)))

(defun mode-icons-modified-help-echo (window _object _point)
  "Return help text specifying WINDOW's buffer modification status."
  (format "Buffer is %smodified\nmouse-1: Save Buffer\nmouse-3: Toggle modification state"
          (if (buffer-modified-p (window-buffer window)) "" "not ")))

(defcustom mode-icons-read-only-text-properties
  '('mouse-face 'mode-line-highlight 'local-map
                '(keymap
                  (mode-line keymap
                             (mouse-1 . mode-line-toggle-read-only)))
                'help-echo 'mode-line-read-only-help-echo)
  "List of text propeties to apply to read-only buffer indicator."
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
  (mode-icons-save-buffer-state ;; Otherwise may cause issues with trasient mark mode
   (let (tmp)
     (cond
      ((and (stringp mode) (get-text-property 0 'mode-icons-p mode))
       mode)
      ((not (nth 1 icon-spec))
       "")
      ((and (stringp (nth 1 icon-spec)) (not (nth 2 icon-spec)))
       (propertize (nth 1 icon-spec) 'display (nth 1 icon-spec)
                   'mode-icons-p t))
      ((mode-icons-supported-font-p (nth 1 icon-spec) (nth 2 icon-spec))
       ;; (propertize mode 'display (nth 1 icon-spec) 'mode-icons-p t)
       ;; Use `compose-region' because it allows clicable text.
       (with-temp-buffer
         (if (stringp mode)
             (insert mode)
           (insert (or (and (integerp (nth 1 icon-spec))
                            (make-string 1 (nth 1 icon-spec)))
                       (nth 1 icon-spec))))
         (compose-region (point-min) (point-max) (or (and (integerp (nth 1 icon-spec))
                                                          (make-string 1 (nth 1 icon-spec)))
                                                     (nth 1 icon-spec)))
         (put-text-property (point-min) (point-max) 'mode-icons-p t)
         (buffer-string)))
      (t (propertize (format "%s" mode) 'display (mode-icons-get-icon-display (nth 1 icon-spec) (nth 2 icon-spec)) 'mode-icons-p t))))))

(defun mode-icons-get-icon-spec (mode)
  "Get icon spec for MODE based on regular expression."
  (catch 'found-mode
    (dolist (item mode-icons)
      (when (and (mode-icons-supported-p item)
                 (or
                  (and
                   (stringp (car item))
                   (stringp mode)
                   (string-match-p (car item) mode))
                  (and
                   (symbolp (car item))
                   (symbolp mode)
                   (eq mode (car item)))))
        (throw 'found-mode item)))
    nil))

(defcustom mode-icons-show-mode-name nil
  "Show Icon and `mode-name'."
  :type 'boolean
  :group 'mode-icons)

(defun mode-icons-get-mode-icon (mode)
  "Get the icon for MODE, if there is one."
  (let* ((mode-name (format-mode-line mode))
         (icon-spec (mode-icons-get-icon-spec mode-name)))
    (if icon-spec
        (if mode-icons-show-mode-name
            (concat (mode-icons-propertize-mode mode-name icon-spec) " " mode-name)
          (mode-icons-propertize-mode mode-name icon-spec))
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

(defun mode-icons--generate-narrow ()
  "Extracts all rich strings necessary for narrow indicator."
  (let (icon-spec)
    (delete " " (delete "" (mapcar (lambda(mode)
                                     (concat " " (eval `(propertize
                                                         ,(if (setq icon-spec (mode-icons-get-icon-spec mode))
                                                              (mode-icons-propertize-mode mode icon-spec)
                                                            mode)
                                                         ,@mode-icons-narrow-text-properties))))
                                   (split-string (format-mode-line "%n")))))))


(defcustom mode-icons-read-only-space t
  "Add Space after read-only icon."
  :type 'boolean
  :group 'mode-icons)

(defun mode-icons--read-only-status ()
  "Get Read Only Status icon."
  (eval `(propertize
          ,(let ((ro (format-mode-line "%1*"))
                 icon-spec)
             (setq ro (or (cond
                           ((string= "%" ro)
                            (if (setq icon-spec (mode-icons-get-icon-spec 'read-only))
                                (mode-icons-propertize-mode 'read-only icon-spec)
                              ro))
                           (t
                            (if (setq icon-spec (mode-icons-get-icon-spec 'writable))
                                (mode-icons-propertize-mode 'writable icon-spec)
                              ro)))
                          ""))
             (when (and mode-icons-read-only-space
                        (not (string= ro "")))
               (setq ro (concat ro " ")))
             ro)
          ,@mode-icons-read-only-text-properties)))

(defcustom mode-icons-modified-status-space t
  "Add Space to modified status."
  :type 'boolean
  :group 'mode-icons)

(defun mode-icons--modified-status ()
  "Get modified status icon."
  (eval `(propertize
          ,(let ((mod (format-mode-line "%1+"))
                 icon-spec)
             (setq mod (or (cond
                            ((string= "*" mod)
                             (if (setq icon-spec (mode-icons-get-icon-spec 'save))
                                 (mode-icons-propertize-mode 'save icon-spec)
                               mod))
                            (t
                             (if (setq icon-spec (mode-icons-get-icon-spec 'saved))
                                 (mode-icons-propertize-mode 'saved icon-spec)
                               mod)))
                           ""))
             (when (and mode-icons-modified-status-space
                        (not (string= mod "")))
               (setq mod (concat mod " ")))
             mod)
          ,@mode-icons-modified-text-properties)))

;; Based on rich-minority by Artur Malabarba
(defvar mode-icons--backup-construct nil)
(defvar mode-icons--mode-line-construct
  '(:eval (mode-icons--generate-minor-mode-list))
  "Construct used to replace `minor-mode-alist'.")

(defvar mode-icons--major-backup-construct nil)
(defvar mode-icons--major-construct
  '(:eval (mode-icons--generate-major-mode-item))
  "Construct used to replace `mode-name'.")

(defvar mode-icons--narrow-backup-construct nil)
(defvar mode-icons--narrow-construct
  '(:eval (mode-icons--generate-narrow))
  "Construct used to replace %n in `mode-line-modes'.")


(defvar mode-icons--read-only-backup-construct nil)
(defvar mode-icons--read-only-construct
  '(:eval (mode-icons--read-only-status))
  "Construct used to replace %1* in `mode-line-modified'.")


(defvar mode-icons--modified-backup-construct nil)
(defvar mode-icons--modified-construct
  '(:eval (mode-icons--modified-status))
  "Construct used to replace %1+ in `mode-line-modified'.")

(defvar mode-icons--backup-eol-construct nil)
(defvar mode-icons--eol-construct
  '(:eval (mode-icons--mode-line-eol-desc))
  "End of Line Construct.")

(defcustom mode-icons-eol-space t
  "Add a space to the end of line specification."
  :type 'boolean
  :group 'mode-icons)

(defcustom mode-icons-eol-text nil
  "Describe end of line type
\(Unix) -> LF
\(DOS) -> CRLF
\(Mac) -> CR"
  :type 'boolean
  :group 'mode-icons)

(defun mode-icons--mode-line-eol-desc (&optional string)
  "Modify `mode-line-eol-desc' to have icons.
STRING is the string to modify, or if absent, the value from `mode-line-eol-desc'."
  (let* ((str (or string (mode-line-eol-desc)))
         (props (text-properties-at 0 str))
         (lt2 "")
         icon-spec)
    (setq str (or (cond
                   ((string= "(Unix)" str)
                    (setq lt2 " LF")
                    (if (setq icon-spec (mode-icons-get-icon-spec 'unix))
                        (mode-icons-propertize-mode 'unix icon-spec)
                      str))
                   ((or (string= str "(DOS)")
                        (string= str "\\"))
                    (setq lt2 " CRLF")
                    (if (setq icon-spec (mode-icons-get-icon-spec 'win))
                        (mode-icons-propertize-mode 'win icon-spec)
                      str))
                   ((string= str "(Mac)")
                    (setq lt2 " CR")
                    (if (setq icon-spec (mode-icons-get-icon-spec 'apple))
                        (mode-icons-propertize-mode 'apple icon-spec)
                      str))
                   ((string= str ":")
                    (setq lt2 " Undecided")
                    (if (setq icon-spec (mode-icons-get-icon-spec 'undecided))
                        (mode-icons-propertize-mode 'undecided icon-spec)
                      str))
                   (t str))
                  ""))
    (when mode-icons-eol-text
      (setq str (concat str lt2)))
    (when (and mode-icons-eol-space
               (not (string= "" str)))
      (setq str (concat str " ")))
    (add-text-properties 0 (length str) props str)
    str))


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
                          mode-line-modes))
            (place-narrow (cl-member-if
                           (lambda(x)
                             (and (stringp x) (string= "%n" x)))
                           mode-line-modes))
            (place-ro (cl-member-if
                       (lambda(x)
                         (and (stringp x) (string-match-p "%[0-9]*[*]" x)))
                       mode-line-modified))
            (place-mod (cl-member-if
                        (lambda(x)
                          (and (stringp x) (string-match-p "%[0-9]*[+]" x)))
                        mode-line-modified))
            (place-eol (cl-member-if
                        (lambda(x)
                          (and (listp x)
                               (equal (car x) :eval)
                               (eq (caadr x) 'mode-line-eol-desc)))
                        mode-line-mule-info)))
        (when place
          (setq mode-icons--backup-construct (car place))
          (setcar place mode-icons--mode-line-construct))
        (when place-major
          (setq mode-icons--major-backup-construct (car place-major))
          (setcar place-major mode-icons--major-construct))
        (when place-narrow
          (setq mode-icons--narrow-backup-construct (car place-narrow))
          (setcar place-narrow mode-icons--narrow-construct))
        (when place-ro
          (setq mode-icons--read-only-backup-construct (car place-ro))
          (setcar place-ro mode-icons--read-only-construct))
        (when place-mod
          (setq mode-icons--modified-backup-construct (car place-mod))
          (setcar place-mod mode-icons--modified-construct))
        (when place-eol
          (setq mode-icons--backup-eol-construct (car place-eol))
          (setcar place-eol mode-icons--eol-construct)))
    (let ((place (member mode-icons--mode-line-construct mode-line-modes))
          (place-major (member mode-icons--major-construct mode-line-modes))
          (place-narrow (member mode-icons--narrow-construct mode-line-modes))
          (place-ro (member mode-icons--read-only-construct mode-line-modified))
          (place-mod (member mode-icons--modified-construct mode-line-modified))
          (place-eol (member mode-icons--eol-construct mode-line-mule-info)))
      (when place
        (setcar place mode-icons--backup-construct))
      (when place-major
        (setcar place-major mode-icons--major-backup-construct))
      (when place-narrow
        (setcar place-narrow mode-icons--narrow-backup-construct))
      (when place-ro
        (setcar place-ro mode-icons--read-only-backup-construct))
      (when place-mod
        (setcar place-mod mode-icons--modified-backup-construct))
      (when place-eol
        (setcar place-eol mode-icons--backup-eol-construct)))))

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
