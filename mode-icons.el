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
;; - IcoMoon, found at URL `https://icomoon.io/#icons-icomoon'.
;;
;; You should have these installed if you want to use these icons,
;; otherwise you may get strange glyphs in your mode-line instead of
;; an icon.

;;; Code:

(require 'cl-lib)
(require 'color)

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
  (expand-file-name icon (expand-file-name "icons" mode-icons--directory)))

(defmacro mode-icons-save-buffer-state (&rest body)
  "Eval BODY saving buffer state.
This macro restores the buffer state under the assumption that no
significant modification has been made in BODY.  A change is
considered significant if it affects the buffer text in any way
that isn't completely restored again.  Changes in text properties
like `face' or `syntax-table' are considered insignificant.  This
macro allows text properties to be changed, even in a read-only
buffer.

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
       (and (member ,(format "%s" font) (font-family-list)) (font-spec :name ,(format "%s" font))))
     (defvar ,(intern (format "mode-icons-font-%s" font))
       (and (member ,(format "%s" font) (font-family-list)) (find-font ,(intern (format "mode-icons-font-spec-%s" font)))))))

(mode-icons-define-font "github-octicons")
(mode-icons-define-font "font-mfizz")
(mode-icons-define-font "FontAwesome")
(mode-icons-define-font "IcoMoon-Free")

(defcustom mode-icons
  `(("\\`CSS\\'" "css" xpm)
    ("\\`Coffee\\'" "coffee" xpm-bw)
    ("\\`Compilation\\'" "compile" xpm)
    ("\\`Emacs-Lisp\\'" "emacs" xpm)
    ("\\`Lisp Interaction\\'" "emacs" xpm)
    ("\\`HTML\\'" "html" xpm)
    ("\\`Haml\\'" "haml" xpm)
    ("\\`Image[imagemagick]\\'" "svg" xpm)
    ("\\`Inf-Ruby\\'" "infruby" xpm)
    ("\\`JavaScript\\'" "js" xpm)
    ("\\`Lisp\\'" "cl" xpm)
    ("\\`nXML\\'" "xml" xpm)
    ("\\`Org\\'" "org" xpm)
    ("\\`PHP\\(\\|/.*\\)\\'" "php" xpm)
    ("\\`Projectile Rails Server\\'" "rails" xpm)
    ("\\`Python\\'" "python" xpm)
    ("\\`Ruby\\'" "ruby" xpm)
    ("\\`ESS\\[S\\]\\'" "R" xpm)
    ("\\`ESS\\[SAS\\]\\'" "sas" xpm)
    ("\\`ESS\\[BUGS\\]\\'" #xf188 FontAwesome)
    ("\\`iESS\\'" "R" xpm)
    ("\\`SCSS\\'" "sass" xpm)
    ("\\`Sass\\'" "sass" xpm)
    ("\\`Scheme" "scheme" xpm-bw)
    ("\\`Shell-script" "bash" xpm-bw)
    ("\\`Slim" "slim" xpm-bw)
    ("\\`Snippet" "yas" xpm)
    ("\\`Term\\'" "term" xpm)
    ("\\`Web\\'" "html" xpm)
    ("\\`XML\\'" "xml" xpm)
    ("\\`YAML\\'" "yaml" xpm)
    ("\\` YASnippet\\'" "yas" xpm)
    ("\\` yas\\'" "yas" xpm)
    ("\\` hs\\'" "hs" xpm)
    ("\\`Markdown\\'" #xf0c9 github-octicons)
    ("\\`Scala\\'" #xf15b font-mfizz)
    ("\\`Magit\\'" #xf1d2 FontAwesome)
    ("\\` Pulls\\'" #xf092 FontAwesome)
    ("\\`Zip-Archive\\'" #xf1c6 FontAwesome)
    ("\\` ARev\\'" #xf021 FontAwesome)
    ("\\`Calc\\(ulator\\)?\\'" #xf1ec FontAwesome)
    ("\\`Debug.*\\'" #xf188 FontAwesome)
    ("\\`Debug.*\\'" #xf188 FontAwesome)
    ("\\`Calendar\\'" #xf073 FontAwesome)
    ("\\`Help\\'" #xf059 FontAwesome)
    ("\\`WoMan\\'" #xf05a FontAwesome)
    ("\\`C\\(/.*\\|\\)\\'" #xf107 font-mfizz)
    ("\\`Custom\\'" #xf013 FontAwesome)
    ("\\`Go\\'" "go" xpm)
    ("\\` Rbow\\'" "rainbow" xpm)
    ("\\` ICY\\'" "icy" xpm) ;; http://www.clipartpal.com/clipart_pd/weather/ice_10206.html
    ("\\` Golden\\'" "golden" xpm-bw) ;; Icon created by Arthur Shlain from Noun Project
    ("\\`BibTeX\\'\\'" "bibtex" xpm)
    ("\\`C[+][+]\\(/.*\\|\\)\\'" #xf10c font-mfizz)
    ("\\`C[#]\\(/.*\\|\\)\\'" #xf10d font-mfizz)
    ("\\`Elixir\\'" #xf115 font-mfizz)
    ("\\`Erlang\\'" #xf116 font-mfizz)
    ("\\`Haskell\\'" #xf126 font-mfizz)
    ("\\`Clojure\\'" #xf10a font-mfizz)
    ("\\`Java\\(/.*\\|\\)\\'" #xf12b font-mfizz)
    ("\\`C?Perl\\'" #xf148 font-mfizz)
    ("\\`Octave\\'" "octave" xpm)
    ("\\`AHK\\'" "autohotkey" xpm)
    ("\\`Info\\'" #xf05a FontAwesome)
    ("\\` Narrow\\'" #xf066 FontAwesome)
    ("\\`Dockerfile\\'" "docker" xpm)
    (read-only #xf023 FontAwesome)
    (writable #xf09c FontAwesome)
    (save #xf0c7 FontAwesome)
    (saved "" nil)
    (modified-outside #xf071 FontAwesome)
    (steal #xf21b FontAwesome)
    ;; Prefer finder icon since it looks like the old mac icon
    (apple #xeabf IcoMoon-Free)
    (apple #xf179 FontAwesome)
    (win #xf17a FontAwesome)
    ;; FIXME: use lsb_release to determine Linux variant and choose appropriate icon
    (unix #xeabd IcoMoon-Free)  ;; Clear Tux (Unlike FontAwesome)

    ;; This icon is clearer than FontAwesome's Linux Penguin
    (unix #xf166 font-mfizz)    ;; Use ubuntu, since I think it is the most common.
    (unix #xf17c FontAwesome) ;; Fall Back to FontAwesome
    (undecided #xf128 FontAwesome)
    ("Text\\'" #xf0f6 FontAwesome)
    ("\\` ?company\\'" #xf1ad FontAwesome)
    ("\\` ?AC\\'" #xf18e FontAwesome)
    ("\\` ?Fly\\'" #xea12 IcoMoon-Free)
    ("\\` Ergo" #xf11c FontAwesome)
    ("\\` drag\\'" #xf047 FontAwesome)
    ("\\` Helm\\'" "helm" xpm-bw) ;; By Noe Araujo, MX, https://thenounproject.com/term/helm/233101/
    ("\\`Messages\\'" #xf044 FontAwesome)
    ("\\`Conf" #xf1de FontAwesome)
    ("\\`Fundamental\\'" #xf016 FontAwesome)
    ("\\`Javascript-IDE\\'" "js" xpm)
    ;; Diminished modes
    ("\\` \\(?:ElDoc\\|Anzu\\|SP\\|Guide\\|PgLn\\|Undo-Tree\\|Ergo.*\\|,\\|Isearch\\|Ind\\)\\'" nil nil)
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
                 (const :tag "Modified Outside Emacs" modified-outside)
                 (const :tag "Locked By Someone Else" steal)
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
                 (const :tag "Ico Moon Free" IcoMoon-Free)
                 (const :tag "png" png)
                 (const :tag "gif" gif)
                 (const :tag "jpeg" jpeg)
                 (const :tag "jpg" jpg)
                 (const :tag "xbm" xbm)
                 (const :tag "xpm" xpm)
                 (const :tag "Black and White xpm that changes color to match the mode-line face" xpm-bw))))
  :group 'mode-icons)

(defun mode-icons-get-icon-display-xpm-replace (icon-path rep-alist &optional name)
  "Get xpm image from ICON-PATH and reaplce REP-ALIST in file.
When NAME is non-nil, also replace the internal xpm image name."
  (let ((case-fold-search t)
        (img (with-temp-buffer (insert-file-contents icon-path) (buffer-string))))
    (dolist (c rep-alist)
      (setq img (replace-regexp-in-string (regexp-quote (car c)) (cdr c) img t t)))
    (when name
      (setq img (replace-regexp-in-string "^[ ]*static[ ]+char[ ]+[*][ ]+.*?\\[" (concat "static char * " name "[") img t t)))
    img))

(defun mode-icons-interpolate (c1 c2 &optional factor)
  "Interpolate between C1 and C2 by FACTOR.
If FACTOR is unspecified, use 0.5"
  (let* ((factor (or factor 0.5))
         (red (+ (* (nth 0 c1) factor) (* (nth 0 c2) (- 1.0 factor))))
         (green (+ (* (nth 1 c1) factor) (* (nth 1 c2) (- 1.0 factor))))
         (blue (+ (* (nth 2 c1) factor) (* (nth 2 c2) (- 1.0 factor)))))
    (setq red (/ (round (* 256.0 red)) 256.0)
          green (/ (round (* 256.0 green)) 256.0)
          blue (/ (round (* 256.0 blue)) 256.0))
    (color-rgb-to-hex red green blue)))

(defun mode-icons-interpolate-from-scale (foreground background)
  "Interpolate black to FOREGROUND and white to BACKGROUND.
Grayscales are in between."
  (let ((black '(0.0 0.0 0.0))
        (white '(1.0 1.0 1.0))
        lst tmp
        (i 0))
    (while (< i 256)
      (setq tmp (/ i 255.0))
      (push (cons (mode-icons-interpolate black white tmp)
                  (mode-icons-interpolate foreground background tmp)) lst)
      (setq i (1+ i)))
    lst))

(defvar mode-icons-get-icon-display-xpm-bw-face (make-hash-table)
  "Hash table of dynamic images.")

(defun mode-icons-get-icon-display-xpm-bw-face (icon-path &optional face)
  "Change xpm at ICON-PATH to match FACE.
The white is changed to the background color.
The black is changed to the foreground color.
Grayscale colors are aslo changed by `mode-icons-interpolate-from-scale'."
  (let* ((background (color-name-to-rgb (face-background (or face 'mode-line))))
         (foreground (color-name-to-rgb (face-foreground (or face 'mode-line))))
         (lst (mode-icons-interpolate-from-scale foreground background))
         (name (concat "mode_icons_bw_" (substring (mode-icons-interpolate background foreground 0.0) 1) "_"
                       (substring (mode-icons-interpolate background foreground 1.0) 1) "_"
                       (file-name-sans-extension (file-name-nondirectory icon-path))))
         (sym (intern name))
         ret)
    (or (gethash sym mode-icons-get-icon-display-xpm-bw-face)
        (puthash sym (mode-icons-get-icon-display-xpm-replace icon-path lst name) mode-icons-get-icon-display-xpm-bw-face))))

(defvar mode-icons-get-icon-display (make-hash-table :test 'equal)
  "Hash table of `mode-icons-get-icon-display'.")

(defun mode-icons-get-icon-display (icon type &optional face)
  "Get the value for the display property of ICON having TYPE.

ICON should be a string naming the file of the icon, without its
extension.  Type should be a symbol designating the file type for
the icon.

FACE should be the face for rendering black and white xpm icons
specified by type 'xpm-bw."
  (let ((face (or face
                  (and (mode-icons--selected-window-active)
                       'mode-line)
                  'mode-line-inactive)))
    (or (gethash (list icon type face) mode-icons-get-icon-display)
        (puthash (list icon type face)
                 (let ((icon-path (mode-icons-get-icon-file
                                   (concat icon "." (or (and (eq type 'xpm-bw) "xpm")
                                                        (symbol-name type))))))
                   (if (eq type 'xpm-bw)
                       (create-image (mode-icons-get-icon-display-xpm-bw-face icon-path face)
                                     'xpm t :ascent 'center
                                     :face face)
                     `(image :type ,(or (and (eq type 'jpg) 'jpeg) type) :file ,icon-path :ascent center)))
                 mode-icons-get-icon-display))))

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
                             (mouse-1 . mode-icons-save-steal-or-revert-buffer)
                             (mouse-3 . mode-line-toggle-modified)))
                'help-echo 'mode-icons-modified-help-echo)
  "List of text propeties to apply to read-only buffer indicator."
  :type '(repeat sexp)
  :group 'mode-icons)

(defun mode-icons-save-steal-or-revert-buffer (event)
  "Save buffer OR revert file from mode line.
Use EVENT to determine location."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (let* ((bfn (buffer-file-name))
           (revert-p (not (or (and bfn (file-remote-p buffer-file-name))
                              (verify-visited-file-modtime (current-buffer)))))
           (steal-p (and (not (or (and bfn (file-remote-p buffer-file-name))
                                  (member (file-locked-p bfn) '(nil t)))))))
      (cond
       (revert-p (revert-buffer t t))
       (steal-p
        (message "To steal or ignore lock, start editing the file."))
       (t (call-interactively (key-binding (where-is-internal 'save-buffer global-map t))))))
    (force-mode-line-update)))

(defun mode-icons-modified-help-echo (window _object _point)
  "Return help text specifying WINDOW's buffer modification status."
  (let* ((bfn (buffer-file-name))
         (revert-p (not (or (and bfn (file-remote-p buffer-file-name))
                            (verify-visited-file-modtime (current-buffer)))))
          (steal-p (and (not (or (and bfn (file-remote-p buffer-file-name))
                                (member (file-locked-p bfn) '(nil t))))))
         (mod-p (buffer-modified-p (window-buffer window))))
    (format "Buffer is %s\nmouse-1: %s Buffer\nmouse-3: Toggle modification state"
            (cond
             (steal-p
              "locked for editing by another user.")
             (revert-p
              "modified outside of emacs!")
             ((buffer-modified-p (window-buffer window))
              "modified")
             (t "unmodified"))
            (cond
             (steal-p
              "Echo about lock status of")
             (revert-p
              "Revert")
             (mod-p
              "Save")
             (t "")))))

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

(defun mode-icons-supported-font-p (char font)
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
   (mode-icons-supported-font-p (nth 1 icon-spec) (nth 2 icon-spec))
   (and (eq (nth 2 icon-spec) 'jpg) (image-type-available-p 'jpeg))
   (and (eq (nth 2 icon-spec) 'xpm-bw) (image-type-available-p 'xpm))
   (and (image-type-available-p (nth 2 icon-spec)))))

(defun mode-icons-propertize-mode (mode icon-spec &optional face)
  "Propertize MODE with ICON-SPEC.

MODE should be a string, the name of the mode to propertize.
ICON-SPEC should be a specification from `mode-icons'.
FACE is the face to match when a xpm-bw image is used."
  (mode-icons-save-buffer-state ;; Otherwise may cause issues with trasient mark mode
   (cond
    ((and (stringp mode) (get-text-property 0 'mode-icons-p mode))
     mode)
    ((not (nth 1 icon-spec))
     "")
    ((and (stringp (nth 1 icon-spec)) (not (nth 2 icon-spec)))
     (propertize (nth 1 icon-spec) 'display (nth 1 icon-spec)
                 'mode-icons-p icon-spec))
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
       (put-text-property (point-min) (point-max) 'mode-icons-p icon-spec)
       (buffer-string)))
    (t (propertize (format "%s" mode) 'display
                   (mode-icons-get-icon-display (nth 1 icon-spec) (nth 2 icon-spec)
                                                (or face
                                                    (and (mode-icons--selected-window-active)
                                                         'mode-line)
                                                    'mode-line-inactive))
                   'mode-icons-p icon-spec)))))

(defvar mode-icons-get-icon-spec (make-hash-table :test 'equal)
  "Hash table of icon-specifications.")
(defun mode-icons-get-icon-spec (mode)
  "Get icon spec for MODE based on regular expression."
  (or (gethash mode mode-icons-get-icon-spec)
      (puthash mode (let (case-fold-search)
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
               mode-icons-get-icon-spec)))

(defcustom mode-icons-show-mode-name nil
  "Show Icon and `mode-name'."
  :type 'boolean
  :group 'mode-icons)

(defcustom mode-icons-change-mode-name t
  "Change the `mode-name' variable.

This allows functions like `ibuffer' or `helm-mode' to show the
icon as well."
  :type 'boolean
  :group 'mode-icons)

(defun mode-icons-get-mode-icon (mode &optional face)
  "Get the icon for MODE, if there is one.
FACE represents the face used when the icon is a xpm-bw image."
  (let* ((mode-name (format-mode-line mode))
         (icon-spec (mode-icons-get-icon-spec mode-name))
         ret)
    (if icon-spec
        (setq ret
              (if mode-icons-show-mode-name
                      (concat (mode-icons-propertize-mode mode-name icon-spec) " " mode-name)
                    (mode-icons-propertize-mode mode-name icon-spec face)))
      (setq ret mode-name))
    ;; Don't hide major mode names...
    (when (string= ret "")
      (setq ret mode-name))
    ret))

(defvar mode-icons-cached-mode-name nil
  "Cached mode name to restore when disabling mode-icons.")

(defvar mode-icons-mode-name-active nil
  "Active icon for `mode-name'.")

(defvar mode-icons-mode-name-inactive nil
  "Inactive icon for `mode-name'.")

(defun mode-icons-set-mode-icon (mode)
  "Set the icon for MODE."
  (unless mode-icons-cached-mode-name
    (set (make-local-variable 'mode-icons-cached-mode-name)
         mode-name)
    (set (make-local-variable 'mode-icons-mode-name-active)
         (mode-icons-get-mode-icon mode 'mode-line))
    (set (make-local-variable 'mode-icons-mode-name-inactive)
         (mode-icons-get-mode-icon mode 'mode-line-inactive))
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

(defun mode-icons-set-minor-mode-icon-undo (&optional dont-update)
  "Undo minor modes.
When DONT-UPDATE is non-nil, don't call `force-mode-line-update'."
  (let (minor)
    (dolist (mode mode-icons-set-minor-mode-icon-alist)
      (setq minor (assq (car mode) minor-mode-alist))
      (when minor
        (setcdr minor (cdr mode)))))
  (setq mode-icons-set-minor-mode-icon-alist nil)
  (unless dont-update
    (force-mode-line-update)))

(defcustom mode-icons-separate-images-with-spaces t
  "Separate minor-mode icons with spaces."
  :type 'boolean
  :group 'mode-icons)

(defun mode-icons-set-minor-mode-icon (&optional dont-update)
  "Set the icon for the minor modes.
When DONT-UPDATE is non-nil, don't call `force-mode-line-update'"
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
  (unless dont-update
    (force-mode-line-update)))

(defun mode-icons--generate-major-mode-item ()
  "Give rich strings needed for `major-mode' viewing."
  (let ((active (mode-icons--selected-window-active)))
    (eval `(propertize ,(or (and active mode-icons-mode-name-active)
                            mode-icons-mode-name-inactive)
                       ,@mode-icons-major-mode-base-text-properties))))

;;; selected take from powerline
(defvar mode-icons--selected-window (frame-selected-window)
  "Selected window.")

(defun mode-icons--set-selected-window ()
  "Set the variable `mode-icons--selected-window' appropriately."
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq mode-icons--selected-window (frame-selected-window))))

(defun mode-icons--unset-selected-window ()
  "Unsets the variable `mode-icons--selected-window' and update the modeline."
  (setq mode-icons--selected-window nil)
  (force-mode-line-update))

(add-hook 'window-configuration-change-hook 'mode-icons--set-selected-window)

;; focus-in-hook was introduced in emacs v24.4.
;; Gets evaluated in the last frame's environment.
(add-hook 'focus-in-hook 'mode-icons--set-selected-window)

;; focus-out-hook was introduced in emacs v24.4.
(add-hook 'focus-out-hook 'mode-icons--unset-selected-window)

;; Executes after the window manager requests that the user's events
;; be directed to a different frame.
(defadvice handle-switch-frame
    (after mode-icons--set-selected-window-after-switch-frame activate)
  "Make `mode-icons' aware of selected window."
  (mode-icons--set-selected-window))

(defadvice select-window (after mode-icons--select-window activate)
  "Make `mode-icons' aware of selected window."
  (mode-icons--set-selected-window))

(defun mode-icons--selected-window-active ()
  "Return whether the current window is active."
  (eq mode-icons--selected-window (selected-window)))

(defun mode-icons--recolor-minor-mode-image (mode active)
  "Recolor MODE image based on if the window is ACTIVE."
  (let ((icon-spec (get-text-property 0 'mode-icons-p mode)))
    (cond
     ((and icon-spec (eq (nth 2 icon-spec) 'xpm-bw))
      (propertize mode 'display (mode-icons-get-icon-display (nth 1 icon-spec) (nth 2 icon-spec)
                                                             (or (and active 'mode-line)
                                                                 'mode-line-inactive))
                  'mode-icons-p icon-spec))
     (t mode))))

(defun mode-icons--generate-minor-mode-list ()
  "Extracts all rich strings necessary for the minor mode list."
  (let ((active (mode-icons--selected-window-active)))
    (delete " " (delete "" (mapcar (lambda(mode)
                                     (concat " " (eval `(propertize ,(mode-icons--recolor-minor-mode-image mode active)
                                                                    ,@mode-icons-minor-mode-base-text-properties))))
                                   (split-string (format-mode-line minor-mode-alist)))))))

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
          ,(or (ignore-errors
                 (let* ((bfn (buffer-file-name))
                        (nice-file-p (and  (file-remote-p bfn)))
                        (mod (or (and (not (or nice-file-p (verify-visited-file-modtime (current-buffer))))
                                      "!")
                                 (and (not (or nice-file-p (member (file-locked-p bfn) '(nil t))))
                                      "s")
                                 (format-mode-line "%1+")))
                        icon-spec)
                   (setq mod (or (cond
                                  ((not (stringp mod)) "")
                                  ((char-equal ?s (aref mod 0))
                                   (if (setq icon-spec (mode-icons-get-icon-spec 'steal))
                                       (mode-icons-propertize-mode 'steal icon-spec)
                                     mod))
                                  ((char-equal ?! (aref mod 0))
                                   (if (setq icon-spec (mode-icons-get-icon-spec 'modified-outside))
                                       (mode-icons-propertize-mode 'modified-outside icon-spec)
                                     mod))
                                  ((char-equal ?* (aref mod 0))
                                   (if (setq icon-spec (mode-icons-get-icon-spec 'save))
                                       (mode-icons-propertize-mode 'save icon-spec)
                                     mod))
                                  (t
                                   (if (setq icon-spec (mode-icons-get-icon-spec 'saved))
                                       (mode-icons-propertize-mode 'saved icon-spec)
                                     mod)))
                                 ""))
                   (when (and mode-icons-modified-status-space
                              (stringp mod)
                              (not (string= mod "")))
                     (setq mod (concat mod " ")))
                   mod)) "")
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
  "Describe end of line type.
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
  "Fix mode-icons.
When ENABLE is non-nil, enable the changes to the mode line."
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
                               (eq (cl-caadr x) 'mode-line-eol-desc)))
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
        (add-hook 'after-change-major-mode-hook #'mode-icons-reset)
        (mode-icons-fix t)
        (mode-icons-set-minor-mode-icon)
        (mode-icons-major-mode-icons))
    (remove-hook 'after-change-major-mode-hook #'mode-icons-reset)
    (mode-icons-set-minor-mode-icon-undo)
    (mode-icons-major-mode-icons-undo)
    (mode-icons-fix)))

(defun mode-icons-reset-now ()
  "Reset mode-icons icons."
  (interactive)
  (when (and mode-icons-mode (not (minibufferp)))
    (mode-icons-set-current-mode-icon)
    ;; FIXME -- undo to allow `ergoemacs-mode' and color changing
    ;; XPMs.  Seems a bit heavy handed.
    ;; (mode-icons-set-minor-mode-icon-undo t)
    (mode-icons-set-minor-mode-icon)))

(defun mode-icons-reset ()
  "Reset mode-icons icons."
  (interactive)
  (run-with-idle-timer 0.1 nil #'mode-icons-reset-now))

(add-hook 'emacs-startup-hook #'mode-icons-reset)

(provide 'mode-icons)
;;; mode-icons.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
