;;; mode-icons.el --- Show icons for modes -*- lexical-binding: t; -*-

;; Copyright (C) 2013, 2016  Tom Willemse
;;               2016  Matthew L. Fidler

;; Author: Tom Willemse <tom@ryuslash.org>
;; Keywords: multimedia
;; Version: 0.4.0
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
;; - C
;; - C++
;; - C#
;; - CSS
;; - Coffee
;; - Dart
;; - Emacs-Lisp
;; - HTML
;; - Haml
;; - JavaScript
;; - Lisp
;; - Lua
;; - nXML
;; - PHP
;; - Python
;; - React
;; - Ruby
;; - Rust
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
;; As of version 0.3.0 this project includes some icons which can use icon
;; fonts instead of images.  These fonts are:
;;
;; - Font Awesome, found at URL `http://fontawesome.io/'.
;; - GitHub Octicons, found at URL `https://octicons.github.com/'.
;; - Font Mfizz, found at URL `http://fizzed.com/oss/font-mfizz'.
;; - IcoMoon, found at URL `https://icomoon.io/#icons-icomoon'.
;;

;;; Code:

(declare-function comint-send-string "comint")
(declare-function emojify-set-emoji-data "emojify")
(declare-function ht-get "ht")
(declare-function powerline-minor-modes "powerline")
(declare-function powerline-raw "powerline-raw")
(declare-function pl/add-text-property "powerline")
(declare-function mode-icons--real-powerline-raw "powerline")
(declare-function mode-icons--powerline-raw "mode-icons")
(declare-function mode-icons--real-powerline-major-mode "powerline")
(declare-function mode-icons--powerline-major-mode "mode-icons")

(require 'cl-lib)
(require 'color)
(require 'emojify nil t)

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

(defcustom mode-icons
  `(("\\`CSS\\'" "css" xpm)
    ("\\`Coffee\\'" "coffee" xpm-bw)
    ("\\`Compilation\\'" "compile" xpm)
    ("\\`Dart\\'" "dart" xpm)
    ("\\`Elixir\\'" "elixir" xpm)
    ("\\`Erlang\\'" "erlang" xpm)
    ("\\`Emacs-Lisp\\'" "emacs" xpm)
    ("\\`Lisp Interaction\\'" "emacs" xpm)
    ("\\`HTML\\'" "html" xpm)
    ("\\`Haml\\'" "haml" xpm)
    ("\\`Image\\[imagemagick\\]\\'" "svg" xpm)
    ("\\`Inf-Ruby\\'" "infruby" xpm)
    ("\\`Java[Ss]cript\\'" "js" xpm)
    ("\\`Lisp\\'" "cl" xpm)
    ("\\`Lua\\'" "Lua-Logo_16x16" png)
    ("\\`nXML\\'" "xml" xpm)
    ("\\`Org\\'" "org" xpm)
    ("\\`PHP\\(\\|/.*\\)\\'" "php" xpm)
    ("\\`Projectile Rails Server\\'" "rails" xpm)
    ("\\`Python\\'" "python" xpm)
    ("\\` Emmet\\'" "emmet" xpm)
    ("\\`RJSX\\'" "react" xpm)
    ("\\`Ruby\\'" "ruby" xpm)
    ("\\`Rust\\'" "rust" xpm)
    ("\\`EnhRuby\\'" "ruby" xpm)
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
    ("\\` ?YASnippet\\'" "yas" xpm)
    ("\\` ?yas\\'" "yas" xpm)
    ("\\` ?hs\\'" "hs" xpm)
    ("\\`Markdown\\'" #xf0c9 github-octicons)
    ("\\`GFM\\'" #xf0c9 github-octicons)
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
    ("\\`C\\(/.*\\|\\)\\'" "c" xpm)
    ("\\`Custom\\'" #xf013 FontAwesome)
    ("\\`Go\\'" "go" xpm)
    ("\\` ?Rbow\\'" "rainbow" xpm)
    ("\\` ?ivy\\'" "ivy" xpm) ;; Icon created by Philipp Lehmann from the Noun Project https://thenounproject.com/search/?q=ivy&i=329756
    ("\\` ?ICY\\'" "icy" xpm) ;; http://www.clipartpal.com/clipart_pd/weather/ice_10206.html
    ("\\` ?Golden\\'" "golden" xpm-bw) ;; Icon created by Arthur Shlain from Noun Project
    ("\\`BibTeX\\'\\'" "bibtex" xpm-bw)
    ("\\`C[+][+]\\(/.*\\|\\)\\'" "cpp" xpm)
    ("\\`C[#]\\(/.*\\|\\)\\'" "csharp" xpm)
    ("\\`Haskell\\'" #xf126 font-mfizz)
    ("\\`Clojure\\'" #xf10b font-mfizz)
    ("\\`Java\\(/.*\\|\\)\\'" #xf12b font-mfizz)
    ("\\`C?Perl\\'" #xf148 font-mfizz)
    ("\\`Octave\\'" "octave" xpm)
    ("\\`AHK\\'" "autohotkey" xpm)
    ("\\`Info\\'" #xf05a FontAwesome)
    ("\\` ?Narrow\\'" #xf066 FontAwesome)
    ("\\`Dockerfile\\'" "docker" xpm)
    ("\\`Spacemacs buffer\\'" "spacemacs" png)
    ("\\` ?emoji\\'" "emoji" png)
    ("\\`Org-Agenda" #xf046 FontAwesome)
    ("\\`PS\\'" "powershell" xpm)
    (mode-icons-powershell-p "powershell" xpm)
    (mode-icons-cmd-p "cmd" xpm-bw)
    (mode-icons-msys-p "msys" xpm)
    (mode-icons-cygwin-p "cygwin" xpm)
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
    ;; ("\\` ?FlyC.*\\'" "flycheck" xpm)
    ("\\` ?SP\\(/s\\)?\\'" "smartparens" xpm)
    ("\\` ?Ergo" #xf11c FontAwesome)
    ("\\` ?drag\\'" #xf047 FontAwesome)
    ("\\` ?Helm\\'" "helm" xpm-bw) ;; By Noe Araujo, MX, https://thenounproject.com/term/helm/233101/
    ("\\`Messages\\'" #xf27b FontAwesome)
    ("\\`Conf" #xf1de FontAwesome)
    ("\\`Fundamental\\'" #xf016 FontAwesome)
    ("\\`Javascript-IDE\\'" "js" xpm)
    ("\\` Undo-Tree\\'" ":palm_tree:" emoji)
    ("\\`LaTeX\\'" "tex" ext)
    ("\\`Image\\[xpm\\]\\'" "xpm" ext)
    ("\\`Image\\[png\\]\\'" "png" ext)
    ("\\` ?AI\\'" #xf03c FontAwesome)
    ("\\` ?Isearch\\'" #xf002)
    (default #xf059 FontAwesome)
    ;; Diminished modes
    ("\\` ?\\(?:ElDoc\\|Anzu\\|SP\\|Guide\\|PgLn\\|Undo-Tree\\|Ergo.*\\|,\\|Isearch\\|Ind\\)\\'" nil nil))
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
                 (const :tag "Unix" unix)
                 (const :tag "Default Icon" default)
                 (function :tag "Enriched minor mode"))
                (choice
                 (string :tag "Icon Name")
                 (integer :tag "Font Glyph Code")
                 (const :tag "ess" nil))
                (choice
                 (const :tag "text" nil)
                 (const :tag "png" png)
                 (const :tag "gif" gif)
                 (const :tag "jpeg" jpeg)
                 (const :tag "jpg" jpg)
                 (const :tag "xbm" xbm)
                 (const :tag "xpm" xpm)
                 (const :tag "Black and White xpm that changes color to match the mode-line face" xpm-bw)
                 (const :tag "Emoji" emoji)
                 (const :tag "Mode Icons Generated file-type" ext)
                 (symbol :tag "Font"))))
  :group 'mode-icons)

(defun mode-icons-powershell-p (&optional match)
  "Is the current mode a powershell process?"
  (let ((proc (get-buffer-process (current-buffer))))
    (and proc (string-match-p (or match "powershell") (car (process-command proc))))))

(defun mode-icons-cmd-p ()
  "Is the current mode a CMD shell?"
  (mode-icons-powershell-p "cmdproxy"))

(defun mode-icons-cygwin-p ()
  "Is the current mode a CMD shell?"
  (mode-icons-powershell-p "cygwin"))

(defun mode-icons-msys-p ()
  "Is the current mode a CMD shell?"
  (mode-icons-powershell-p "msys"))

(defvar mode-icons-get-xpm-string (make-hash-table :test 'equal))
(defun mode-icons-get-xpm-string (icon-path)
  "Get XPM file contents for ICON-PATH.
If ICON-PATH is a string, return that."
  (or (and (file-exists-p icon-path)
           (or (gethash icon-path mode-icons-get-xpm-string)
               (puthash icon-path (mode-icons-save-buffer-state (with-temp-buffer (insert-file-contents icon-path) (buffer-string)))
                        mode-icons-get-xpm-string)))
      (and (stringp icon-path) icon-path)))

(defun mode-icons-get-icon-display-xpm-replace (icon-path rep-alist &optional name)
  "Get xpm image from ICON-PATH and replace REP-ALIST in file.
When NAME is non-nil, also replace the internal xpm image name."
  (let ((case-fold-search t)
        (img (mode-icons-get-xpm-string icon-path))
        (i 0))
    (dolist (c rep-alist)
      (setq img (replace-regexp-in-string (regexp-quote (car c)) (format "COLOR<%d>" i) img t t)
            i (1+ i)))
    (let ((i 0))
      (dolist (c rep-alist)
        (setq img (replace-regexp-in-string (format "COLOR<%d>" i) (cdr c) img t t)
              i (1+ i))))
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
Grayscales are in between.
Assumes that FOREGROUND and BACKGROUND are (r g b) lists."
  (let ((black '(0.0 0.0 0.0))
        (white '(1.0 1.0 1.0))
        lst tmp
        (i 0))
    (while (< i 256)
      (setq tmp (/ i 255.0))
      (push (cons (upcase (mode-icons-interpolate black white tmp))
                  (upcase (mode-icons-interpolate foreground background tmp))) lst)
      (setq i (1+ i)))
    lst))

(defvar mode-icons-get-icon-display-xpm-bw-face (make-hash-table)
  "Hash table of dynamic images.")

(defun mode-icons-background-color (&optional face)
  "Get the background color of FACE.
In order, will try to get the background color from:
- FACE
- `mode-line' face
- `default' face
- Assume white."
  (color-name-to-rgb (or (face-background (or face 'mode-line))
                         (face-background 'mode-line)
                         (face-background 'default)
                         "white")))

(defun mode-icons-foreground-color (&optional face)
  "Get the foreground color of FACE.
In order, will try to get the foreground color from:
- FACE
- `mode-line' face
- `default' face
- Assume black."
  (color-name-to-rgb (or (face-foreground (or face 'mode-line))
                         (face-foreground 'mode-line)
                         (face-foreground 'default)
                         "black")))

(defun mode-icons-get-icon-display-xpm-bw-face (icon-path &optional face)
  "Change xpm at ICON-PATH to match FACE.
The white is changed to the background color.
The black is changed to the foreground color.
Grayscale colors are also changed by `mode-icons-interpolate-from-scale'."
  (let* ((background (mode-icons-background-color face))
         (foreground (mode-icons-foreground-color face))
         (lst (mode-icons-interpolate-from-scale foreground background))
         (name (concat "mode_icons_bw_" (substring (mode-icons-interpolate background foreground 0.0) 1) "_"
                       (substring (mode-icons-interpolate background foreground 1.0) 1) "_"
                       (file-name-sans-extension (file-name-nondirectory icon-path))))
         (sym (intern name)))
    (or (gethash sym mode-icons-get-icon-display-xpm-bw-face)
        (puthash sym (mode-icons-get-icon-display-xpm-replace icon-path lst name) mode-icons-get-icon-display-xpm-bw-face))))

(defun mode-icons-get-xpm-icon-colors (icon-path)
  "Get a list of rgb colors based on ICON-PATH xpm icon.
ICON-PATH can be a XPM string or a XPM file."
  (let (colors)
    (mode-icons-save-buffer-state
     (with-temp-buffer
       (insert (mode-icons-get-xpm-string icon-path))
       (goto-char (point-min))
       (while (re-search-forward "#[0-9A-Fa-f]\\{6\\}" nil t)
         (push (color-name-to-rgb (match-string 0)) colors))))
    colors))

(defun mode-icons-desaturate-colors (colors &optional foreground background)
  "Desaturate COLORS.

If COLORS is an icon-path of an xpm file, use the colors from
that file.

When FOREGROUND and BACKGROUND are both non-nil, use
`mode-icons-interpolate-from-scale' to change the grayscale to
match the foreground (black) and background (white) colors.

Assume that COLORS is a list of (r g b) values.

Returns a replacement list for `mode-icons-get-icon-display-xpm-replace'"
  (if (and colors (stringp colors))
      (mode-icons-desaturate-colors (mode-icons-get-xpm-icon-colors colors) foreground background)
    (let (color-list
          val tmp
          (trans-alist (and foreground background (mode-icons-interpolate-from-scale foreground background))))
      (dolist (color colors)
        (setq val (+ (* 0.3 (nth 0 color)) (* 0.59 (nth 1 color)) (* 0.11 (nth 2 color)))
              val (upcase (color-rgb-to-hex val val val)))
        (when (and trans-alist (setq tmp (assoc val trans-alist)))
          (setq val (cdr tmp)))
        (push (cons (upcase (color-rgb-to-hex (nth 0 color) (nth 1 color) (nth 2 color))) val) color-list))
      color-list)))

(defun mode-icons-desaturate-xpm (icon-path &optional face)
  "Desaturate the xpm at ICON-PATH.
When FACE is non-nil, match the foreground and background colors
in FACE instead of making the image black and white."
  (let* ((background (mode-icons-background-color face))
         (foreground (mode-icons-foreground-color face))
         (lst (mode-icons-desaturate-colors icon-path foreground background))
         (name (concat "mode_icons_desaturate_"
                       (or (and background foreground
                                (substring (mode-icons-interpolate background foreground 0.0) 1))
                           "black") "_"
                           (or (and background foreground
                                    (substring (mode-icons-interpolate background foreground 1.0) 1))
                               "white") "_"
                       (file-name-sans-extension (file-name-nondirectory icon-path))))
         (sym (intern name)))
    (or (gethash sym mode-icons-get-icon-display-xpm-bw-face)
        (puthash sym (mode-icons-get-icon-display-xpm-replace icon-path lst name) mode-icons-get-icon-display-xpm-bw-face))))


(defcustom mode-icons-desaturate-inactive t
  "Should the inactive mode-line be desaturated.
And changed to match the icon colors?
This only works with xpm files."
  :type 'boolean
  :group 'mode-icons)

(defcustom mode-icons-desaturate-active nil
  "Should the active mode-line be desaturated.
And changed to match the icon colors?
This only works with xpm files."
  :type 'boolean
  :group 'mode-icons)

(defcustom mode-icons-grayscale-transform t
  "Should grayscale 'xpm-bw images match mode-line colors?"
  :type 'boolean
  :group 'mode-icons)

(defvar mode-icons-get-icon-display (make-hash-table :test 'equal)
  "Hash table of `mode-icons-get-icon-display'.")

(defun mode-icons--get-face (&optional face active)
  "If FACE is unspecified, use ACTIVE to determine the face.
ACTIVE tells if current window is active."
  (or face (and active 'mode-line) 'mode-line-inactive))

(defcustom mode-icons-line-height-adjust 0
  "The manual adjustment of the mode-line height for images."
  :type 'integer
  :group 'mode-icons)

(defun mode-icons-line-height (&optional window)
  "Gets the height in pixels of WINDOW's mode-line, if accessible.
This uses `window-mode-line-height' on emacs 24.4+.  Otherwise it assumes 16.

This function also adjusts the line height by `mode-icons-line-height-adjust'."
  (+ mode-icons-line-height-adjust
     (or (and (fboundp 'window-mode-line-height) (window-mode-line-height window)) 16)))

(defun mode-icons-get-icon-display (icon type &optional face active)
  "Get the value for the display property of ICON having TYPE.

ICON should be a string naming the file of the icon, without its
extension.  Type should be a symbol designating the file type for
the icon.

FACE should be the face for rendering black and white xpm icons
specified by type 'xpm-bw.

ACTIVE is an indicator that the current window is active."
  (let* ((face (mode-icons--get-face face active))
         (key (list icon type face active
                    mode-icons-desaturate-inactive mode-icons-desaturate-active
                    mode-icons-grayscale-transform custom-enabled-themes))
         tmp)
    (or (gethash key mode-icons-get-icon-display)
        (puthash key
                 (cond
                  ((memq type '(png xpm xpm-bw gif jpeg jpg xbm xpm))
                   (let ((icon-path (mode-icons-get-icon-file
                                     (concat icon "." (or (and (eq type 'xpm-bw) "xpm")
                                                          (symbol-name type))))))
                     (cond
                      ((and mode-icons-grayscale-transform (eq type 'xpm-bw))
                       (create-image (mode-icons-get-icon-display-xpm-bw-face icon-path face)
                                     ;; Use imagemagick for rescaling...
                                     (or (and (fboundp 'imagemagick-types)
                                              (memq 'png (imagemagick-types)) 'imagemagick)
                                         'xpm)
                                     t :ascent 'center
                                     :face face
                                     :xpm-bw t
                                     :height (mode-icons-line-height)
                                     :icon icon))
                      ((eq type 'xpm-bw)
                       (create-image icon-path
                                     (or (and (fboundp 'imagemagick-types)
                                              (memq 'png (imagemagick-types)) 'imagemagick)
                                         'xpm)
                                     :height (mode-icons-line-height)
                                     :ascent 'center
                                     :face face
                                     :icon icon))
                      ((and (eq type 'xpm)
                            (or (and active mode-icons-desaturate-active)
                                (and (not active) mode-icons-desaturate-inactive)))
                       (create-image (mode-icons-desaturate-xpm icon-path face)
                                     (or (and (fboundp 'imagemagick-types)
                                              (memq 'png (imagemagick-types)) 'imagemagick)
                                         'xpm) t
                                         :ascent 'center
                                         :height (mode-icons-line-height)
                                         :face face :icon icon))
                      (t
                       (create-image icon-path
                                     (or (and (fboundp 'imagemagick-types)
                                              (memq (or (and (eq type 'jpg) 'jpeg) type) (imagemagick-types))
                                              'imagemagick)
                                         (or (and (eq type 'jpg) 'jpeg) type))
                                     nil 
                                     :height (mode-icons-line-height)
                                     :ascent 'center :face face :icon icon)))))
                  ((and (eq type 'emoji) (setq tmp (mode-icons--get-emoji " " (list "" icon type) face)))
                   (get-text-property 0 'display tmp))
                  ;; Shouldn't get here...
                  ((and (eq type 'ext) (setq tmp (mode-icons--ext-available-p (list "" icon type))))
                   (mode-icons-get-icon-display (concat "ext-" (downcase icon)) 'xpm-bw face active))
                  ((and (image-type-available-p 'xpm)
                        (setq tmp (mode-icons--get-font-xpm-file (list "" icon type)))
                        (file-exists-p tmp))
                   (setq tmp nil)
                   (mode-icons-get-icon-display (mode-icons--get-font-xpm-file (list "" icon type) t) 'xpm-bw face active))
                  (t nil))
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
  (if (memq font '(ext emoji xpm xbm jpg jpeg gif png nil)) nil
    (unless (boundp (intern (format "mode-icons-font-spec-%s" font)))
      (set (intern (format "mode-icons-font-spec-%s" font))
           (and (member (format "%s" font) (font-family-list))
                (font-spec :name (format "%s" font)))))
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
          t))))))

(defun mode-icons-supported-p (icon-spec)
  "Determine if ICON-SPEC is suppored on your system."
  (or
   (and (or (eq (nth 2 icon-spec) nil) (eq (nth 1 icon-spec) nil)) t)
   (and (eq (nth 2 icon-spec) 'emoji)
        (or (and (image-type-available-p 'png) (featurep 'emojify))
            (and (image-type-available-p 'xpm)
                 (file-exists-p (mode-icons--get-emoji-xpm-file icon-spec)))))
   (and (eq (nth 2 icon-spec) 'jpg) (image-type-available-p 'jpeg))
   (and (eq (nth 2 icon-spec) 'xpm-bw) (image-type-available-p 'xpm))
   (and (eq (nth 2 icon-spec) 'ext) (image-type-available-p 'xpm)
        (mode-icons--ext-available-p icon-spec))
   (or (mode-icons-supported-font-p (nth 1 icon-spec) (nth 2 icon-spec))
       (and (image-type-available-p 'xpm)
            (mode-icons--get-font-xpm-file icon-spec)
            (file-exists-p (mode-icons--get-font-xpm-file icon-spec))))
   (or (image-type-available-p (nth 2 icon-spec))
       (and (eq (nth 2 icon-spec) 'png)
            (and (image-type-available-p 'xpm)
                 (file-exists-p (mode-icons--get-png-xpm-file icon-spec))))) ))

(defvar emojify-emojis)

(defvar mode-icons--gimp (executable-find "gimp")
  "Gimp to convert png to xpm.")

(defvar mode-icons--gimp-inferior-args "-i -d -b -")

(defvar mode-icons--stop-gimp-after nil
  "Seconds of idle time before mode-icons gimp is stopped.
When nil, don't stop the gimp inferior mode.")

(defvar mode-icons--stop-gimp-timer nil)

(defun mode-icons--start-gimp-inferior ()
  "GIMP inferior process."
  (interactive)
  (when (file-exists-p mode-icons--gimp)
    (unless (get-buffer "*mode-icons-gimp*")
      (cl-letf (((symbol-function 'pop-to-buffer-same-window) (lambda(&rest _ignore))))
        (save-excursion
          (run-scheme  (format "\"%s\" %s" mode-icons--gimp mode-icons--gimp-inferior-args))))
      (with-current-buffer (get-buffer "*scheme*")
        (rename-buffer "*mode-icons-gimp*")
        (set-process-query-on-exit-flag (get-buffer-process (get-buffer "*mode-icons-gimp*")) nil)))))

(defvar mode-icons--gimp-ready-p nil)

(defun mode-icons--gimp-ready-p ()
  "Determine if GIMP inferior process is ready."
  (if (file-exists-p mode-icons--gimp)
      (or mode-icons--gimp-ready-p
          (let (buf)
            (mode-icons--start-gimp-inferior)
            (and (setq buf (get-buffer "*mode-icons-gimp*"))
                 (with-current-buffer buf
                   (goto-char (point-min))
                   (when (re-search-forward "ts>" nil t)
                     (setq mode-icons--gimp-ready-p t))))))))

(defvar mode-icons--stop-gimp-inferior nil)
(defun mode-icons--stop-gimp-inferior ()
  "Stop the inferior gimp process."
  (interactive)
  (when (file-exists-p mode-icons--gimp)
    (let ((buf (get-buffer "*mode-icons-gimp*")))
      (cond
     ((and (mode-icons--gimp-ready-p) buf
           (get-buffer-process buf))
      (mode-icons--process-gimp "(gimp-quit 0)")
      (setq mode-icons--gimp-ready-p nil
            mode-icons--stop-gimp-inferior t)
      (run-with-idle-timer 1 nil #'mode-icons--stop-gimp-inferior))
     ((and buf (not (get-buffer-process buf)))
      (kill-buffer (get-buffer "*mode-icons-gimp*")))
     (t (run-with-idle-timer 1 nil #'mode-icons--stop-gimp-inferior))))))

(defun mode-icons--process-gimp (scm)
  "Process gimp SCM (scheme)."
  (when mode-icons--stop-gimp-timer
    (cancel-timer mode-icons--stop-gimp-timer))
  (when (file-exists-p mode-icons--gimp)
    (if (mode-icons--gimp-ready-p)
        (progn
          (comint-send-string
           (with-current-buffer (get-buffer "*mode-icons-gimp*"))
           (concat scm "\n"))
          (when mode-icons--stop-gimp-after
            (setq mode-icons--stop-gimp-timer (run-with-timer mode-icons--stop-gimp-after nil #'mode-icons--stop-gimp-inferior))))
      (run-with-idle-timer 1 nil #'mode-icons--process-gimp scm))))

(defvar mode-icons--generic-type-to-xpm-gimp-script
  (replace-regexp-in-string
   "[ \n\t]+" " "
   "(let* ((image-width 1024)
       (image-height 20)
       (buffer-image 1)
       (text \"%s\")
       (font-size 20)
       (font-name \"FontAwesome\")
       (xpm-image \"%s\")
       (font-size-2 10)
       (text-2 \"%s\")
       (font-name-2 \"Haettenschweiler\")
       (bg-color '(255 255 255))
       (fg-color '(0 0 0))
       (image (car (gimp-image-new 1024 16 0)))
       (layer (car (gimp-layer-new image image-width image-height RGB-IMAGE \"layer 1\" 100 NORMAL)))
       (layer2 (car (gimp-layer-new image image-width image-height RGB-IMAGE \"layer 2\" 100 NORMAL)))
       (out-text)
       (out-width)
       (out-height)
       (out-buffer)
       (drawable))
  (gimp-image-add-layer image layer 0)
  (gimp-context-set-background bg-color)
  (gimp-context-set-foreground fg-color)
  (gimp-layer-add-alpha layer)
  (gimp-drawable-fill layer TRANSPARENT-FILL)
  (gimp-image-add-layer image layer2 0)
  (gimp-layer-add-alpha layer2)
  (gimp-drawable-fill layer2 TRANSPARENT-FILL)
  (gimp-text-fontname image layer2 3 7 text-2 0 TRUE font-size-2 PIXELS font-name-2)
  (set! out-text (car (gimp-text-fontname image layer 0 0 text 0 TRUE font-size PIXELS font-name)))
  (set! out-width (car (gimp-drawable-width out-text)))
  (set! out-height (car (gimp-drawable-height out-text)))
  (set! out-buffer (* out-height (/ buffer-image 100)))
  (set! out-height (+ out-height out-buffer out-buffer))
  (set! out-width (+ out-width  out-buffer out-buffer))
  (gimp-image-resize image out-width out-height 0 0)
  (gimp-layer-resize layer out-width out-height 0 0)
  (gimp-layer-set-offsets out-text out-buffer out-buffer)
  (gimp-image-flatten image)
  (set! drawable (car (gimp-image-get-active-layer image)))
  (file-xpm-save RUN-NONINTERACTIVE image drawable xpm-image xpm-image 127)
  (gimp-image-delete image))")
  "Generic Type script.")

(defvar mode-icons--font-to-xpm-gimp-script
  (replace-regexp-in-string
   "[ \n\t]+" " "
   "(let* ((image-width 1024)
       (image-height 20)
       (buffer-image 1)
       (text \"%s\")
       (font-size 20)
       (font-name \"%s\")
       (xpm-image \"%s\")
       (bg-color '(255 255 255))
       (fg-color '(0 0 0))
       (image (car (gimp-image-new 1024 16 0)))
       (layer (car (gimp-layer-new image image-width image-height RGB-IMAGE \"layer 1\" 100 NORMAL)))
       (out-text)
       (out-width)
       (out-height)
       (out-buffer)
       (drawable))
  (gimp-image-add-layer image layer 0)
  (gimp-context-set-background bg-color)
  (gimp-context-set-foreground fg-color)
  (gimp-layer-add-alpha layer)
  (gimp-drawable-fill layer TRANSPARENT-FILL)
  (set! out-text (car (gimp-text-fontname image layer 0 0 text 0 TRUE font-size PIXELS font-name)))

  (set! out-width (car (gimp-drawable-width out-text)))
  (set! out-height (car (gimp-drawable-height out-text)))
  (set! out-buffer (* out-height (/ buffer-image 100)))
  (set! out-height (+ out-height out-buffer out-buffer))
  (set! out-width (+ out-width  out-buffer out-buffer))
  (gimp-image-resize image out-width out-height 0 0)
  (gimp-layer-resize layer out-width out-height 0 0)
  (gimp-layer-set-offsets out-text out-buffer out-buffer)
  (gimp-image-flatten image)
  (set! drawable (car (gimp-image-get-active-layer image)))
  (file-xpm-save RUN-NONINTERACTIVE image drawable xpm-image xpm-image 127)
  (gimp-image-delete image))")
  "Gimp scheme script to convert a font character to xpm file.")

(defvar mode-icons--convert-ext-to-xpm (make-hash-table :test 'equal))
(defun mode-icons--convert-ext-to-xpm (ext)
  "Convert EXT to a xpm file."
  (let ((xpm (mode-icons-get-icon-file (concat "ext-" (downcase ext) ".xpm"))))
    (when (and mode-icons--gimp (file-exists-p mode-icons--gimp)
               xpm (not (gethash xpm mode-icons--convert-ext-to-xpm))
               (not (file-exists-p xpm)))
      (puthash xpm t mode-icons--convert-ext-to-xpm)
      (mode-icons--process-gimp
       (format mode-icons--generic-type-to-xpm-gimp-script (make-string 1 #xf016) xpm
               (downcase ext))))))

(defun mode-icons--ext-available-p (icon-spec)
  "Determine if ICON-SPEC's ext is availble for display.
If not, try `mode-icons--convert-ext-to-xpm'."
  (when (eq (nth 2 icon-spec) 'ext)
    (let ((xpm (mode-icons-get-icon-file (concat "ext-" (downcase (nth 1 icon-spec)) ".xpm"))))
      (if (file-readable-p xpm)
          xpm
        (mode-icons--convert-ext-to-xpm (nth 1 icon-spec))
        nil))))

(defcustom mode-icons-generate-font-grayscale nil
  "Generate grayscale images for font icons.
This is used instead of transparancy to capure the font's
anti-aliasing.  `mode-icons' will transform the colors to match
the background instead."
  :type 'boolean
  :group 'mode-icons)

(defvar mode-icons--convert-text-to-xpm (make-hash-table :test 'equal))
(defun mode-icons--convert-text-to-xpm (text font xpm &optional face height)
  "Convert TEXT in FONT to XPM file using gimp.

When FACE is non-nil, use the face background and foreground
properties to render the font (its no longer transparent).

When HEIGHT is non-nil, use the font HEIGHT (in pixels) instead
of 20px."
  (when (and mode-icons--gimp (file-exists-p mode-icons--gimp)
             xpm (not (gethash xpm mode-icons--convert-text-to-xpm))
             (not (file-exists-p xpm)))
    (puthash xpm t mode-icons--convert-text-to-xpm)
    (let ((script (format mode-icons--font-to-xpm-gimp-script text font xpm))
          (background (mode-icons-background-color face))
          (foreground (mode-icons-foreground-color face)))
      (when face
        (setq background (mapcar (lambda(x)
                                   (round (* 255 x))) background)
              foreground (mapcar (lambda(x)
                                   (round (* 255 x))) foreground))
        (setq script (replace-regexp-in-string
                      (regexp-quote "(bg-color '(255 255 255))")
                      (format "(bg-color '%s)" background)
                      script)
              script (replace-regexp-in-string
                      (regexp-quote "(fg-color '(0 0 0))")
                      (format "(fg-color '%s)" foreground)
                      script)
              script (replace-regexp-in-string
                      "TRANSPARENT-FILL" "BACKGROUND-FILL" script)
              script (replace-regexp-in-string
                      (regexp-quote "(gimp-layer-add-alpha layer)") "" script)))
      (when height
        (setq script (replace-regexp-in-string
                      (regexp-quote "(image-height 20)")
                      (format "(image-height %s)" background)
                      script)
              script (replace-regexp-in-string
                      (regexp-quote "(font-size 20)")
                      (format "(font-size %s)" background)
                      script)
              script (replace-regexp-in-string
                      "TRANSPARENT-FILL" "BACKGROUND-FILL" script)
              script (replace-regexp-in-string
                      (regexp-quote "(gimp-layer-add-alpha layer)") "" script)))
      (when mode-icons-generate-font-grayscale
        (setq script (replace-regexp-in-string
                      "TRANSPARENT-FILL" "BACKGROUND-FILL" script)
              script (replace-regexp-in-string
                      (regexp-quote "(gimp-layer-add-alpha layer)") "" script)))
      (mode-icons--process-gimp script))))

(defun mode-icons--get-font-xpm-file (icon-spec &optional icon-name)
  "Get the font icon equivalent xpm file name from ICON-SPEC.
When ICON-NAME is non-nil, return the small icon name without the
extension or directory."
  (let* ((xpm-int (or (and (stringp (nth 1 icon-spec))
                           (= 1 (length (nth 1 icon-spec)))
                           (aref (nth 1 icon-spec) 0))
                      (and (integerp (nth 1 icon-spec))
                           (nth 1 icon-spec))))
         (xpm-base (and (integerp xpm-int)
                        (format "%s-%x" (nth 2 icon-spec)
                                xpm-int))))
    (and xpm-base
         (if icon-name
             xpm-base
           (mode-icons-get-icon-file (concat xpm-base ".xpm"))))))

(defun mode-icons--create-font-xpm-file (icon-spec)
  "Create a font-based xpm file based on ICON-SPEC."
  (mode-icons--convert-text-to-xpm
   (or (and (stringp (nth 1 icon-spec))
            (nth 1 icon-spec))
       (and (integerp (nth 1 icon-spec))
            (make-string 1 (nth 1 icon-spec))))
   (symbol-name (nth 2 icon-spec))
   (mode-icons--get-font-xpm-file icon-spec)))

(defun mode-icons--convert-all-font-icons-to-xpm ()
  "Convert all font icons to xpm files."
  (interactive)
  (setq mode-icons--convert-text-to-xpm (make-hash-table :test 'equal))
  (dolist (icon-spec mode-icons)
    (when (mode-icons-supported-font-p (nth 1 icon-spec) (nth 2 icon-spec))
      (mode-icons--create-font-xpm-file icon-spec))))

(defvar mode-icons--png-to-xpm-gimp-script
  (replace-regexp-in-string
   "[ \n\t]+" " "
   "(let* ((png-image \"%s\")
       (xpm-image \"%s\")
       (image (car (file-png-load RUN-NONINTERACTIVE png-image png-image)))
       (drawable (car (gimp-image-get-active-layer image)))
       (width (car (gimp-image-width image)))
       (height (car (gimp-image-height image)))
       (new-height 16.0)
       (new-width (inexact->exact (round (* width (/ new-height height))))))
  (gimp-image-resize image 16 new-width 0 0)
  (set! drawable (car (gimp-image-get-active-layer image)))
  (file-xpm-save RUN-NONINTERACTIVE image drawable xpm-image xpm-image 127)
  (gimp-image-delete image))")
  "Gimp scheme script to convert png to xpm.")

(defvar mode-icons--convert-png-to-xpm (make-hash-table :test 'equal)
  "Hash table to make sure you only convert once.")

(defun mode-icons--convert-png-to-xpm (png xpm)
  "Covert PNG to a ?x16 XPM using `mode-icons--gimp'."
  (when (and mode-icons--gimp (file-exists-p mode-icons--gimp)
             xpm (not (gethash (list png xpm) mode-icons--convert-png-to-xpm))
             (not (file-exists-p xpm)))
    (puthash (list png xpm) t mode-icons--convert-png-to-xpm)
    (mode-icons--process-gimp (format mode-icons--png-to-xpm-gimp-script png xpm))))

(defun mode-icons--get-png-xpm-file (icon-spec &optional icon-name)
  "Get the png->xpm file name from ICON-SPEC.

When ICON-NAME is non-nil, return the mode-icons icon name."
  (if icon-name
      (nth 1 icon-spec)
    (mode-icons-get-icon-file (concat (nth 1 icon-spec) ".xpm"))))

(defun mode-icons--convert-all-png-icons-to-xpm ()
  "Convert all png icons to xpm files."
  (interactive)
  (setq mode-icons--convert-png-to-xpm (make-hash-table :test 'equal))
  (dolist (icon-spec mode-icons)
    (when (eq 'png (nth 2 icon-spec))
      (mode-icons--convert-png-to-xpm
       (mode-icons-get-icon-file (concat (nth 1 icon-spec) ".png"))
       (mode-icons-get-icon-file (concat (nth 1 icon-spec) ".xpm"))))))

(defun mode-icons--get-emoji-xpm-file (icon-spec &optional icon-name)
  "Get the emoji xpm file name from ICON-SPEC.
This only supports emoji enclosed in a \":\" like :herb:.

When ICON-NAME is non-nil, return the mode-icons icon name.
For :herb: it would be e-herb."
  (let* ((xpm-base (nth 1 icon-spec))
         file)
    (when (char-equal (aref xpm-base 0) ?:)
      (setq file (substring xpm-base 1))
      (when (char-equal (aref (substring xpm-base -1) 0) ?:)
        (setq file (substring file 0 -1))
        (if icon-name
            (concat "e-" file)
          (mode-icons-get-icon-file (concat "e-" file ".xpm")))))))

(defun mode-icons--get-png (mode icon-spec &optional face active)
  "Get MODE for png ICON-SPEC using FACE.
If possible, convert the png file to an xpm file.
ACTIVE is a flag telling if the current window is active."
  (let* ((xpm (mode-icons--get-png-xpm-file icon-spec))
         (xpm-name (mode-icons--get-png-xpm-file icon-spec t))
         (xpm-p (file-readable-p xpm))
         (png (mode-icons-get-icon-file (concat (nth 1 icon-spec) ".png")))
         (png-p (file-readable-p png))
         (face (mode-icons--get-face face active)))
    (if xpm-p
        (propertize (format "%s" mode) 'display
                    (mode-icons-get-icon-display
                     xpm-name 'xpm
                     face active)
                    'face face
                    'mode-icons-p (list (nth 0 icon-spec) xpm-name 'xpm))
      (if (not png-p)
          (propertize (format "%s" mode)
                      'face face
                      'mode-icons-p icon-spec)
        (mode-icons--convert-png-to-xpm png xpm)
        (propertize (format "%s" mode)
                      'display
                      (create-image png
                                    ;; use imagemagick if available and supports PNG images
                                    ;; (allows resizing images)
                                    (or (and (and (fboundp 'imagemagick-types)
                                                  (memq 'png (imagemagick-types)))
                                             'imagemagick) 'png)
                                    nil
                                    :height (mode-icons-line-height)
                                    :ascent 'center
                                    :heuristic-mask t
                                    :face face)
                      'face face
                      'mode-icons-p icon-spec)))))

(defcustom mode-icons-prefer-xpm-over-emoji nil
  "Prefer generated xpms over fonts.
If mode-icons has a generated font character, prefer that over
the actual font."
  :type 'boolean
  :group 'mode-icons)

(defcustom mode-icons-generate-emoji-xpms nil
  "Generate font compatibility xpms for fonts."
  :type 'boolean
  :group 'mode-icons)

(defun mode-icons--get-emoji (mode icon-spec &optional face active)
  "Get MODE emoji for ICON-SPEC using FACE.
ACTIVE is a flag for if  the current window is active."
  (let* ((xpm (mode-icons--get-emoji-xpm-file icon-spec))
         (xpm-name (mode-icons--get-emoji-xpm-file icon-spec t))
         (xpm-p (file-readable-p xpm))
         (face (mode-icons--get-face face active)))
    (if (or (and mode-icons-prefer-xpm-over-emoji xpm-p)
            (and xpm-p (not (featurep 'emojify)))
            (and xpm-p (not (image-type-available-p 'png))))
        (propertize (format "%s" mode) 'display
                    (mode-icons-get-icon-display
                     xpm-name 'xpm face active)
                    'mode-icons-p (list (nth 0 icon-spec) xpm-name 'xpm))
      (unless emojify-emojis
        (emojify-set-emoji-data))
      (let* ((emoji (ht-get emojify-emojis (nth 1 icon-spec)))
             (image-file (expand-file-name (ht-get emoji "image") (if (fboundp 'emojify-image-dir)
                                                                      (emojify-image-dir)
                                                                    emojify-image-dir)))
             (image-type (intern (upcase (file-name-extension image-file)))))
        (if (not (file-exists-p image-file))
            (propertize (format "%s" mode)
                        'face face
                        'mode-icons-p icon-spec)
          (when mode-icons-generate-emoji-xpms
            (mode-icons--convert-png-to-xpm image-file xpm))
          (propertize (format "%s" mode)
                      'display
                      (create-image image-file
                                    ;; use imagemagick if available and supports PNG images
                                    ;; (allows resizing images)
                                    (or (and (and (fboundp 'imagemagick-types)
                                                  (memq image-type (imagemagick-types)))
                                             'imagemagick) 'png)
                                    nil
                                    :ascent 'center
                                    :heuristic-mask t
                                    :face face
                                    ;; :background (emojify--get-image-background beg end)
                                    ;; no-op if imagemagick is not available
                                    :height (mode-icons-line-height))
                      'face face
                      'mode-icons-p icon-spec))))))

(defcustom mode-icons-prefer-xpm-over-font nil
  "Prefer generated xpms over fonts.
If mode-icons has a generated font character, prefer that over
the actual font."
  :type 'boolean
  :group 'mode-icons)

(defcustom mode-icons-generate-font-xpms nil
  "Generate font compatibility xpms for fonts."
  :type 'boolean
  :group 'mode-icons)

(defun mode-icons--get-font (mode icon-spec &optional face active)
  "Get font for MODE based on ICON-SPEC, and FACE.
ACTIVE if a flag for if the current window is active."
  ;; Use `compose-region' because it allows clickable text.
  (let* ((xpm (mode-icons--get-font-xpm-file icon-spec))
         (xpm-name (mode-icons--get-font-xpm-file icon-spec t))
         (xpm-p (file-readable-p xpm))
         (face (mode-icons--get-face face active)))
    (when (and (not xpm-p) mode-icons-generate-font-xpms)
      (mode-icons--create-font-xpm-file icon-spec))
    (if (and xpm-p (or mode-icons-prefer-xpm-over-font
                       (not (mode-icons-supported-font-p (nth 1 icon-spec) (nth 2 icon-spec)))))
        (propertize (format "%s" mode) 'display
                    (mode-icons-get-icon-display
                     xpm-name 'xpm face active)
                    'mode-icons-p (list (nth 0 icon-spec) xpm-name 'xpm-bw)
                    'face face)
      (mode-icons-save-buffer-state
       (with-temp-buffer
         (if (stringp mode)
             (insert mode)
           (insert (or (and (integerp (nth 1 icon-spec))
                            (make-string 1 (nth 1 icon-spec)))
                       (nth 1 icon-spec))))
         (compose-region (point-min) (point-max) (or (and (integerp (nth 1 icon-spec))
                                                          (make-string 1 (nth 1 icon-spec)))
                                                     (nth 1 icon-spec)))
         (put-text-property (point-min) (point-max)
                            'face face)
         (if (mode-icons-supported-font-p (nth 1 icon-spec) (nth 2 icon-spec))
             (put-text-property (point-min) (point-max)
                                'mode-icons-p icon-spec)
           (put-text-property (point-min) (point-max)
                              'mode-icons-p (list (nth 0 icon-spec) xpm-name 'xpm-bw)))
         (buffer-string))))))

(defun mode-icons-propertize-mode (mode icon-spec &optional face active)
  "Propertize MODE with ICON-SPEC.

MODE should be a string, the name of the mode to propertize.
ICON-SPEC should be a specification from `mode-icons'.
FACE is the face to match when a xpm-bw image is used.
ACTIVE is a flag to tell if the current window is active."
  (let (tmp new-icon-spec)
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
       ;;(mode-icons--get-font " AI" '("\\` ?AI\\'" 61500 FontAwesome) face active)
       (mode-icons--get-font mode icon-spec face active))
      ((and (stringp (nth 1 icon-spec)) (eq (nth 2 icon-spec) 'emoji))
       (mode-icons--get-emoji mode icon-spec face active))
      ((and (stringp (nth 1 icon-spec)) (eq (nth 2 icon-spec) 'png))
       (mode-icons--get-png mode icon-spec face active))
      ((and (stringp (nth 1 icon-spec)) (eq (nth 2 icon-spec) 'ext))
       (propertize (format "%s" mode) 'display
                   (mode-icons-get-icon-display
                    (concat "ext-" (nth 1 icon-spec)) 'xpm-bw face active)
                   'mode-icons-p (list (nth 0 icon-spec)
                                       (concat "ext-" (nth 1 icon-spec))
                                       'xpm-bw)))
      (t (setq tmp (mode-icons-get-icon-display (nth 1 icon-spec) (nth 2 icon-spec) face active))
         ;; (when (string= (nth 0 icon-spec) "\\` ?AI\\'")
         ;;   (message "plist: %s" tmp))
         (cond
          ((and (plist-get (cdr tmp) :xpm-bw) (plist-get (cdr tmp) :icon))
           (setq new-icon-spec (list (nth 0 icon-spec) (plist-get (cdr tmp) :icon) 'xpm-bw)))
          ((and (eq (plist-get (cdr tmp) :type) 'xpm) (plist-get (cdr tmp) :icon))
           (setq new-icon-spec (list (nth 0 icon-spec) (plist-get (cdr tmp) :icon) 'xpm)))
          (t (setq new-icon-spec icon-spec)))
         (propertize (format "%s" mode) 'display tmp
                     'mode-icons-p new-icon-spec))))))

(defvar mode-icons-get-icon-spec (make-hash-table :test 'equal)
  "Hash table of icon-specifications.")
(defun mode-icons-get-icon-spec (mode &optional is-major-mode-p)
  "Get icon spec for MODE based on regular expression."
  (or (gethash mode mode-icons-get-icon-spec)
      (let* (case-fold-search
             (ignore-cache nil)
             (icon-spec (catch 'found-mode
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
                                         (eq mode (car item)))
                                        (and
                                         is-major-mode-p
                                         (symbolp (car item))
                                         (functionp (car item))
                                         (and (ignore-errors (funcall (car item)))
                                              (setq ignore-cache t)))))
                              (throw 'found-mode item)))
                          nil)))
        (when (and icon-spec (eq (nth 2 icon-spec) 'emoji)
                   (file-exists-p (mode-icons--get-emoji-xpm-file icon-spec)))
          (setq icon-spec (list (nth 0 icon-spec) (mode-icons--get-emoji-xpm-file icon-spec t) 'xpm)))
        (unless ignore-cache
          (puthash mode icon-spec mode-icons-get-icon-spec))
        icon-spec)))

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

(defcustom mode-icons-use-default-icon nil
  "Use the 'default icon when icon-name cannot be found."
  :type 'boolean
  :group 'mode-icons)

(defun mode-icons-get-mode-icon (mode &optional face active)
  "Get the icon for MODE, if there is one.
FACE represents the face used when the icon is a xpm-bw image.
ACTIVE represents if the window is active."
  (let* ((mode-name (format-mode-line mode))
         (icon-spec (mode-icons-get-icon-spec mode-name t))
         (face (mode-icons--get-face face active))
         ret)
    (when (and (not icon-spec) mode-icons-use-default-icon)
      (setq icon-spec (mode-icons-get-icon-spec 'default)))
    (if icon-spec
        (setq ret
              (if mode-icons-show-mode-name
                      (concat (mode-icons-propertize-mode mode-name icon-spec face active) " " mode-name)
                    (mode-icons-propertize-mode mode-name icon-spec face active)))
      (setq ret mode-name))
    ;; Don't hide major mode names...
    (when (string= ret "")
      (setq ret mode-name))
    ret))

(defvar mode-icons-cached-mode-name nil
  "Cached mode name to restore when disabling mode-icons.")

(defvar mode-icons--mode-name nil
  "Mode name displayed by mode-icons.")

(defun mode-icons-set-mode-icon (mode)
  "Set the icon for MODE."
  (unless mode-icons-cached-mode-name
    (set (make-local-variable 'mode-icons-cached-mode-name)
         mode-name)
    (set (make-local-variable 'mode-icons--mode-name)
         (mode-icons-get-mode-icon mode nil t))
    (when mode-icons-change-mode-name
      (setq mode-name mode-icons--mode-name))))

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
  (let (icon-spec mode-name minor cur-mode)
    (dolist (mode minor-mode-alist)
      (setq cur-mode
            (or (assq (car mode) mode-icons-set-minor-mode-icon-alist)
                mode))
      (setq mode-name (format-mode-line cur-mode)
            icon-spec (mode-icons-get-icon-spec mode-name))
      (when icon-spec
          (setq minor (assq (car cur-mode) minor-mode-alist))
          (when minor
            (or (assq (car cur-mode) mode-icons-set-minor-mode-icon-alist)
                (push (copy-sequence minor) mode-icons-set-minor-mode-icon-alist))
            (setq mode-name (replace-regexp-in-string "^ " "" mode-name)
                  mode-name (mode-icons-propertize-mode mode-name icon-spec))
            (if (string= "" mode-name)
                (setcdr minor (list ""))
              (setcdr minor (list (concat (or (and mode-icons-separate-images-with-spaces " ") "")
                                          mode-name))))))))
  (unless dont-update
    (force-mode-line-update)))

(defun mode-icons--generate-major-mode-item (&optional face)
  "Give rich strings needed for `major-mode' viewing.
FACE is the face that the major mode item should be rendered in."
  (let* ((active (mode-icons--selected-window-active))
         (face (mode-icons--get-face face active)))
    (eval `(propertize ,(mode-icons--recolor-string (or mode-icons--mode-name mode-name) active face)
                       'face ',face
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
;; (add-hook 'focus-in-hook 'mode-icons--set-selected-window)

;; focus-out-hook was introduced in emacs v24.4.
;; (add-hook 'focus-out-hook 'mode-icons--unset-selected-window)

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

(defun mode-icons--property-substrings (str prop)
  "Return a list of substrings of STR when PROP change."
  ;; Taken from powerline by Donald Ephraim Curtis, Jason Milkins and
  ;; Nicolas Rougier
  (let ((beg 0) (end 0)
        (len (length str))
        (out))
    (while (< end (length str))
      (setq end (or (next-single-property-change beg prop str) len))
      (setq out (append out (list (substring str beg (setq beg end))))))
    out))

(defun mode-icons--recolor-string (string &optional active face)
  "Recolor `mode-icons' in STRING.
ACTIVE tells if the current window is active.
FACE is the face to recolor the icon to."
  (let* ((face (mode-icons--get-face face active))
         icon-spec)
    (mapconcat
     (lambda(str)
       (cond
        ((get-text-property 0 'display str)
         (mode-icons--recolor-minor-mode-image str active face))
        ((and (setq icon-spec (get-text-property 0 'mode-icons-p str))
              (mode-icons-supported-font-p (nth 1 icon-spec) (nth 2 icon-spec)))
         (mode-icons--get-font str icon-spec face active))
        (t
         str)))
     (mode-icons--property-substrings string 'mode-icons-p)
     "")))

(defun mode-icons--recolor-minor-mode-image (mode active &optional face)
  "Recolor MODE image based on if the window is ACTIVE.
Use FACE when specified."
  (let ((icon-spec (get-text-property 0 'mode-icons-p mode))
        (face (mode-icons--get-face face active)))
    (cond
     ((and icon-spec (memq (nth 2 icon-spec) '(xpm xpm-bw)))
      (propertize mode 'display (mode-icons-get-icon-display
                                 (nth 1 icon-spec) (nth 2 icon-spec) face active)
                  'face face
                  'mode-icons-p icon-spec))
     ((and icon-spec (memq (nth 2 icon-spec) '(emoji))
           (file-exists-p (mode-icons--get-emoji-xpm-file icon-spec)))
      (propertize mode 'display (mode-icons-get-icon-display
                                 (mode-icons--get-emoji-xpm-file icon-spec t)
                                 'xpm face active) 'face face
                                 'mode-icons-p icon-spec))
     (t (propertize mode 'face face)))))

(defun mode-icons--generate-minor-mode-list (&optional face)
  "Extracts all rich strings necessary for the minor mode list.
When FACE is non-nil, use FACE to render the `minor-mode-alist'."
  (let* ((active (mode-icons--selected-window-active))
         (face (mode-icons--get-face face active)))
    (delete " " (delete "" (mapcar (lambda(mode)
                                     (concat " " (eval `(propertize ,(mode-icons--recolor-minor-mode-image mode active face)
                                                                    ,@mode-icons-minor-mode-base-text-properties))))
                                   (split-string (format-mode-line minor-mode-alist)))))))

(defun mode-icons--generate-narrow (&optional face)
  "Extracts all rich strings necessary for narrow indicator.
When FACE is non-nil, use FACE to render the narrow indicator."
  (let* ((active (mode-icons--selected-window-active))
         (face (mode-icons--get-face active face))
        icon-spec)
    (delete " " (delete "" (mapcar (lambda(mode)
                                     (concat " " (eval `(propertize
                                                         ,(if (setq icon-spec (mode-icons-get-icon-spec (concat " " mode)))
                                                              (mode-icons--recolor-minor-mode-image
                                                               (mode-icons-propertize-mode (concat " " mode) icon-spec face active)
                                                               active face)
                                                            mode)
                                                         ,@mode-icons-narrow-text-properties))))
                                   (split-string (format-mode-line "%n")))))))


(defcustom mode-icons-read-only-space t
  "Add Space after read-only icon."
  :type 'boolean
  :group 'mode-icons)

(defun mode-icons--read-only-status (&optional face)
  "Get Read Only Status icon.
FACE is the face to render the icon in."
  (let ((active (mode-icons--selected-window-active)))
    (eval `(propertize
            ,(let ((ro (format-mode-line "%1*"))
                    icon-spec)
                (setq ro (or (cond
                              ((string= "%" ro)
                               (if (setq icon-spec (mode-icons-get-icon-spec 'read-only))
                                   (mode-icons-propertize-mode 'read-only icon-spec face active)
                                 ro))
                              (t
                               (if (setq icon-spec (mode-icons-get-icon-spec 'writable))
                                   (mode-icons-propertize-mode 'writable icon-spec face active)
                                 ro)))
                             "")
                      ro (mode-icons--recolor-minor-mode-image ro active face))
                (when (and mode-icons-read-only-space
                           (not (string= ro "")))
                  (setq ro (concat ro " ")))
                ro)
            ,@mode-icons-read-only-text-properties))))

(defcustom mode-icons-modified-status-space t
  "Add Space to modified status."
  :type 'boolean
  :group 'mode-icons)

(defun mode-icons--modified-status (&optional face)
  "Get modified status icon.
FACE is the face to render the icon in."
  (let ((active (mode-icons--selected-window-active)))
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
                                          (mode-icons-propertize-mode 'steal icon-spec face active)
                                        mod))
                                     ((char-equal ?! (aref mod 0))
                                      (if (setq icon-spec (mode-icons-get-icon-spec 'modified-outside))
                                          (mode-icons-propertize-mode 'modified-outside icon-spec face active)
                                        mod))
                                     ((char-equal ?* (aref mod 0))
                                      (if (setq icon-spec (mode-icons-get-icon-spec 'save))
                                          (mode-icons-propertize-mode 'save icon-spec face active)
                                        mod))
                                     (t
                                      (if (setq icon-spec (mode-icons-get-icon-spec 'saved))
                                          (mode-icons-propertize-mode 'saved icon-spec face active)
                                        mod)))
                                    ""))
                      (setq mod (mode-icons--recolor-minor-mode-image mod active face))
                      (when (and mode-icons-modified-status-space
                                 (stringp mod)
                                 (not (string= mod "")))
                        (setq mod (concat mod " ")))
                      mod)) "")
            ,@mode-icons-modified-text-properties))))

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

(defun mode-icons--mode-line-eol-desc (&optional string face)
  "Modify `mode-line-eol-desc' to have icons.

STRING is the string to modify, or if absent, the value from
`mode-line-eol-desc'.

FACE is the face that will be used to render the segment."
  (let* ((str (or string (mode-line-eol-desc)))
         (props (text-properties-at 0 str))
         (lt2 "")
         (active (mode-icons--selected-window-active))
         icon-spec)
    (setq str (or (cond
                   ((string= "(Unix)" str)
                    (setq lt2 " LF")
                    (if (setq icon-spec (mode-icons-get-icon-spec 'unix))
                        (mode-icons-propertize-mode 'unix icon-spec face active)
                      str))
                   ((or (string= str "(DOS)")
                        (string= str "\\"))
                    (setq lt2 " CRLF")
                    (if (setq icon-spec (mode-icons-get-icon-spec 'win))
                        (mode-icons-propertize-mode 'win icon-spec face active)
                      str))
                   ((string= str "(Mac)")
                    (setq lt2 " CR")
                    (if (setq icon-spec (mode-icons-get-icon-spec 'apple))
                        (mode-icons-propertize-mode 'apple icon-spec face active)
                      str))
                   ((string= str ":")
                    (setq lt2 " Undecided")
                    (if (setq icon-spec (mode-icons-get-icon-spec 'undecided))
                        (mode-icons-propertize-mode 'undecided icon-spec face active)
                      str))
                   (t str))
                  ""))
    (setq str (mode-icons--recolor-minor-mode-image str active face))
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

(defun mode-icons-reset-hash ()
  "Reset `mode-icons-get-icon-spec' and `mode-icons-get-icon-display'."
  (interactive)
  (setq mode-icons-get-icon-spec (make-hash-table :test 'equal)
        mode-icons-get-icon-display (make-hash-table :test 'equal)))

(defun mode-icons-reset ()
  "Reset mode-icons icons."
  (interactive)
  (when (and mode-icons-mode (not (minibufferp)))
    ;; Reset the major mode now.
    (mode-icons-set-current-mode-icon)
    ;; Reset the minor mode later, in case the mode turns on some
    ;; minor-modes.
    (run-with-idle-timer
     0.1 nil `(lambda()
                ;; Reset the minor mode icons
                (when (buffer-live-p ,(current-buffer))
                  (with-current-buffer ,(current-buffer)
                    (mode-icons-set-minor-mode-icon)))))))

(add-hook 'emacs-startup-hook #'mode-icons-reset)

(defadvice isearch-mode (after mode-icons--reset-isearch-icon activate)
  "Make `mode-icons' aware of icon."
  (mode-icons-set-minor-mode-icon))

(eval-after-load 'powerline
  '(progn
     (declare-function mode-icons--real-powerline-minor-modes "powerline")
     (fset 'mode-icons--real-powerline-minor-modes #'powerline-minor-modes)
     (defun mode-icons--powerline-minor-modes (&optional face pad)
       "Powerline minor modes is replaced by this function.
FACE is the face to use.
PAD is the padding around the minor modes.

The original is called if `mode-icons-mode' is disabled.  It is
saved in `mode-icons--real-powerline-minor-modes'."
       (if mode-icons-mode
           (mode-icons--generate-minor-mode-list face)
         (mode-icons--real-powerline-minor-modes face pad)))
     (fset 'mode-icons--real-powerline-major-mode #'powerline-minor-modes)
     (defun mode-icons--powerline-major-mode (&optional face pad)
       "Powerline major modes is replaced by this function.
FACE is the face to use.
PAD is the padding around the minor modes.

The original is called if `mode-icons-mode' is disabled.  It is
saved in `mode-icons--real-powerline-major-mode'."
       (if mode-icons-mode
           (powerline-raw (format-mode-line (mode-icons--generate-major-mode-item face) face) face pad)
         (mode-icons--real-powerline-major-mode face pad)))
     (fset 'powerline-major-mode #'mode-icons--powerline-major-mode)
     (fset 'mode-icons--real-powerline-raw #'powerline-raw)
     (defun mode-icons--powerline-raw (str &optional face pad)
       "Render STR as mode-line data using FACE and optionally PAD import on left (l) or right (r).
This uses `mode-icons--recolor-string' when `mode-icons-mode' is enabled."
       (if mode-icons-mode
           (when str
             (let* ((rendered-str (format-mode-line str))
                    (padded-str (concat
                                 (when (and (> (length rendered-str) 0) (eq pad 'l)) " ")
                                 (if (listp str) rendered-str str)
                                 (when (and (> (length rendered-str) 0) (eq pad 'r)) " "))))
               (if face
                   (mode-icons--recolor-string (pl/add-text-property padded-str 'face face)
                                               (mode-icons--selected-window-active) face)
                 padded-str)))
         (mode-icons--real-powerline-raw str face pad)))
     (fset 'powerline-raw #'mode-icons--powerline-raw)))


(eval-after-load 'emojify
  '(progn
     (mode-icons-reset-hash)))

(provide 'mode-icons)
;;; mode-icons.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
