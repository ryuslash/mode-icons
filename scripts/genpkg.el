(require 'package)

(find-file "mode-icons.el")
(let ((info (package-buffer-info)))
  (with-current-buffer (get-buffer-create "pkg")
    (insert "(define-package \"" (elt info 0) "\"\n"
            "                \"" (elt info 3) "\"\n"
            "                \"" (elt info 2) "\")")
    (princ (buffer-string))))
