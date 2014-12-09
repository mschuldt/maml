#!/usr/bin/emacs --script

(setq files '("_opcodes.h"
              "primitives.c"
              "_prim.c"))

(find-file "avm.c")
(dolist (f files)
  (goto-char 1)
  (while (re-search-forward (format "#include +\"%s\"" f) nil t)
    (beginning-of-line)
    (kill-line)
    (insert-file-contents-literally f)
    (message "inserted %s" f)))
(write-file "avm.ino")
