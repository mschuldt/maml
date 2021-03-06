#!/bin/sh
":"; exec emacs --quick --script "$0" "$@" #

(setq files '("_opcodes.h"
              "primitives.c"
              "_prim.c"
              "_entrytable.h"
              "arduino_only_primitives.c"
              "maml_HardwareSerial.cpp"))

(find-file "avm.c")
(re-search-forward "#define +ARDUINO +\\(0\\)")
(replace-match "1" nil nil nil 1)
(goto-char 1)
(insert "/***** This file is auto-generated, do not modify. *****/\n\n")
(dolist (f files)
  (goto-char 1)
  (re-search-forward (format "#include +\"%s\"" f))
  (beginning-of-line)
  (kill-line)
  (insert-file-contents-literally f)
  (message "inserted %s" f))

(write-file "avm.ino")
