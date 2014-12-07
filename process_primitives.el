#!/usr/bin/emacs --script

;;TODO: should probably rewrite this in Python

;;input: primitives.c
;;output: _prim.c, _prim.py

(setq primitives_file "primitives.c"
      c_out "_prim.c"
      py_out "_prim.py"
      token "_PRIMITIVE_"
      re (concat token "[ \n\t]+\\([a-zA-Z0-9_]+[ \t\n]+\\)+\\([a-zA-Z0-9_]+\\)[ \n\t]*("))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq names nil)

(defun process_primitives ()
  (find-file primitives_file)
  (while (re-search-forward  re nil :noerror)
    (setq names (cons (match-string 2) names))))


(defun write_py ()
  (find-file py_out)
  (erase-buffer)
  (insert "primitives = {}\n")
  (let ((c 0)
        (p names))
    (while p
      (insert (format "primitives['%s'] = %s\n" (car p) c))
      (setq c (1+ c)
            p (cdr p))))
  (save-buffer))


(defun write_c ()
  (find-file c_out)
  (erase-buffer)
  (let ((c 0)
        (p names)
        (l (length names)))
    (insert (format "primitives = malloc(sizeof(void*)*%s);\n" (length names)))
    (insert (format "n_primitives = %s;\n" l))
    (while p
      (insert (format "primitives[%s] = &%s;\n" c (car p)))
      (setq c (1+ c)
            p (cdr p))))
  (save-buffer))

(process_primitives)
(write_py)
(write_c)

