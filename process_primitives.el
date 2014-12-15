#!/usr/bin/emacs --script

;;TODO: should probably rewrite this in Python

;;input: primitives.c
;;output: _prim.c, _prim.py

(setq primitives_files '("primitives.c" "avm.c")
      non_arduino_files '("non_arduino_primitives.c")
      c_out "_prim.c"
      py_out "_prim.py"
      token "_DEFUN_"
      re (format"^%s[ \n\t]+\\([a-zA-Z0-9\*_]+[ \t\n]+\\)+\\([a-zA-Z0-9\*_]+\\)[ \n\t]*(" token))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process_primitives (file)
  (let (names)
    (find-file file)
    (while (re-search-forward  re nil :noerror)
      (setq names (cons (match-string 2) names)))
    names))

(defun write_py (names &optional start indent)
  (let ((c (or start 0)))
    (while names
      (insert (format "primitives['%s'] = %s\n" (car names) c))
      (setq c (1+ c)
            names (cdr names)))))

(defun write_c (names &optional start)
  (let ((c (or start 0)))
    (while names
      (insert (format "primitives[%s] = (void*)&%s;\n" c (car names)))
      (setq c (1+ c)
            names (cdr names)))))

(setq names nil
      na_names nil)
(dolist (file primitives_files)
  (setq names (append (process_primitives file) names)))
(dolist (file non_arduino_files)
  (setq na_names (append (process_primitives file) na_names)))
(setq l_names (length names)
      l_na_names (length na_names))

(find-file py_out)
(erase-buffer)
(insert "###### This file is auto-generated, do not modify. #####

primitives = {}\n")
(write_py names)
;;TODO: need some variable that is set when not compiling for arduino
(write_py na_names l_names t)
(save-buffer)


(find-file c_out)
(erase-buffer)
(insert (format "/***** This file is auto-generated, do not modify. *****/

#if arduino
n_primitives = %s;
#else
n_primitives = %s;
#endif
primitives = (void**)malloc(sizeof(void*)*n_primitives);
" l_names (+ l_names l_na_names)))
(write_c names)
(insert "#if ! arduino\n")
(write_c na_names l_names)
(insert "#endif\n")
(save-buffer)
