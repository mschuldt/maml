#!/usr/bin/emacs --script

;;TODO: should probably rewrite this in Python

;;input: primitives.c
;;output: _prim.c, _prim.py

(setq primitives_files '("primitives.c" "avm.c")
      desktop_files '("desktop_only_primitives.c")
      arduino_files '("arduino_only_primitives.c")
      c_out "_prim.c"
      py_out "_prim.py"
      token "_DEFUN_"
      function_re (format"^%s[ \n\t]+\\([a-zA-Z0-9\*_]+[ \t\n]+\\)+\\([a-zA-Z0-9\*_]+\\)[ \n\t]*(" token)
      declaration_re "^[ \t]*_DECL_(\\([A-Za-z][A-Za-z0-9]*\\).*)[ \t]*$"
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process_primitives (file)
  (let (names)
    (find-file file)
    ;;TODO: extract/process types and parameter list length
    (while (re-search-forward function_re nil :noerror)
      (setq names (cons (match-string 2) names)))
    (goto-char 1)
    (while (re-search-forward declaration_re nil :noerror)
      (setq names (cons (match-string 1) names)))
    names))

(defun write_py (names &optional target start)
  (let ((c (or start 0)))
    (while names
      (if (null target)
          (insert (format "desktop_primitives['%s'] = %s
arduino_primitives['%s'] = %s\n" (car names) c (car names) c))
        (insert (format "%s_primitives['%s'] = %s\n"
                        (if (eq target :desktop)
                            "desktop"
                          "arduino")
                        (car names) c)))
      (setq c (1+ c)
            names (cdr names)))))

(defun write_c (names &optional start)
  (let ((c (or start 0)))
    (while names
      (insert (format "primitives[%s] = (void*)&%s;\n" c (car names)))
      (setq c (1+ c)
            names (cdr names)))))

(setq names nil
      desktop_names nil
      arduino_names nil)

(dolist (file primitives_files)
  (setq names (append (process_primitives file) names)))
(dolist (file desktop_files)
  (setq desktop_names (append (process_primitives file) desktop_names)))
(dolist (file arduino_files)
  (setq arduino_names (append (process_primitives file) arduino_names)))
(setq l_names (length names)
      l_arduino_names (length arduino_names)
      l_desktop_names (length desktop_names))

(find-file py_out)
(erase-buffer)
(insert "###### This file is auto-generated, do not modify. #####

desktop_primitives = {}
arduino_primitives = {}\n")
(write_py names)
;;TODO: need some variable that is set when not compiling for arduino
(write_py desktop_names :desktop l_names)
(write_py arduino_names :arduino l_names)
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
" (+ l_names l_arduino_names) (+ l_names l_desktop_names)))
(write_c names)
(insert "#if  arduino\n")
(write_c arduino_names l_names)
(insert "#else\n")
(write_c desktop_names l_names)
(insert "#endif\n")
(save-buffer)
