#!/bin/sh
":"; exec emacs --quick --script "$0" "$@" #

;;#!/usr/bin/emacs --script

;;input: primitives.c
;;       avm.c
;;       desktop_only_primitives.c
;;       arduino_only_primitives.c
;;output: _prim.c
;;        _prim.py

(setq primitives_files '("primitives.c" "avm.c")
      desktop_files '("desktop_only_primitives.c")
      arduino_files '("arduino_only_primitives.c")
      c_out "_prim.c"
      py_out "_prim.py"
      function_re "^[ \t]*_DEFUN_[ \t]*(\\(.*\\))[ \t\n]*[a-zA-Z0-9\*_ ]+[ \t\n]+\\([a-zA-Z0-9\*_]+\\)[ \n\t]*("
      declaration_re "^[ \t]*_DECL_(\\([A-Za-z][A-Za-z0-9]*\\),?\\(.*\\))[ \t]*$"
      valid_types '("int" "float" "str" "list" "any") ;;TODO: others
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-type (str)
  ;;TODO: fix: this should not be valid: "]arg["
  (dolist (x '(":" "->"))
    (if (string-match x str)
        (setq str (s-trim (car (last (split-string str x)))))))
  (let ((start 0)
        (end (1- (length str)))
        (s ?\[)
        (e ?\])
        (done nil)
        ok)

    (while (and (< start end) (not done))
      (setq s (aref str start)
            e (aref str end)
            start (1+ start)
            end (1- end))
      (if (not (cond ((eq s ?\[)
                      (eq e ?\]))
                     ((eq s ?\()
                      (eq e ?\)))
                     (t (if (not (or (eq e ?\])
                                     (eq e ?\))))
                            (setq ok t))
                        nil)))
          (setq done t)))
    (if ok str
      (message "SyntaxError: invalid parameter type")
      ;;(kill-emacs 1)
      )))


(defsubst return-type-p (str)
  (string-match "->" str))

(defun parse-param-info (str name)
  "types are comma seporated values
only the last word in each group is the type, this allows for including arg names.
If STR is the empty string, no parameters with return type None.

if the last value is preceded by '->' then it is the return type, default: None
examples:
   'int, int'
   'int, str, -> str'
   'n:int,x: str, -> list'
   ''
   'n : int, str'
return format: (list-of-arg-types . return-type)
"
  (let (arg-types
        type
        ret)
    (dolist (arg (mapcar 's-trim (split-string str "," t)))
      (setq type (extract-type arg))
      (if (return-type-p arg)
          (if ret
              (progn
                (message "SyntaxError: Invalid type declaration for '%s'" name)
                ;;(kill-emacs 1)
                )
            (setq ret type))
        (setq arg-types (cons type arg-types))))
    (cons (to-py-list (reverse arg-types)) (py-str (or ret "none")))))

(defun py-str (x)
  (format "'%s'" x))

(defun to-py-list (lst)
  (format "[%s]" (mapconcat (lambda (x) (py-str x)) lst ", ")))

(defun process_primitives (file)
  (let (names name)
    (find-file file)
    ;;TODO: extract/process types and parameter list length
    (while (re-search-forward function_re nil :noerror)
      (setq name (match-string 2)
            type-info (parse-param-info (match-string 1) name)
            names (cons (cons name type-info) names)))
    (goto-char 1)
    (while (re-search-forward declaration_re nil :noerror)
      (setq name (match-string 1)
            type-info (parse-param-info (match-string 2) name)
            names (cons (cons name type-info) names)))
    names))

(defun write_py (names &optional target start)
  (let ((c (or start 0))
        name info)
    (while names
      (setq name (car (car names))
            info (cdr (car names))
            info (format "prim(%s, %s, %s)" c (car info) (cdr info)))
      (if (null target)
          (insert (format "desktop_primitives['%s'] = %s
arduino_primitives['%s'] = %s\n" name info name info))
        (insert (format "%s_primitives['%s'] = %s\n"
                        (if (eq target :desktop)
                            "desktop"
                          "arduino")
                        name info)))
      (setq c (1+ c)
            names (cdr names)))))

(defun write_c (names &optional start)
  (let ((c (or start 0)))
    (while names
      (insert (format "primitives[%s] = (void*)&%s;\n" c (car (car names))))
      (setq c (1+ c)
            names (cdr names)))))

;;string trimming functions from  s.el
(defun s-trim-left (s)
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s))
(defun s-trim-right (s)
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))
(defun s-trim (s)
  (s-trim-left (s-trim-right s)))


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
arduino_primitives = {}

class prim:
    def __init__(self, index, args, ret):
        self.index = index
        self.args = args
        self.ret = ret

")
(write_py names)
;;TODO: need some variable that is set when not compiling for arduino
(write_py desktop_names :desktop l_names)
(write_py arduino_names :arduino l_names)
(save-buffer)


(find-file c_out)
(erase-buffer)
(insert (format "/***** This file is auto-generated, do not modify. *****/

#if ARDUINO
n_primitives = %s;
#else
n_primitives = %s;
#endif
primitives = (void**)malloc(sizeof(void*)*n_primitives);
" (+ l_names l_arduino_names) (+ l_names l_desktop_names)))
(write_c names)
(insert "#if  ARDUINO\n")
(write_c arduino_names l_names)
(insert "#else\n")
(write_c desktop_names l_names)
(insert "#endif\n")
(save-buffer)
