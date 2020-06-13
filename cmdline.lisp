;;;; cmdline.lisp

#|
To create a program called lnb-parabola type the following lines
from a lisp prompt:

(compile-file "package.lisp" :output-file "pkg.o" :system-p t)
(compile-file "lnbd.lisp" :output-file "lnbd.o" :system-p t)
(compile-file "cmdline.lisp" :output-file "cmdline.o" :system-p t)
(c::build-program "lnb-parabola" :lisp-files '("pkg.o" "lnbd.o" "cmdline.o"))

Files pkg.o, lnbd.o and cmdline.o could be safety removed later.
|#

(setq ext:*help-message* "
lnb-parabola [-h | -? | --help]

    Display help message about command line arguments.

lnb-parabola [--find-satellite orbit-or-name]

    Find satellite from orbit or name.

lnb-parabola [--ft-cm feet]

    Convert feet to centimeter.

lnb-parabola diameter depth focus satellites*

    Calculate LNBs distances for parabola.
      - Diameter and Depth is number in centimeter.
      - Focus and Satellite is orbit degree or satellite name.
    Satellites could be add one or mores.

lnb-parabola [--list-satellites]

    Display list of satellites.

")

(defun validate-arg (string)
  (if (digit-char-p (elt string 0))
      (read-from-string string)
      string))

(defconstant +lnb-parabola-rules+
  '((("-h" "-?" "--help") 0
     (progn (princ ext:*help-message* *standard-output*)
            (ext:quit 0)))
    ("--list-satellites" 0
     (progn (mapc (lambda (satellite)
                    (princ satellite *standard-output*)
                    (terpri *standard-output*))
                  lnbd:*satellites*)
            (ext:quit 0)))
    ("--find-satellite" 1
     (progn (princ (lnbd:find-satellite (validate-arg 1)) *standard-output*)
            (terpri *standard-output*)
            (ext:quit 0)))
    ("--ft-cm" 1
     (progn (princ (lnbd:ft->cm (read-from-string 1)) *standard-output*)
            (terpri *standard-output*)
            (ext:quit 0)))
    ("*DEFAULT*" 1
     (progn (apply 'lnbd:make-parabola (mapcar #'validate-arg 1))) :stop)))

(let ((ext:*lisp-init-file-list* NIL)) ; No initialization files
  (handler-case (ext:process-command-args :rules +lnb-parabola-rules+)
    (error (c)
      (princ ext:*help-message* *error-output*)
      (ext:quit 1))))
(ext:quit 0)
