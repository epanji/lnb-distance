;;;; package.lisp

(defpackage #:lnbd
  (:use #:cl)
  (:export #:*satellites*
           #:find-satellite
           #:make-satellite)
  (:export #:make-parabola
           #:print-parabola)
  (:export #:ft->cm
           #:radius
           #:focal-length
           #:distance-two-lnb))
