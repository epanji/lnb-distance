;;;; lnbd.lisp

(in-package #:lnbd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Satellites
;;;

(defclass satellite ()
  ((%orbit
    :initarg :orbit
    :initform (error "Required")
    :accessor satellite-orbit)
   (%name
    :initarg :name
    :initform (error "Required")
    :accessor satellite-name)))

(defmethod print-object ((self satellite) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~S ~A°" (satellite-name self) (satellite-orbit self))))

(defun make-satellite (orbit name)
  (make-instance 'satellite :orbit orbit :name name))

(defparameter *satellites* (list (make-satellite 057.0 "NSS 12")
                                 (make-satellite 064.2 "Intelsat 906")
                                 (make-satellite 066.0 "Intelsat 17")
                                 (make-satellite 068.5 "Intelsat 20")
                                 (make-satellite 070.5 "Eutelsat 70B")
                                 (make-satellite 075.0 "ABS 2")
                                 (make-satellite 076.5 "Apstar 7")
                                 (make-satellite 078.5 "Thaicom 5")
                                 (make-satellite 078.5 "Thaicom 6")
                                 (make-satellite 087.5 "ChinaSat 12")
                                 (make-satellite 088.0 "ST 2")
                                 (make-satellite 091.5 "Measat 3")
                                 (make-satellite 091.5 "Measat 3a")
                                 (make-satellite 091.5 "Measat 3B")
                                 (make-satellite 093.5 "G Sat 17")
                                 (make-satellite 093.5 "Insat 4B")
                                 (make-satellite 095.0 "NSS 6")
                                 (make-satellite 096.5 "Express AM33")
                                 (make-satellite 098.0 "ChinaSat 11")
                                 (make-satellite 100.5 "AsiaSat 5")
                                 (make-satellite 105.5 "AsiaSat 7")
                                 (make-satellite 108.2 "SES 7")
                                 (make-satellite 108.2 "SES 9")
                                 (make-satellite 108.2 "Telkom 4")
                                 (make-satellite 110.5 "ChinaSat 10")
                                 (make-satellite 113.0 "Palapa D")
                                 (make-satellite 115.5 "ChinaSat 6B")
                                 (make-satellite 118.0 "Telkom 3S")
                                 (make-satellite 119.5 "Thaicom 4")
                                 (make-satellite 120.0 "Thaicom 7")
                                 (make-satellite 122.2 "AsiaSat 9")
                                 (make-satellite 124.0 "JCSAT 4B")
                                 (make-satellite 125.0 "ChinaSat 6A")
                                 (make-satellite 128.0 "Laosat 1")
                                 (make-satellite 128.0 "JCSAT 3A")
                                 (make-satellite 132.0 "Vinasat 1")
                                 (make-satellite 134.0 "Apstar 6")
                                 (make-satellite 138.0 "Telstar 18")
                                 (make-satellite 142.0 "Apstar 9")
                                 (make-satellite 154.0 "JCSAT 2A")
                                 (make-satellite 159.0 "ABS 6")
                                 (make-satellite 166.0 "Intelsat 19")
                                 (make-satellite 169.0 "Intelsat 8")
                                 (make-satellite 172.0 "Eutelsat 172A")
                                 ;; add more satellites
                                 ))

(defun find-satellite-by-orbit (orbit)
  (declare (number orbit))
  (find orbit *satellites* :test '= :key 'satellite-orbit))

(defun find-satellite-by-name (name)
  (declare (string name))
  (find name *satellites* :test 'string-equal :key 'satellite-name))

(defun find-satellite (orbit-or-name)
  (typecase orbit-or-name
    (satellite orbit-or-name)
    (number (find-satellite-by-orbit orbit-or-name))
    (string (find-satellite-by-name orbit-or-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parabola
;;;

(defvar *ft/cm* (complex 1.0 30.48))

(defun ft->cm (n)
  (imagpart (* *ft/cm* n)))

(defclass parabola ()
  ((%diameter
    :initarg :diameter
    :initform (error "Required")
    :accessor parabola-diameter)
   (%depth
    :initarg :depth
    :initform (error "Required")
    :accessor parabola-depth)
   (%focus-satellite
    :initarg :focus-satellite
    :initform (error "Required")
    :accessor focus-satellite)
   (%more-satellites
    :initarg :more-satellites
    :initform (error "Minimum one required")
    :accessor more-satellites)))

(defmethod print-object ((self parabola) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "diameter ~A cm, depth ~A cm"
            (parabola-diameter self) (parabola-depth self))))

(defun make-parabola (diameter depth focus-satellite &rest satellites)
  (declare (number diameter depth)
           (type (not null) focus-satellite satellites))
  (setf focus-satellite (find-satellite focus-satellite))
  (setf satellites (mapcar 'find-satellite satellites))
  (let ((parabola (make-instance 'parabola :diameter diameter
                                           :depth depth
                                           :focus-satellite focus-satellite
                                           :more-satellites satellites)))
    (prog1 parabola
      (print-parabola parabola))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Calculation
;;;

(defvar *deg/rad* (complex 1.0 0.017453292519943295d0))

(defun deg->rad (n)
  (imagpart (* *deg/rad* n)))

(defun focal-length (parabola)
  (declare (type parabola parabola))
  "Formula F = D² / 16d"
  (/ (expt (parabola-diameter parabola) 2)
     (* 16 (parabola-depth parabola))))

(defun radius (parabola)
  (declare (type parabola parabola))
  "Formula R = (d / 2) + (D² / 8d)"
  (+ (/ (parabola-depth parabola) 2)
     (/ (expt (parabola-diameter parabola) 2)
        (* 8 (parabola-depth parabola)))))

(defun distance-two-lnb (focus other radius focal-length)
  (declare (number focus other radius focal-length))
  "Formula L = RADIANS(ABS(DEG1 - DEG2)) * (R - F)"
  (* (deg->rad (abs (- focus other)))
     (- radius focal-length)))

(defun print-parabola (self &optional (stream t))
  (declare (type parabola self))
  "Human readable print for parabola with lnb distance in centimeter."
  (let ((focal-length (focal-length self))
        (radius (radius self))
        (focus (focus-satellite self)))
    (format stream "~%LNB calculation for parabola ~\
                    with diameter ~,2F cm and depth ~,2F cm.~\
                    ~2%~4TFocal length ~20T~,2F cm~\
                    ~%~4TRadius ~20T~,2F cm~\
                    ~2%This table start from WEST to EAST ~\
                    with focus on ~S.~%~\
                    ~:{~%~4T~12A ~12A ~,2F~}~2&"
            (parabola-diameter self)
            (parabola-depth self)
            focal-length
            radius
            (satellite-name focus)
            (loop with sats = (sort (more-satellites self) '>
                                    :key 'satellite-orbit)
                  for sat in sats
                  if (> (satellite-orbit sat) (satellite-orbit focus))
                    collect (list (satellite-name sat)
                                  (satellite-name focus)
                                  (distance-two-lnb (satellite-orbit focus)
                                                    (satellite-orbit sat)
                                                    radius
                                                    focal-length))
                      into west
                  else
                    collect (list (satellite-name focus)
                                  (satellite-name sat)
                                  (distance-two-lnb (satellite-orbit focus)
                                                    (satellite-orbit sat)
                                                    radius
                                                    focal-length))
                      into east
                  finally (return `(("From" "To" "Cm")
                                    ("-----------" "-----------" "-----")
                                    ,@west
                                    (,(satellite-name focus)
                                     ,(satellite-name focus) 0.0)
                                    ,@east))))))
