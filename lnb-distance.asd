;;;; lnb-distance.asd

(asdf:defsystem #:lnb-distance
  :description "LNB-DISTANCE is system to calculate multi LNB distances from center in PARABOLA."
  :author "Panji Kusuma <epanji@gmail.com>"
  :license  "Public Domain"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "lnbd")))
