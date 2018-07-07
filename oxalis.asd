;;;; oxalis.asd

(eval-when (:load-toplevel :compile-toplevel :execute)
  (push :verbose-no-init *features*)

  #+quicklisp (ql:quickload :verbose)
  #-quicklisp (asdf:load-system :verbose))

(asdf:defsystem #:oxalis
  :description "Describe oxalis here"
  :author "Hsu, Po-Chun"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :defsystem-depends-on (:qtools)
  :depends-on (:qtcore
               :qtgui
               :qtopengl
               :uiop)
  :components ((:file "package")
               (:file "oxalis"))
  :build-operation "qt-program-op"
  :build-pathname "oxalis"
  :entry-point "oxalis:main")
