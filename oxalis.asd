;;;; oxalis.asd
(asdf:defsystem #:oxalis
  :description "A Desktop pet is written in common lisp and Qt4."
  :author "Hsu, Po-Chun"
  :homepage "https://github.com/ex7763/oxalis"
  :license  "GPLv3"
  :version "0.1.2"
  :serial t
  :defsystem-depends-on (:qtools)
  :depends-on (:qtcore
               :qtgui
               :qtopengl
               :uiop
               :alexandria)
  :components ((:module "src"
                        :components ((:file "package")
                                     (:file "lang")
                                     (:file "config")
                                     (:file "oxalis"))))
  :build-operation "qt-program-op"
  :build-pathname "oxalis"
  :entry-point "oxalis:main")
