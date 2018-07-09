;;;; oxalis.asd
(asdf:defsystem #:oxalis
  :description "Desktop Pet"
  :author "許博鈞 Hsu, Po-Chun"
  :homepage "https://github.com/ex7763/oxalis"
  :license  "GPLv3"
  :version "0.1.0"
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
