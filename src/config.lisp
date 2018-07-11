(in-package #:oxalis)
(in-readtable :qtools)

(defclass menu-config ()
  ((act-all :accessor act-all)
   )
  (:metaclass qt-class)
  (:qt-superclass "QMenu"))

(defmethod initialize-instance :after ((instance menu-config) &key)
  (new instance)

  (#_setTitle instance =config=)

  (setf (act-all instance) (#_new QAction =all-config= instance))

  (#_addAction instance (act-all instance))
  )
