;;;; package.lisp

(defpackage #:oxalis
  (:use #:cl+qt)
  (:export :main))

(in-package #:oxalis)

(defparameter *application-root* (asdf:system-source-directory :oxalis))
(defparameter *images-directory* (merge-pathnames #P"images/" *application-root*))
(defparameter *figure-images-directory* (merge-pathnames "figure/" *images-directory*))


;; 初始化亂數，每次產生不同的亂數
(setf *random-state* (make-random-state t))
