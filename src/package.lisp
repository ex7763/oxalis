;;;; package.lisp

(defpackage #:oxalis
  (:use #:cl+qt #:alexandria)
  (:export :main))

(in-package #:oxalis)

(defparameter *application-root* (asdf:system-source-directory :oxalis))
(defparameter *images-directory* (merge-pathnames #P"images/" *application-root*))
(defparameter *figure-images-directory* (merge-pathnames #P"figure/" *images-directory*))
(defparameter *config-file* (merge-pathnames ".oxalis" *application-root*))

;; 初始化亂數，每次產生不同的亂數
(setf *random-state* (make-random-state t))
