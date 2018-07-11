(in-package #:oxalis)

;; Support list:
;; 繁體中文 :traditional-chinese
;; English :english
(defparameter ==language== :traditional-chinese)


;; config.lisp
(defvar =config=)
(defvar =all-config=)
;; oxalis.lisp
(defvar =about=)
(defvar =description=)
(defvar =homepage=)
(defvar =author=)
(defvar =author-name=)
(defvar =version=)
(defvar =auto-walking=)
(defvar =auto-walking-start=)
(defvar =auto-walking-stop=)
(defvar =quit=)

(alexandria:switch
 (==language== :test 'equal)
 (:traditional-chinese
  ;; config.lisp
  (setf =config= "設定"
        =all-config= "全部設定")
  ;; oxalis.lisp
  (setf =about= "關於"
        =description= "用 common lisp 跟 Qt4 做的桌面寵物"
        =homepage= "主頁"
        =author= "作者"
        =author-name= "許博鈞"
        =version= "版本"
        =auto-walking= "自動走路"
        =auto-walking-start= "開始自動走路"
        =auto-walking-stop= "停止自動走路"
        =quit= "離開"))
 (:english
  ;; config.lisp
  (setf =config= "Config"
        =all-config= "All config")
  ;; oxalis.lisp
  (setf =about= "About"
        =description= "A Desktop pet is written in common lisp and Qt4."
        =homepage= "Homepage"
        =author= "Author"
        =author-name= "Hsu, Po-Chun"
        =version= "Version"
        =auto-walking= "Auto walking"
        =auto-walking-start= "Auto walking start"
        =auto-walking-stop= "Auto walking stop"
        =quit= "Quit")))


