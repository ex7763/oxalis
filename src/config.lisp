(in-package #:oxalis)
(in-readtable :qtools)

(defclass menu-config ()
  ((act-all :accessor act-all)
   )
  (:metaclass qt-class)
  (:qt-superclass "QMenu")
  (:slots
   ("menu_config_page()" menu-config-page)))

(defmethod initialize-instance :after ((instance menu-config) &key)
  (new instance)

  (#_setTitle instance =config=)

  ;; 初始化變數
  (setf (act-all instance) (#_new QAction =all-config= instance))
  ;; 加進 menu
  (#_addAction instance (act-all instance))

  ;; 連接訊號
  (connect (act-all instance) "triggered()" instance "menu_config_page()")

)


(defun menu-config-page (instance)
  (let* ((page (#_new QWidget instance))
         (width 450)
         (height 600)
         (desktop (#_desktop *qapplication*))
         (current-monitor (#_screenNumber desktop instance))
         (desktop-size (#_screenGeometry desktop current-monitor))

         (layout (#_new QGridLayout))

         (box-language (#_new QComboBox)))
    
    (#_setWindowFlags page (enum-value (#_Qt::Window)))
    (#_setWindowTitle page "Config")
    ;(#_resize page width height)
    ;; (x, y) 是螢幕左上角，在多螢幕中的位置，width, height 是現在這個螢幕的大小
    (#_move page
            (- (+ (#_x desktop-size) 
                (floor (#_width desktop-size) 2))
               (floor width 2))
            (- (+ (#_y desktop-size)
                (floor (#_height desktop-size) 2))
               (floor height 2)))

    (#_setLayout page layout)
    (#_addWidget layout (#_new QLabel "<h1>All Configuration</h1>") 1 0 1 3)
    (#_addWidget layout (#_new QLabel "Always on top") 2 0)
    (#_addWidget layout (#_new QCheckBox) 2 1)
    (#_addWidget layout (#_new QLabel "Language") 2 2)
    (#_addWidget layout box-language 2 3)
    (#_addItem box-language "Traditional Chinese")
    (#_addItem box-language "English")

    (#_show page)))

;; 直接取代原本的設定檔
(defun config-write-settings (ht &optional (file *config-file*))
  (with-open-file (out file
                       :direction :output
                       :if-exists :supersede)
    (maphash #'(lambda (key value)
                 (format out "'~A' : '~A';~%" key value))
             ht)))

;; 目前沒有錯誤檢查功能，設定檔格式必須完全正確
(defun config-read-settings (&optional (file *config-file*))
  (let ((str (read-file-into-string file)))
    (config-parse-settings str)
    ))

;; 返回一個 hash-table ， key 是設定的項目， value 是設定的值
(defun config-parse-settings (str)
  (let ((ht (make-hash-table :test 'equalp)))
    (mapcar #'(lambda (parsed)
                (setf (gethash (first parsed) ht) (second parsed)))
            (mapcar #'(lambda (line)
                        (config-parse-line line))
                    (ppcre:all-matches-as-strings "'.*;" str)))
    ht))

(defun config-parse-line (line)
  (mapcar #'(lambda (token)
              (subseq token 1 (1- (length token))))
          (ppcre:all-matches-as-strings "'.*?'" line)))
