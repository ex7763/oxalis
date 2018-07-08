;;;; oxalis.lisp

(in-package #:oxalis)
(in-readtable :qtools)

(defvar *qmessagebox-yes* 16384) ;; (#_QMessageBox::Yes)
(defvar *qmessagebox-no* 65536) ;; (#_QMessageBox::No)

(defvar *window-height* 128)
(defvar *window-width* 128)

(defvar *window-flags* (enum-value (#_Qt::FramelessWindowHint)))

(defclass figure ()
  ((m-pressed :initform nil :accessor m-pressed)
   (m-point-x :accessor m-point-x)
   (m-point-y :accessor m-point-y)
   (auto-move-timer :accessor auto-move-timer)
   
   (menu :accessor menu)
   (act-about :accessor act-about)
   (act-walking :accessor act-walking)
   (act-top :accessor act-top)
   (act-quit :accessor act-quit)

   (tray :accessor tray)
   )
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:slots
   ("show_about_msg()" show-about-msg)
   ("auto_walking(bool)" auto-walking)
   ("auto_walking_move()" auto-walking-move)
   ("always_on_top(bool)" always-on-top))
  (:override ("mousePressEvent" mouse-press-event)
             ("mouseMoveEvent" mouse-move-event)
             ("mouseReleaseEvent" mouse-release-event)
             ("contextMenuEvent" context-menu-event)
             ("paintEvent" paint-event)))

(defmethod initialize-instance :after ((instance figure) &key)
  (new instance)
  (let ((tray-icon (#_new QIcon "/home/eh643027/Project/oxalis/shime1.png")))
    (#_setWindowTitle instance "oxalis")
    (#_resize instance *window-height* *window-width*)
    ;;; 使視窗無邊框、透明
    (#_setWindowFlags instance (logior *window-flags* (enum-value (#_Qt::WindowStaysOnTopHint))))
    (#_setAttribute instance (enum-value (#_Qt::WA_TranslucentBackground)))
    (#_setWindowOpacity instance 1.0)

    ;;; 初始化物件
    ;; Timer
    (setf auto-move-timer (#_new QTimer instance))
    ;; Menu
    (setf menu (#_new QMenu instance))
    (setf act-about (#_new QAction "About" menu))
    (setf act-walking (#_new QAction "Walking" menu))
    (setf act-top (#_new QAction "Always on top" menu))
    (setf act-quit (#_new QAction "Quit" menu))
    ;; 設定是否可勾選
    (#_setCheckable act-walking t)
    (#_setChecked act-walking nil)
    (#_setCheckable act-top t)
    (#_setChecked act-top nil)
    ;; 加進 menu
    (#_addAction menu act-about)
    (#_addAction menu act-walking)
    (#_addAction menu act-top)
    (#_addAction menu act-quit)
    ;; Tray
    (setf tray (#_new QSystemTrayIcon instance))
    (#_setIcon tray tray-icon)
    (#_setToolTip tray "oxalis")
    (#_show tray) ; tray 不會主動顯示
    (#_showMessage tray "test" "ppppp") ; 顯示訊息
    (#_setContextMenu tray menu)

    
    ;;; 連接信號
    ;; Timer
    (connect auto-move-timer "timeout()" instance "auto_walking_move()")
    ;; Menu
    (connect act-about "triggered()" instance "show_about_msg()")
    (connect act-quit "triggered()" instance "close()")
    (connect act-walking "triggered(bool)" instance "auto_walking(bool)")
    (connect act-top "triggered(bool)" instance "always_on_top(bool)")
    ))

(defun auto-walking (instance bool)
  (if bool
      (#_start auto-move-timer 1000)
      (#_stop auto-move-timer)))

(defun auto-walking-move (instance)
  (#_move instance
          (+ (#_x (#_pos instance)) (random 10))
          (+ (#_y (#_pos instance)) (random 10))
          ))

(defun always-on-top (instance bool)
  ;; (declare (ignore bool))
  ;; (setf *window-flags* (logior *window-flags* (enum-value (#_Qt::WindowStaysOnTopHint))))
  ;; Qt 不能重設 windowflags，重設會呼叫 hide 強制隱藏
  ;; (#_setWindowFlags instance *window-flags*)
)

;; mouse-press-event, mouse-move-event, mouse-release-event 主要處理移動視窗的功能
(defmethod mouse-press-event ((instance figure) event)
  (when (equalp (enum-value (#_button event)) (enum-value (#_Qt::LeftButton)))
    (setf (m-pressed instance) t)
    (setf (m-point-x instance) (#_x (#_pos event)))
    (setf (m-point-y instance) (#_y (#_pos event)))))

(defmethod mouse-move-event ((instance figure) event)
    (let ((event-y (#_y (#_pos event)))
          (event-x (#_x (#_pos event)))
          (instance-x (#_x (#_pos instance)))
          (instance-y (#_y (#_pos instance))))
      (when (m-pressed instance)
        (#_move instance (+ event-x instance-x (- (m-point-x instance))) 
                (+ event-y instance-y (- (m-point-y instance)))))))

(defmethod mouse-release-event ((instance figure) event)
  (setf (m-pressed instance) nil))

;; 右鍵的菜單
(defmethod context-menu-event ((instance figure) event)
  ;; 顯示位置
  (#_move menu (#_pos (#_cursor instance)))
  (#_show menu))

(defun show-about-msg (instance)
  (#_QMessageBox::information
   instance
   "About"
   "<p>作者: 許博鈞</p><p>測試</p>"
   ))

;; 畫出圖片
(defmethod paint-event ((instance figure) event)
  (let* ((painter (#_new QPainter instance))
         (pixel (#_new QPixmap "/home/eh643027/Project/oxalis/shime1.png")))
    (#_drawPixmap painter 0 0 pixel)
    ))

(defun main()
  (make-qapplication)
  (with-objects ((example (make-instance 'figure)))
    (#_show example)
    (#_exec *qapplication*)))

(main)
