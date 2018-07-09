;;;; oxalis.lisp

(in-package #:oxalis)
(in-readtable :qtools)

(defvar *qmessagebox-yes* 16384) ;; (#_QMessageBox::Yes)
(defvar *qmessagebox-no* 65536) ;; (#_QMessageBox::No)

(defvar *window-height* 128)
(defvar *window-width* 128)

(defvar *window-flags* (logior
                        (enum-value (#_Qt::FramelessWindowHint))))
                        ;; (enum-value (#_Qt::ToolTip))))
(defvar *window-icon* (namestring
                       (merge-pathnames "oxalis.png" *images-directory*)))
(defvar *figure-image* (namestring
                        (merge-pathnames "shime0.png" *figure-images-directory*)))
(defvar *figure-frame* 0)
(defvar *figure-mirrored* nil)

(defclass figure ()
  ((m-pressed :initform nil :accessor m-pressed)
   (m-point-x :accessor m-point-x)
   (m-point-y :accessor m-point-y)

   (figure-animate-timer :accessor figure-animate-timer)
   (auto-move-timer :accessor auto-move-timer)
   (walkingp :initform nil :accessor walkingp)
   (walking-point-x :accessor walking-point-x)
   (walking-point-y :accessor walking-point-y)
   
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
   ("figure_animate()" figure-animate)
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
  (let ((window-icon (#_new QIcon *window-icon*)))
    (#_setWindowTitle instance "oxalis")
    (#_setWindowIcon instance window-icon)
    (#_resize instance *window-height* *window-width*)
    ;;; 使視窗無邊框、透明
    (#_setWindowFlags instance (logior *window-flags*
                                       (enum-value (#_Qt::WindowStaysOnTopHint))))
    (#_setAttribute instance (enum-value (#_Qt::WA_TranslucentBackground)))
    (#_setWindowOpacity instance 1.0)

    ;;; 初始化物件
    ;; Timer
    (setf (figure-animate-timer instance) (#_new QTimer instance))
    (setf (auto-move-timer instance) (#_new QTimer instance))
    ;; Menu
    (setf (menu instance) (#_new QMenu instance))
    (setf (act-about instance) (#_new QAction "About" (menu instance)))
    (setf (act-walking instance) (#_new QAction "Walking" (menu instance)))
    (setf (act-top instance) (#_new QAction "Always on top" (menu instance)))
    (setf (act-quit instance) (#_new QAction "Quit" (menu instance)))
    ;; 設定是否可勾選
    (#_setCheckable (act-walking instance) t)
    (#_setChecked (act-walking instance) nil)
    (#_setCheckable (act-top instance) t)
    (#_setChecked (act-top instance) nil)
    ;; 加進 menu
    (#_addAction (menu instance) (act-about instance))
    (#_addAction (menu instance) (act-walking instance))
    (#_addAction (menu instance) (act-top instance))
    (#_addAction (menu instance) (act-quit instance))
    ;; Tray
    (setf (tray instance) (#_new QSystemTrayIcon instance))
    (#_setIcon (tray instance) window-icon)
    (#_setToolTip (tray instance) "oxalis")
    (#_show (tray instance)) ; tray 不會主動顯示
    (#_setContextMenu (tray instance) (menu instance))
    (#_setParent (tray instance) instance)
    ; (#_delete (tray instance))

    
    ;;; 連接信號
    ;; Timer
    (connect (figure-animate-timer instance) "timeout()" instance "figure_animate()")
    (connect (auto-move-timer instance) "timeout()" instance "auto_walking_move()")
    ;; Menu
    (connect (act-about instance) "triggered()" instance "show_about_msg()")
    (connect (act-quit instance) "triggered()" instance "close()")
    (connect (act-walking instance) "triggered(bool)" instance "auto_walking(bool)")
    (connect (act-top instance) "triggered(bool)" instance "always_on_top(bool)")
    ))

;; 移動的動畫，每次換不同的圖片
(defun figure-animate (instance)
  (setf *figure-frame* (mod (1+ *figure-frame*) 3))
  (setf *figure-image* (namestring
                   (merge-pathnames (format nil "shime~A.png" *figure-frame*)
                                    *figure-images-directory*)))
  (#_repaint instance))

;; 隨機移動
(defun auto-walking (instance bool)
  (if bool
      (progn
        (#_showMessage (tray instance) "oxalis" "auto walking start" (#_QSystemTrayIcon::Information) 3000) ; 顯示訊息
        (setf (walkingp instance) t)
        ;; 隨機走到一個點
        (setf (walking-point-x instance)
              (random (#_width (#_desktop *qapplication*))))
        (setf (walking-point-y instance)
              (random (#_height (#_desktop *qapplication*))))
        (#_start (auto-move-timer instance) 16)
        ;; 開始走路的動畫
        (#_start (figure-animate-timer instance) 300))
      (progn
        (#_showMessage (tray instance) "oxalis" "auto walking stop" (#_QSystemTrayIcon::Information) 3000) ; 顯示訊息
        (#_stop (auto-move-timer instance))
        (#_stop (figure-animate-timer instance)))))

(defun walking-arrow (num)
  (if (> num 0)
      -1
      (if (< num 0)
          1
          0)))

(defun auto-walking-move (instance)
  (let ((x (- (#_x (#_pos instance)) (walking-point-x instance)))
        (y (- (#_y (#_pos instance)) (walking-point-y instance)))
        )
    ;; (format t "~A ~A~%" x y) ; 顯示離目標位置差多遠
    (when (= x y 0)
      (setf (walkingp instance) nil)
      (sleep (random 10)) ; TODO fix 會整個程式停掉
      )

    (if (> (walking-arrow x) 0)
        (setf *figure-mirrored* t)
        (setf *figure-mirrored* nil))

    (if (walkingp instance)
        (progn
          (#_move instance
                  (+ (#_x (#_pos instance))
                     (walking-arrow x))
                  (+ (#_y (#_pos instance))
                     (walking-arrow y)))
          )
        (progn
          (setf (walkingp instance) t)
          (setf (walking-point-x instance)
              (random (#_width (#_desktop *qapplication*))))
          (setf (walking-point-y instance)
                (random (#_height (#_desktop *qapplication*))))))
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
  (#_move (menu instance) (#_pos (#_cursor instance)))
  (#_show (menu instance)))

(defun show-about-msg (instance)
  (let ((system (asdf:find-system :oxalis)))
    (#_QMessageBox::about
     instance
     "About"
     (format nil
             "~a<br />
The source code is licensed under ~a.<br />
<br />
Homepage: <a href=\"~a~:*\">~a</a><br />
Author: ~a<br />
Version: ~a"
             (asdf:system-description system)
             (asdf:system-license system)
             (asdf:system-homepage system)
             (asdf:system-author system)
             (asdf:component-version system)))))

;; 畫出圖片
(defmethod paint-event ((instance figure) event)
  (let* ((painter (#_new QPainter))
         (image (#_new QImage *figure-image*))
         (pixel))
    ; (#_save image *figure-image* "PNG") ; 轉換成 Qt 接受的 png 格式
    (when *figure-mirrored*
      (setf image (#_mirrored image t nil)))
    (setf pixel (#_QPixmap::fromImage image))
    (#_begin painter instance)
    (#_drawPixmap painter 0 0 pixel)
    (#_end painter)
    ))

(defun main()
  (make-qapplication)
  (with-objects ((example (make-instance 'figure)))
    (#_show example)
    (#_exec *qapplication*)))
