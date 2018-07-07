;;;; oxalis.lisp

(in-package #:oxalis)
(in-readtable :qtools)

(defvar *window-height* 128)
(defvar *window-width* 128)

(defclass main-window ()
  ()
  (:metaclass qt-class)
  (:qt-superclass "QWidget"))

(defmethod initialize-instance :after ((instance main-window) &key)
  (new instance)
  (let ((figure (make-instance 'figure)))

    (#_resize instance *window-height* *window-width*)
    ;; (#_setWindowFlags instance (enum-value (#_Qt::FramelessWindowHint)))
    ;; (#_setAttribute instance (enum-value (#_Qt::WA_TranslucentBackground)))
    ;; (#_setWindowOpacity instance 0.5)
    ;; (#_setStyleSheet instance "background-color: rgba(255, 255, 255, 1)")
    ;; (#_setColor pal (#_QPalette::Background) (#_new QColor 127 127 127 0))
    ;; (#_setPalette instance pal)
    
    (#_setWindowTitle instance "oxalis")
    ))

(defclass figure ()
  ((m-pressed :initform nil :accessor m-pressed)
   (m-point-x :accessor m-point-x)
   (m-point-y :accessor m-point-y))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:override ("mousePressEvent" mouse-press-event)
             ("mouseMoveEvent" mouse-move-event)
             ("mouseReleaseEvent" mouse-release-event)
             ("paintEvent" paint-event)))

(defmethod initialize-instance :after ((instance figure) &key)
  (new instance)
  (let ((pal (#_new QPalette (#_palette instance))))

    ;; (#_resize instance *window-height* *window-width*)
    ;; (#_setWindowFlags instance (enum-value (#_Qt::FramelessWindowHint)))
    ;; (#_setAttribute instance (enum-value (#_Qt::WA_TranslucentBackground)))
    (#_setWindowOpacity instance 0.5)
    ;; (#_setStyleSheet instance "background-color: rgba(255, 255, 255, 1)")
    ;; (#_setColor pal (#_QPalette::Background) (#_new QColor 127 127 127 0))
    ;; (#_setPalette instance pal)
    ))

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
                (+ event-y instance-y (- (m-point-y instance))))))

    (defmethod mouse-release-event ((instance figure) event)
      (setf (m-pressed instance) nil)))

(defmethod paint-event ((instance figure) event)
  (let* ((painter (#_new QPainter instance))
         (pixel (#_new QPixmap "/home/eh643027/Project/oxalis/shime1.png")))
  ;;  (#_setCompositionMode painter (#_QPainter::CompositionMode_Clear))
    (#_drawPixmap painter 0 0 pixel)
    ))

(defun main()
  (make-qapplication)
  (with-objects ((example (make-instance 'main-window)))
    (#_show example)
    (#_exec *qapplication*)))

(main)
