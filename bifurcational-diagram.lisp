;;-*- Mode:     Lisp -*-
;;;; Author: Pylyp Kolpakov
;;;; Contains:

(uiop/package:define-package :funny/bifurcational-diagram
  (:use :cl)
  (:nicknames :bifd)
  (:import-from :sdl2)
  (:import-from :sdl2-image)
  )

(in-package :funny/bifurcational-diagram)

(defparameter *screen-width* 1280)
(defparameter *screen-height* 720)

(defparameter *repaint* t)

(defparameter *max-pixel* 512)
(defparameter *exposure* 1)
(defparameter *pixels* (make-array (list *screen-width* *screen-height*) :initial-element 0 :element-type 'fixnum))

(defparameter *start-x* 2.8d0)
(defparameter *end-x* 4.d0)
(defparameter *start-y* 0.d0)
(defparameter *end-y* 1.d0)

(defparameter *max-iterations* 1000)
(defparameter *min-iterations* 300)

(declaim (type double-float *start-x* *end-x* *start-y* *end-y*)
         (type fixnum *screen-width* *screen-height* *max-pixel*)
         (type (simple-array fixnum 2) *pixels*))

(defmacro with-window-renderer ((window renderer) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "Bifurcational diagram"
                        :w *screen-width*
                        :h *screen-height*
                        :flags '(:shown))
       (sdl2:with-renderer (,renderer ,window :index -1 :flags '(:accelerated))
         ,@body))))

(defun load-texture (renderer filename)
  (sdl2:create-texture-from-surface renderer (sdl2-image:load-image filename)))

(defmacro with-all-pixels ((x y) &body body)
  `(loop :for ,y :from 0 :below *screen-height*
         :do (loop :for ,x :from 0 :below *screen-width*
                   :do (progn ,@body))))

(defun clear-pixels ()
  (with-all-pixels (x y)
    (setf (aref *pixels* x y) 0)))

(defun rescale-coords (scx scy)
  (let* ((wxd2 (/ (- *end-x* *start-x*) 2))
         (wyd2 (/ (- *end-y* *start-y*) 2))
         (cx (+ *start-x* wxd2))
         (cy (+ *start-y* wyd2))
         (nwxd2 (* wxd2 scx))
         (nwyd2 (* wyd2 scy)))
    (setf *start-x* (- cx nwxd2)
          *end-x* (+ cx nwxd2)
          *start-y* (- cy nwyd2)
          *end-y* (+ cy nwyd2))
    (format t "~A ~A ~A ~A~%" *start-x* *end-x* *start-y* *end-y*)
    t))

(declaim (ftype (function (double-float) fixnum) x2sx)
         (inline x2sx))
(defun x2sx (x)
  (declare (optimize (speed 3) (safety 0))
           (type double-float x))
  (values (the fixnum (truncate (/ (* (- x *start-x*) *screen-width*) (- *end-x* *start-x*))))))

(declaim (ftype (function (double-float) fixnum) y2sy)
         (inline y2sy))
(defun y2sy (y)
  (declare (optimize (speed 3) (safety 0))
           (type double-float y))
  (values (the fixnum (truncate (- *screen-height* (/ (* (- y *start-y*) *screen-height*) (- *end-y* *start-y*)))))))

(defun sx2x (sx)
  (+ (* (/ sx *screen-width*) (- *end-x* *start-x*))
     *start-x*))

(defun sy2y (sy)
  (+ (* (- 1 (/ sy *screen-height*)) (- *end-y* *start-y*))
     *start-y*))

(declaim (ftype (function (double-float double-float)) fill-pixel))
(defun fill-pixel (x y)
  (declare (optimize (speed 3) (safety 0))
           (type double-float x y))
  (let ((px (x2sx x))
        (py (y2sy y)))
    (declare (type fixnum px py))
    (when (and (> px 0)
               (< px *screen-width*)
               (> py 0)
               (< py *screen-height*))
      (let ((new-val (1+ (aref *pixels* px py))))
        (unless (> new-val *max-pixel*)
          (setf (aref *pixels* px py)
                new-val)))))
  t)

(defun bifur-loop (r &key (max-iterations *max-iterations*) (start-iterations *min-iterations*))
  (declare (optimize (speed 3) (safety 0))
           (type fixnum max-iterations start-iterations)
           (type double-float r))
  (labels ((%iterate (x iterations)
             (declare (type fixnum iterations)
                      (type double-float x))
             (let ((nx (the double-float (* x r (- 1.0 x)))))
               (when (> iterations start-iterations)
                 (fill-pixel r nx))
               (when (< iterations max-iterations)
                 (%iterate nx (1+ iterations))))))
    (when (and (> r 0.d0) (< r 4.d0))
      (%iterate 0.5d0 0))))

(defun random-double (start end)
  (+ start
     (* (- end start)
        (/ (random #x10000000) 268435456.d0))))

(defun main ()
  (with-window-renderer (window renderer)
    (sdl2-image:init '(:png))
    (let ((last-mouse-x 0)
          (last-mouse-y 0)
          (mouse-start-coords nil))
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:mousebuttonup (:button button)
                        (when (and mouse-start-coords
                                   (eq button (first mouse-start-coords)))
                          (let ((xx1 (sx2x (second mouse-start-coords)))
                                (yy1 (sy2y (third mouse-start-coords)))
                                (xx2 (sx2x last-mouse-x))
                                (yy2 (sy2y last-mouse-y)))
                            (setf *start-x* (min xx1 xx2))
                            (setf *end-x* (max xx1 xx2))
                            (setf *start-y* (min yy1 yy2))
                            (setf *end-y* (max yy1 yy2)))
                          (clear-pixels)
                          (setf mouse-start-coords nil
                                *repaint* t)))
        (:mousebuttondown (:button button)
                          (setf mouse-start-coords (list button last-mouse-x last-mouse-y)))
        (:mousemotion (:x x :y y ;; :xrel xrel :yrel yrel :state state
                          )
                      (setf last-mouse-x x
                            last-mouse-y y)
                      (when mouse-start-coords
                        (setf *repaint* t))
                      ;; (format t "Mouse motion abs(rel): ~a (~a), ~a (~a)~%Mouse state: ~a~%"
                      ;;         x xrel y yrel state)
                      )
        (:keydown (:keysym keysym)
                  ;; (format t "~A~%" (sdl2:scancode keysym))
                  (case (sdl2:scancode keysym)
                    (:scancode-q (sdl2:push-event :quit))
                    (:scancode-b (break))
                    (:scancode-1 (setf *exposure* (* *exposure* 2)) (setf *repaint* t))
                    (:scancode-2 (setf *exposure* (/ *exposure* 2)) (setf *repaint* t))
                    (:scancode-i
                     (format t "~2%^Enter new max iterations coutn (prev - ~A):" *max-iterations*)
                     (setf *max-iterations* (read))
                     (setf *repaint* t))
                    (:scancode-k
                     (format t "~2%^Enter new min iterations count (prev - ~A):" *min-iterations*)
                     (setf *min-iterations* (read))
                     (setf *repaint* t))
                    (:scancode-up
                     (rescale-coords 0.75d0 0.75d0)
                     (clear-pixels)
                     (setf *repaint* t))
                    (:scancode-down
                     (rescale-coords 1.5d0 1.5d0)
                     (clear-pixels)
                     (setf *repaint* t))
                    (:scancode-r (setf *repaint* t))
                    (:scancode-space
                     (loop :for x :from 0 :to 4000 :do
                       (bifur-loop (random-double (max *start-x* 0) (min 4 *end-x*))))
                     (setf *repaint* t))))
        (:idle ()
               (when *repaint*
                 ;; (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
                 ;; (sdl2:render-clear renderer)
                 (sdl2:set-render-draw-color renderer #xFF #x00 #x00 #xFF)
                 (with-all-pixels (x y)
                   (let ((color (min 255
                                     (max 0
                                          (truncate (/ (* *exposure* #xFF (aref *pixels* x y))
                                                       *max-pixel*))))))
                     (declare (type (signed-byte 32) color))
                     (sdl2:set-render-draw-color renderer #x00 #x00 color #xFF)
                     (sdl2:render-draw-point renderer x y)))
                 (sdl2:set-render-draw-color renderer #xFF #x00 #x00 #xFF)
                 (sdl2:render-draw-line renderer (x2sx 0.d0) (y2sy -1.d0) (x2sx 0.d0) (y2sy 1.d0))
                 (sdl2:render-draw-line renderer (x2sx -1.d0) (y2sy 0.d0) (x2sx 1.d0) (y2sy 0.d0))
                 (when mouse-start-coords
                   (let ((xx1 (min (second mouse-start-coords) last-mouse-x))
                         (yy1 (min (third mouse-start-coords) last-mouse-y))
                         (xx2 (max (second mouse-start-coords) last-mouse-x))
                         (yy2 (max (third mouse-start-coords) last-mouse-y)))
                     (sdl2:set-render-draw-color renderer #x00 #xFF #x00 #xFF)
                     (sdl2:render-draw-rect renderer (sdl2:make-rect xx1 yy1 (- xx2 xx1) (- yy2 yy1)))))
                 (sdl2:render-present renderer)
                 (setf *repaint* nil)))))))
