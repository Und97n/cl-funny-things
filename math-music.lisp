;;-*- Mode:     Lisp -*-
;;;; Author: Pylyp Kolpakov
;;;; Contains:

(uiop/package:define-package :funny/math-music
  (:use :cl)
  (:import-from :bordeaux-threads)
  (:import-from :cl-openal)
  (:import-from :cl-alc)
  (:import-from :cl-alut))

(in-package :funny/math-music)

(defun init-tone (wf freq &optional phase)
  (alut:load-memory-waveform wf (float freq) (float phase) 0.20))

(defun gen-notes (wf &optional (a 0) (b 127) (phase 0))
  (assert (>= a 0))
  (assert (<= b 135))
  (loop :for x :from a :to b
        :for freq := (* 440 (expt 2 (/ (- x 69) 12)))
        :collect (init-tone wf freq phase)))


(defmacro until (condition &body body)
  (let ((block-name (gensym)))
    `(block ,block-name
       (loop
         (if ,condition
             (return-from ,block-name nil)
             (progn
               ,@body))))))


(defun gambie-seq (integer)
  (labels ((%iterate (number last-bit acc rank)
             (if (> number 0)
                 (let ((bit (mod number 2)))
                   (if (eq bit last-bit)
                       (%iterate (ash number -1) bit (+ acc (ash bit rank)) (1+ rank))
                       (%iterate (ash number -1) bit acc rank)))
                 acc)))
    (%iterate integer (- 1 (mod integer 2)) 0 0)))

(defun gg-seq (integer)
  (gambie-seq (gambie-seq integer)))

 (defun print-thread-info ()
      (let* ((curr-thread (bt:current-thread))
             (curr-thread-name (bt:thread-name curr-thread))
             (all-threads (bt:all-threads)))
        (format t "Current thread: ~a~%~%" curr-thread)
        (format t "Current thread name: ~a~%~%" curr-thread-name)
        (format t "All threads:~% ~{~a~%~}~%" all-threads))
      nil)

(defun play-note (notes i &optional (duration 0.2))
  (al:with-buffer (buffer)
    (al:with-source (source)
      (al:buffer-data buffer #x1101 (nth i notes) 16356 11025)
      (al:source source :buffer buffer)
      (al:source source :position #(1 1 1))
      (al:source source :velocity #(0 0 0))
      ;; Place listener at (1 1 1), and have it
      ;; face (0 0 0).
      (al:listener :position #(1 1 1))
      (al:listener :orientation #(0 0 0
                                  0 0 0))
      ;; Let the music play...
      ;; (al:source source :looping :true)
      (al:source-play source)
      (al:source source :looping :false)
      (sleep duration)
      (al:source-stop source)
      )))

;; (defparameter *notes* nil)
(defparameter *lower-note* 60)
(defparameter *notes-count* 64)
(defparameter *tools* '(:sine :sawtooth :square :impulse :whitenoise))

(defparameter *debug-mutex* nil)

(defparameter *sync-semaphore-1* nil)
(defparameter *sync-semaphore-2* nil)

(defun duration-fnc (val)
  (- 1.2 (expt (1+ (gg-seq val)) -0.25)))

(defun play-sound (notes fnc i &optional tool (duration 0.2))
  (when (and *sync-semaphore-1* *sync-semaphore-2*)
    (bt:signal-semaphore *sync-semaphore-2*)
    (bt:wait-on-semaphore *sync-semaphore-1*))
  (let* ((val (funcall fnc i))
         (tool (or tool
                   (min (1- (length *tools*)) (truncate (/ val *notes-count*)))))
         (note (mod val *notes-count*)))
    (when *debug-mutex*
      (bt:with-lock-held (*debug-mutex*)
        (format t "~A: ~6X: ~5A md: ~3A tool: ~A~%" (bt:thread-name (bt:current-thread))
                                                    i val (+ *lower-note* note) (nth tool *tools*))))
    (play-note (nth tool notes) note duration)))

(defun generate-notes-list (&optional (phase 0))
  (let ((notes nil))
    (alut:with-init
      (setf notes
            (mapcar (lambda (tool)
                      (gen-notes tool *lower-note* (+ *lower-note* *notes-count*) phase))
                    *tools*)))
    notes))

(defun main (start end)
  (setf *debug-mutex* (bt:make-lock "debug-print")
        *sync-semaphore-1* (bt:make-semaphore :name "sound-sync-1" :count 0)
        *sync-semaphore-2* (bt:make-semaphore :name "sound-sync-2" :count 0))
  (let* ((t1 (bt:make-thread
              (lambda ()
                (let ((notes (generate-notes-list 0)))
                  (alc:with-device (device)
                    (alc:with-context (context device)
                      (alc:make-context-current context)
                      (loop :for x :from start :to end
                            :do (play-sound notes #'gambie-seq x 0 (duration-fnc x)))))))
              :name "sound-1"))
         (t2 (bt:make-thread
              (lambda ()
                (let ((notes (generate-notes-list 0.32)))
                  (alc:with-device (device)
                    (alc:with-context (context device)
                      (alc:make-context-current context)
                      (loop :for x :from start :to end
                            :do (play-sound notes #'gg-seq x 3 (duration-fnc x)))))))
              :name "sound-2")))
    (unwind-protect (progn
                      (loop :for x :from start :to end
                            :do (progn
                                  (when (and *sync-semaphore-1* *sync-semaphore-2*)
                                    (bt:wait-on-semaphore *sync-semaphore-2*)
                                    (bt:wait-on-semaphore *sync-semaphore-2*)
                                    (bt:signal-semaphore *sync-semaphore-1* :count 2)
                                    )))
                      (bt:join-thread t1)
                      (bt:join-thread t2))
      (mapc (lambda (thread)
              (when (bt:thread-alive-p thread)
                (bt:destroy-thread thread)))
            (list t1 t2))
      (setf *debug-mutex* nil
            *sync-semaphore-1* nil
            *sync-semaphore-2* nil))))
