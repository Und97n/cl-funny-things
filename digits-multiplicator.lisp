;;-*- Mode:     Lisp -*-
;;;; Author: Pylyp Kolpakov
;;;; Contains:

(uiop/package:define-package :funny/digits-multiplicator
  (:use :cl)
  (:import-from :bordeaux-threads))

(in-package :funny/digits-multiplicator)

(defun check-number-with-debug (numb)
  (labels ((%find-next (x acc)
             (if (or (< x 10) (= acc 0))
                 (* acc x)
                 (%find-next (truncate (/ x 10)) (* acc (mod x 10)))))

           (%iterate (x acc)
             (if (> x 9)
                 (let ((rate (%iterate (%find-next x 1) (1+ acc))))
                   (format t "~A ~A~%" x (1+ acc))
                   rate)
                 acc)))
    (%iterate numb 0)))

(defun check (max)
  ;; (declare (optimize (speed 3) (safety 0)))
  (let ((cache (make-hash-table :test #'eql))
        (max-n 1)
        (max-rate 0))
    (labels ((%find-next (x acc)
               ;; (declare (type bignum x acc))
               (if (or (< x 10) (= acc 0))
                   (* acc x)
                   (%find-next (truncate (/ x 10)) (* acc (mod x 10)))))

             (%iterate (x acc)
               ;; (declare (type bignum x acc))
               (if (> x 9)
                   (let ((cv (gethash x cache)))
                     (if cv
                         (+ acc cv)
                         (let ((rate (%iterate (%find-next x 1) (1+ acc))))
                           ;; (format t "~A~%" x)
                           (setf (gethash x cache) acc)
                           (when (> rate max-rate)
                             (setf max-rate acc)
                             (setf max-n x))
                           acc)))
                   acc)))
      (loop :for x :from 0 :to (/ max 10) :do (%iterate (random max) 0))
      (values max-n max-rate))))

(defun find-divisors (numb)
  (labels ((%iterate (n acc)
             (if (< n 4)
                 (cons n acc)
                 (do ((border (truncate (/ n 2)))
                      (x 2 (1+ x))
                      (md (mod n 2) (mod n (1+ x))))
                     ((or (> x border) (= md 0))
                      (if (= md 0)
                          (%iterate (/ n x) (cons x acc))
                          (cons n acc)))))))
    (%iterate numb nil)))
