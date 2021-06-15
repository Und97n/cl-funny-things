;;-*- Mode:     Lisp -*-
;;;; Author: Pylyp Kolpakov
;;;; Contains:

(uiop/package:define-package :funny/primes-fun
  (:use :cl)
  (:import-from :bordeaux-threads)
  (:import-from :alexandria)
  (:import-from :cl-primesieve))

(in-package :funny/primes-fun)

(defvar *primes* nil)

;; (defun load-primes-txt (file &optional (limit 1000000))
;;   (with-open-file (st file :direction :input)
;;     (setf *primes*
;;           (do ((numb (read st) (read st nil))
;;                (lst nil (cons numb lst))
;;                (counter 0 (1+ counter)))
;;               ((or (null numb) (> counter limit))
;;                (make-array (list counter) :initial-contents (reverse lst)))))))

(defun write-primes-to-file (n filename)
  (with-open-file  (stream filename
                           :element-type '(unsigned-byte 64)
                           :direction :output
                           :if-does-not-exist :create
                           :if-exists :supersede)
    (write-sequence (cl-primesieve:generate-n-primes n) stream)
    t))

(defun read-primes-from-file (n filename)
  (with-open-file (stream filename
                          :element-type '(unsigned-byte 64)
                          :direction :input)
    (let ((buffer (make-array (list n) :element-type '(unsigned-byte 64))))
      (read-sequence buffer stream)
      buffer)))

(defun find-divisors (numb)
  (labels ((%iterate (x acc)
             (loop :for prime :across *primes*
                   :for prime-id :from 0
                   :when (= x prime)
                     :return (cons prime acc)
                   :when (zerop (mod x prime))
                     :return (%iterate (/ x prime) (cons prime acc))
                   :finally (warn "Prime list is too small for number '~A'." x))))
    (%iterate numb nil)))

(defun hash-table-contains (ht fn)
  (maphash (lambda (key val)
             (when (funcall fn key val)
               (return-from hash-table-contains t)))
           ht)
  nil)

(defun process-primes-squares (divisor &key (power 2) trimmer print-header)
  (let ((counter-table (make-hash-table :test #'eql))
        (applied-divisor-groups 0)
        (border (/ divisor power)))
    (loop :for prime :across *primes*
          :do (when (> prime divisor)
                (let ((encounters (gethash (mod (expt prime power) divisor) counter-table)))
                  (when (null encounters)
                    (incf applied-divisor-groups)
                    (when (and trimmer
                               (> applied-divisor-groups trimmer))
                      (return-from process-primes-squares nil)))
                  (incf (gethash (mod (expt prime power) divisor) counter-table 0) 1))))
    (when print-header
      (format t "~2%Divisor: ~A~%" divisor))
    (loop :for (residue-class . encounters) :in (sort (alexandria:hash-table-alist counter-table) #'> :key #'car)
          :do (format t "~4T~3A: ~A~%" residue-class encounters))))
