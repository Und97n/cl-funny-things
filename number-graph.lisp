;;-*- Mode:     Lisp -*-
;;;; Author: Pylyp Kolpakov
;;;; Contains:

(uiop/package:define-package :funny/number-graph
  (:use :cl)
  (:import-from :cl-graph))

(in-package :funny/number-graph)

(defun lst-to-number (lst)
  (reduce (lambda (acc x)
            (+ (* acc 10) x))
          lst
          :initial-value 0))

(defun number-to-lst (x)
  (do ((lst nil (cons (mod xx 10) lst))
       (xx x (truncate (/ xx 10))))
      ((<= xx 0) lst)))

(defun process-number (start process-fnc &key waves (processed-numbers (make-hash-table :test #'eql)))
  (let ((next-wave (list start)))
    (labels ((%iterate-3 (origin lst)
               (let ((numb (lst-to-number lst)))
                 (funcall process-fnc origin numb)
                 ;; (format t "~4T ~A -> ~A~%" origin numb)
                 (unless (gethash numb processed-numbers)
                   (push numb next-wave))))
             (%iterate-2 (origin numb head tail)
               ;; (format t "~A ~A ~A~%" head numb tail)
               (%iterate-3 origin (append head (number-to-lst (* numb 2)) tail))
               (when (evenp numb)
                 (%iterate-3 origin (append head (number-to-lst (/ numb 2)) tail))))
             (%iterate-1 (origin x heading)
               (when (and x
                          (/= (car x) 0))
                 (do ((numb (list (car x)) (append numb (list (car rst))))
                      (rst (cdr x) (cdr rst)))
                     ((null rst)
                      (%iterate-2 origin (lst-to-number numb) heading rst))
                   (%iterate-2 origin (lst-to-number numb) heading rst))
                 (%iterate-1 origin (cdr x) (append heading (list (car x))))))
             (%iterate-0 (numb)
               (when (< numb 100000000000)
                 (unless (gethash numb processed-numbers)
                   (setf (gethash numb processed-numbers) t)
                   (%iterate-1 numb (number-to-lst numb) nil)))))
      (loop :for c :from 0 :to waves
            :do (let ((wave next-wave))
                  (setf next-wave nil)
                  ;; (format t "~2%Wave ~A: ~A~%" c wave)
                  (mapc #'%iterate-0 wave)))
      processed-numbers)))

(defun render-cl-graph (data file)
  (when (probe-file file)
    (delete-file file))
  (destructuring-bind (graph processed-numbers) data
    (with-open-file (stream file :direction :output :if-exists :overwrite :if-does-not-exist :create)
      (cl-graph:graph->dot graph
                           stream
                           :edge-formatter (lambda (e stream)
                                             (declare (ignorable e stream))
                                             (format stream
                                                     "label=\"~A\""
                                                     ""))
                           :vertex-formatter (lambda (v stream)
                                               (declare (ignorable v stream))
                                               (let ((numb (cl-graph::element v)))
                                                 (format stream
                                                         "label=\"~A\", color=\"~A\", style=filled"
                                                         numb
                                                         (if (gethash numb processed-numbers) "green" "red"))))))))

(defun build-graph (start &key (waves 5) prev-data)
  (let ((mgraph (if prev-data (first prev-data) (cl-graph:make-graph 'cl-graph:graph-container))))
    (let ((processed-numbers
            (process-number start
                            (lambda (from to)
                              (cl-graph:add-edge-between-vertexes mgraph
                                                                  from
                                                                  to
                                                                  :if-duplicate-do :replace-value))
                            :waves waves
                            :processed-numbers (if prev-data (second prev-data) (make-hash-table :test #'eql)))))
      (list mgraph processed-numbers))))
