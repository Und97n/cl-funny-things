;;-*- Mode:     Lisp -*-
;;;; Author: Pylyp Kolpakov
;;;; Contains:

(uiop/package:define-package :funny/pacman-tree
  (:use :cl)
  (:nicknames :ptree)
  (:import-from :uiop)
  (:import-from :cl-graph)
  (:import-from :alexandria
                #:ensure-gethash
                #:maphash-values))

(in-package :funny/pacman-tree)

(defclass pac-package ()
  ((name
    :accessor pac-name
    :initarg :name)
   (deps
    :accessor pac-deps
    :initarg :deps
    :initform nil)
   (shadow-p
    :accessor pac-shadow-p
    :initarg :shadow-p
    :initform nil)))

(defmethod print-object ((obj pac-package) st)
  (format st "#{~A: ~A}" (pac-name obj) (length (pac-deps obj))))

(defvar *pac-table* (make-hash-table :test #'equal))

(defun sys-list-packages ()
  (cl-ppcre:split
   "\\n"
   (with-output-to-string (st)
     (uiop:run-program "pacman -Qe | awk '{print $1}'" :output st))))

(defun sys-list-depencencies (package)
  (cl-ppcre:split
   "\\n"
   (with-output-to-string (st)
     (uiop:run-program (format nil "pactree -d 1 -u ~A" package) :output st))))

(defun ensure-package-node (pname)
  (ensure-gethash pname *pac-table* (make-instance 'pac-package :name pname)))

(defun clear-data ()
  (setf *pac-table* (make-hash-table :test #'equal)))

(defun load-data ()
  (clear-data)
  (let ((packages (sys-list-packages)))
    (loop :for pname :in packages
          :do (let* ((pac (ensure-package-node pname))
                     (deps (sys-list-depencencies pname)))
                (setf (pac-deps pac) (remove pac (mapcar #'ensure-package-node deps)))))
    *pac-table*))

(defun build-pdg (&key (vert-filter (constantly t)))
  (let ((mgraph (cl-graph:make-graph 'cl-graph:graph-container)))
    (loop :for pac :being :the :hash-values :in *pac-table*
          :when (funcall vert-filter pac)
            :do (progn (cl-graph:add-vertex mgraph pac)
                       (loop :for dep :in (pac-deps pac)
                             :when (funcall vert-filter dep)
                               :do (cl-graph:add-edge-between-vertexes mgraph
                                                                       pac
                                                                       dep
                                                                       :edge-type :directed
                                                                       :if-duplicate-do :replace-value))))
    mgraph))

;; Mark unneeded packages
(defun mark-shadows (required-packages)
  (maphash-values (lambda (pac) (setf (pac-shadow-p pac) t))
                  *pac-table*)
  (dolist (pname required-packages)
    (labels ((%mark (pac)
               (setf (pac-shadow-p pac) nil)
               (mapc #'%mark (pac-deps pac))))
      (%mark (ensure-package-node pname))))
  *pac-table*)

(defun render-cl-graph (graph file)
  (when (probe-file file)
    (delete-file file))
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
                                             (let ((pac (cl-graph::element v)))
                                               (format stream
                                                       "label=\"~A\", color=\"~A\", style=filled"
                                                       pac
                                                       (if (pac-shadow-p pac) "gray" "green")
                                                       ))))))
