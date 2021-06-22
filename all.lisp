;;-*- Mode:     Lisp -*-
;;;; Author: Pylyp Kolpakov
;;;; Contains:

(uiop/package:define-package :funny/all
  (:use :cl)
  (:nicknames :funny)
  (:use :funny/bifurcational-diagram
        :funny/math-music
        :funny/digits-multiplicator
        :funny/primes-fun
        :funny/pacman-tree))

(in-package :funny/all)
