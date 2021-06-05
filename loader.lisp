(unless (boundp 'cl-user::+bfd-home+)
  (defconstant cl-user::+lda-home+ "/home/pcx/workspace/lisp/funny/"))

(require :asdf)

(flet ((%proj-path (x)
         (format nil "~A~A" cl-user::+lda-home+ x)))
  (asdf:initialize-output-translations `(:output-translations (t ,(pathname (format nil "~A/binary/**/*.*" cl-user::+lda-home+))) :inherit-configuration))
  (push (%proj-path "/") asdf:*central-registry*)
  (loop :for x :in (directory (%proj-path "/lib/*/"))
        :do (progn (format t "Lib: ~A~%" x)
                   (push (princ-to-string x) asdf:*central-registry*)))
  (asdf:register-system-packages :funny/all :funny)
  ;; (asdf:register-system-packages :bifd/tests/all :lda-tests)
  (asdf:load-system :funny))
