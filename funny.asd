(asdf::defsystem :funny
  :class :package-inferred-system
  :depends-on (:sb-introspect
               :cl-graph
               :cl-ppcre
               :cl-fad
               :cl-openal
               :cl-alut
               :cl-alc
               :funny/all
               :sdl2))
