(in-package :rastaman)

(defstruct (vec2 (:constructor vec2 (x y)))
  x
  y)

(defun vec2= (u v)
  (and (= (vec2-x u) (vec2-x v))
       (= (vec2-y u) (vec2-y v))))

(defstruct (vec3 (:constructor vec3 (x y z)))
  x
  y
  z)
