(in-package :rastaman)

(defstruct (vec2 (:constructor vec2 (x y)))
  x
  y)

(defun vec2= (u v)
  (and (= (vec2-x u) (vec2-x v))
       (= (vec2-y u) (vec2-y v))))

;;; NOTE: we expect integer coordinates
(defun barycentric-coordinates (p a b c)
  ;; To find 0 <= s,t <= 1 satisfying
  ;;
  ;;    p = (1-s-t)a + sb + tc
  ;;
  ;; we note that this equation equivalent to the
  ;; orthogonality relations
  ;;
  ;;   (1,s,t) \perp (x_{p-a}, x_{b-a}, x_{c-a})
  ;;   (1,s,t) \perp (y_{p-a}, y_{b-a}, y_{c-a})
  ;;
  ;; which can be solved with a cross product.
  (flet ((det (a b c d)
           (- (* a d) (* b c))))
    (let ((cx    (det (- (vec2-x b) (vec2-x a)) (- (vec2-x c) (vec2-x a))
                      (- (vec2-y b) (vec2-y a)) (- (vec2-y c) (vec2-y a))))
          (cy (- (det (- (vec2-x a) (vec2-x p)) (- (vec2-x c) (vec2-x a))
                      (- (vec2-y a) (vec2-y p)) (- (vec2-y c) (vec2-y a)))))
          (cz    (det (- (vec2-x a) (vec2-x p)) (- (vec2-x b) (vec2-x a))
                      (- (vec2-y a) (vec2-y p)) (- (vec2-y b) (vec2-y a)))))
      (if (zerop cx)
          ;; degenerate situation
          (values -1d0 1d0 1d0)
          (values (- 1d0 (/ (+ cy cz) cx))
                  (/ cy cx)
                  (/ cz cx))))))

(defstruct (vec3 (:constructor vec3 (x y z)))
  x
  y
  z)

(defun vec3-difference (u v)
  (vec3 (- (vec3-x u) (vec3-x v))
        (- (vec3-y u) (vec3-y v))
        (- (vec3-z u) (vec3-z v))))

(defun dot-product (u v)
  (+ (* (vec3-x u) (vec3-x v))
     (* (vec3-y u) (vec3-y v))
     (* (vec3-z u) (vec3-z v))))

(defun cross-product (u v)
  (flet ((det (a b c d)
           (- (* a d) (* b c))))
    (vec3  (det (vec3-y u) (vec3-z u)
                (vec3-y v) (vec3-z v))
        (- (det (vec3-x u) (vec3-z u)
                (vec3-x v) (vec3-z v)))
           (det (vec3-x u) (vec3-y u)
                (vec3-x v) (vec3-y v)))))

(defun sum-of-squares (u)
  (let ((x (vec3-x u))
        (y (vec3-y u))
        (z (vec3-z u)))
    (+ (* x x) (* y y) (* z z))))

(defun norm (u)
  (sqrt (sum-of-squares u)))

(defun normalize (u)
  (let ((c (norm u)))
    (vec3 (/ (vec3-x u) c)
          (/ (vec3-y u) c)
          (/ (vec3-z u) c))))
