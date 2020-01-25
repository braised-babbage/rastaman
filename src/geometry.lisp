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
