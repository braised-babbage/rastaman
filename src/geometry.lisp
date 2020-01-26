(in-package :rastaman)

(alexandria:define-constant +depth+ 255)

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
  (check-type p vec3)
  (check-type a vec3)
  (check-type b vec3)
  (check-type c vec3)
  (flet ((det (a b c d)
           (- (* a d) (* b c))))
    (let ((cx    (det (- (vx3 b) (vx3 a)) (- (vx3 c) (vx3 a))
                      (- (vy3 b) (vy3 a)) (- (vy3 c) (vy3 a))))
          (cy (- (det (- (vx3 a) (vx3 p)) (- (vx3 c) (vx3 a))
                      (- (vy3 a) (vy3 p)) (- (vy3 c) (vy3 a)))))
          (cz    (det (- (vx3 a) (vx3 p)) (- (vx3 b) (vx3 a))
                      (- (vy3 a) (vy3 p)) (- (vy3 b) (vy3 a)))))
      (if (< (abs cx) 1)
          ;; degenerate situation
          (values -1.0 1.0 1.0)
          (values (- 1.0 (/ (+ cy cz) cx))
                  (/ cy cx)
                  (/ cz cx))))))


(defun dot-product (u v)
  (v. u v))

(defun cross-product (u v)
  (assert (zerop (vw u)))
  (assert (zerop (vw v)))
  (vxyz_ (vc (vxyz u) (vxyz v))))

(defun unit-vector (u)
  (vunit u))

(defun v4->v3 (v)
  (with-vec (x y z w) v
    (vec3 (/ x w)
          (/ y w)
          (/ z w))))

(defun viewport (x y width height)
  (mat (/ width 2)     0           0     (+ x (/ width 2))
       0      (/ height 2)    0     (+ y (/ height 2))
       0           0     (/ +depth+ 2)  (/ +depth+ 2)
       0           0          0              1))
