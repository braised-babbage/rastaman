(in-package :rastaman)

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
  (check-type p vec2)
  (check-type a vec2)
  (check-type b vec2)
  (check-type c vec2)
  (flet ((det (a b c d)
           (- (* a d) (* b c))))
    (let ((cx    (det (- (vx2 b) (vx2 a)) (- (vx2 c) (vx2 a))
                      (- (vy2 b) (vy2 a)) (- (vy2 c) (vy2 a))))
          (cy (- (det (- (vx2 a) (vx2 p)) (- (vx2 c) (vx2 a))
                      (- (vy2 a) (vy2 p)) (- (vy2 c) (vy2 a)))))
          (cz    (det (- (vx2 a) (vx2 p)) (- (vx2 b) (vx2 a))
                      (- (vy2 a) (vy2 p)) (- (vy2 b) (vy2 a)))))
      (if (< (abs cx) 1)
          ;; degenerate situation
          (values -1d0 1d0 1d0)
          (values (- 1d0 (/ (+ cy cz) cx))
                  (/ cy cx)
                  (/ cz cx))))))


(defun dot-product (u v)
  (v. u v))

(defun cross-product (u v)
  (vc u v))

(defun unit-vector (u)
  (vunit u))
