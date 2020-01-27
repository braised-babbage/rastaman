(in-package :rastaman)


(defun barycentric-coordinates (p a b c)
  "Find barycentric coordinates of the plane point P with respect to points A,B,C.

These are real numbers Q,R,S satisfying q + r + s = 1 and P = qA + rB + sC.

Note: This assumes that all points are expressed in homogenous coordinates."
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


(defun clipped-bounding-box (a b c width height)
  "Get the coordinates of a bounding box containing the triangle ABC.

The result contains the intersection of the triangle with the rectangle [0,width) x [0,height)."
  (let ((min-x (max 0 (truncate (min (vx3 a) (vx3 b) (vx3 c)))))
        (max-x (min (1- width)
                    (truncate (max (vx3 a) (vx3 b) (vx3 c)))))
        (min-y (max 0 (truncate (min (vy3 a) (vy3 b) (vy3 c)))))
        (max-y (min (1- height)
                    (truncate (max (vy3 a) (vy3 b) (vy3 c))))))
    (values min-x min-y max-x max-y)))


(defun dot-product (u v)
  "Compute the dot product of vectors U and V."
  (v. u v))

(defun cross-product (u v)
  "Compute the cross product of vectors U and V.

Note: This assumes that U,V are represented in homogenous coordinates."
  (assert (zerop (vw u)))
  (assert (zerop (vw v)))
  (vxyz_ (vc (vxyz u) (vxyz v))))

(defun unit-vector (C)
  "Get a unit vector in the direction specified by the vector V."
  (vunit c))

(defun dehomogenize (pt)
  "Convert a point PT in homogenous coordinates to one in standard coordinates."
  (with-vec (x y z w) pt
    (vec3 (/ x w)
          (/ y w)
          (/ z w))))

(defun viewport (x y width height depth)
  (let ((d (/ depth 2)))
    (mat (/ width 2)    0      0     (+ x (/ width 2))
              0   (/ height 2) 0     (+ y (/ height 2))
              0         0 (/ depth 2)  (/ depth 2)
              0         0      0            1)))
