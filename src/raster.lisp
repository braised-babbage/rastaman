(in-package :rastaman)

;;; TODO: flip y coord
;;; TODO: clip to screen

(defvar *image-buffer)
(defvar *z-buffer*)
(defvar *world->screen*)
(defvar *draw-color*)

(defvar *viewport-matrix*)
(defvar *projection-matrix*)
(defvar *modelview-matrix*)

(defun draw-triangle (image a b c)
  ;; Note: now a,b,c are vec3f
  (check-type a vec3)
  (check-type b vec3)
  (check-type c vec3)
  (flet ((draw-point (x y)
           (multiple-value-bind (q r s)
               (barycentric-coordinates (vec3 x y 1.0) a b c)
             (unless (or (minusp q) (minusp r) (minusp s))
               ;; inside triangle, so draw it
               (let ((z (+ (* q (vz3 a))
                           (* r (vz3 b))
                           (* s (vz3 c)))))
                 (when (> z (aref *z-buffer* x y))
                   (setf (aref *z-buffer* x y) z)
                   (set-pixel-color! image x y *draw-color*)))))))
    (let ((min-x (truncate (min (vx3 a) (vx3 b) (vx3 c))))
          (max-x (truncate (max (vx3 a) (vx3 b) (vx3 c))))
          (min-y (truncate (min (vy3 a) (vy3 b) (vy3 c))))
          (max-y (truncate (max (vy3 a) (vy3 b) (vy3 c)))))
      (loop :for x :from min-x :upto max-x
            :do (loop :for y :from min-y :upto max-y
                      :do (draw-point x y))))))


(defun set-pixel-color! (image x y color)
  (let ((row (- (array-dimension image 0) y))
        (col x))
    (setf (aref image row col 0) (elt color 0))
    (setf (aref image row col 1) (elt color 1))
    (setf (aref image row col 2) (elt color 2))))


;;; Demo

(defvar *object-path* #P"/Users/erik/Desktop/african_head.obj")

(defun color (r g b)
  (list (floor (min (* r 256) 255))
        (floor (min (* g 256) 255))
        (floor (min (* b 256) 255))))

(defun z-projection (z)
  (mat  1 0      0   0
        0 1      0   0
        0 0      1   0
        0 0 (/ -1 z) 1))

(alexandria:define-constant +depth+ 255)

(defun viewport (x y width height)
  (mat (/ width 2)     0           0     (+ x (/ width 2))
            0      (/ height 2)    0     (+ y (/ height 2))
            0           0     (/ +depth+ 2)  (/ +depth+ 2)
            0           0          0              1))

(defun v4->v3 (v)
  (with-vec (x y z w) v
    (vec3 (/ x w)
          (/ y w)
          (/ z w))))

(defun render-scene (file &key (display t) (width 800) (height 800))
  (let* ((png (make-instance 'png
                             :width (1+ width)
                             :height (1+ height)))
         (image (data-array png))
         (light-direction (vec3 0 0 -1))
         (obj (load-wavefront-object *object-path*)))
    (let ((*projection-matrix* (z-projection 3))
          (*viewport-matrix* (viewport (* width 1/8) (* height 1/8)
                                       (* width 3/4) (* height 3/4)))
          (*z-buffer* (make-array (list (1+ width) (1+ height))
                                  :element-type 'single-float
                                  :initial-element most-negative-single-float)))
      (let ((transform (m* *viewport-matrix* *projection-matrix*)))
        (loop :for (ia ib ic) :across (wavefront-object-faces obj)
              :for a := (elt (wavefront-object-vertices obj) ia)
              :for b := (elt (wavefront-object-vertices obj) ib)
              :for c := (elt (wavefront-object-vertices obj) ic)
              :do (let* ((normal (unit-vector
                                  (cross-product (v- c a)
                                                 (v- b a))))
                         (intensity (dot-product normal light-direction)))
                    (when (> intensity 0)
                      (let ((*draw-color* (color intensity intensity intensity)))
                        ;; get screen coordinates
                        (let ((sa (v4->v3 (m* transform a)))
                              (sb (v4->v3 (m* transform b)))
                              (sc (v4->v3 (m* transform c))))
                          (draw-triangle image sa sb sc))))))))

    (write-png png file)
    (when display
      (sb-ext:run-program "/usr/bin/open" (list file)))))
