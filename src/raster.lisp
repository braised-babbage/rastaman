(in-package :rastaman)

(defvar *z-buffer*)
(defvar *draw-color*)
(defvar *viewport-matrix*)
(defvar *projection-matrix*)
(defvar *modelview-matrix*)

(defun draw-triangle (image a b c la lb lc)
  ;; Note: now a,b,c are vec3f
  (check-type a vec3)
  (check-type b vec3)
  (check-type c vec3)
  (flet ((draw-point (x y)
           (multiple-value-bind (q r s)
               (barycentric-coordinates (vec3 x y 1.0) a b c)
             (unless (or (minusp q) (minusp r) (minusp s))
               ;; inside triangle, so draw it
               (let* ((z (+ (* q (vz3 a))
                            (* r (vz3 b))
                            (* s (vz3 c))))
                      (frag-depth (max 0 (min 255 (round z))))
                      (intensity (max 0 (min 1
                                             (+ (* q la)
                                                (* r lb)
                                                (* s lc))))))
                 (when (and (< frag-depth (aref *z-buffer* x y)))
                   (setf (aref *z-buffer* x y) frag-depth)
                   (set-pixel-color! image x y
                                     (color intensity intensity intensity))))))))
    (let ((min-x (max 0 (truncate (min (vx3 a) (vx3 b) (vx3 c)))))
          (max-x (min (1- (array-dimension image 1))
                      (truncate (max (vx3 a) (vx3 b) (vx3 c)))))
          (min-y (max 0 (truncate (min (vy3 a) (vy3 b) (vy3 c)))))
          (max-y (min (1- (array-dimension image 0))
                      (truncate (max (vy3 a) (vy3 b) (vy3 c))))))
      (loop :for x :from min-x :upto max-x
            :do (loop :for y :from min-y :upto max-y
                      :do (draw-point x y))))))


(defun set-pixel-color! (image x y color)
  (let ((row (- (array-dimension image 0) (1+ y)))
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

(defparameter *eye*       (vec 0 0 3 1))
(defparameter *center*    (vec 0 0 0 1))
(defparameter *light-dir* (unit-vector (vec 1 1 1 0)))
(defparameter *up*        (vec 0 1 0 0))

(defun render-model (obj image)
  (let ((transform (m* *viewport-matrix* *projection-matrix* *modelview-matrix*)))
    (loop :for (ia ib ic) :across (wavefront-object-faces obj)
          :for a := (elt (wavefront-object-vertices obj) ia)
          :for b := (elt (wavefront-object-vertices obj) ib)
          :for c := (elt (wavefront-object-vertices obj) ic)
          :for la := (dot-product *light-dir*
                                  (elt (wavefront-object-normals obj) ia))
          :for lb := (dot-product *light-dir*
                                  (elt (wavefront-object-normals obj) ib))
          :for lc := (dot-product *light-dir*
                                  (elt (wavefront-object-normals obj) ic))
          :do (let ((sa (v4->v3 (m* transform a)))
                    (sb (v4->v3 (m* transform b)))
                    (sc (v4->v3 (m* transform c))))
                (draw-triangle image sa sb sc la lb lc)))))


(defun render-depthmap (image)
  (destructuring-bind (rows columns channels) (array-dimensions image)
    (when (> channels 1)
      (warn "Rendering depth to an image with ~A channels, although only 1 was expected." channels))
    (dotimes (i rows)
      (dotimes (j columns)
        (setf (aref image i j 0)
              (- 255 (aref *z-buffer* j (- rows i 1))))))) )

(defun render-scene (file &key (display t) (width 800) (height 800)
                            (render-depth nil))
  (let* ((png (make-instance 'png
                             :width width
                             :height height))
         (depthmap (make-instance 'png
                                   :width width
                                   :height height
                                   :color-type :grayscale))
         (obj (load-wavefront-object *object-path*)))
    (let ((*modelview-matrix* (mlookat *eye* *center* *up*))
          (*projection-matrix* (mfrustum -0.5 0.5 -0.5 0.5 1 10))
          (*viewport-matrix* (viewport (* width 1/8) (* height 1/8)
                                       (* width 3/4) (* height 3/4)))
          (*z-buffer* (make-array (list width height)
                                  :element-type '(unsigned-byte 8)
                                  :initial-element 255)))
      (time
       (render-model obj (data-array png)))

      (when render-depth
        (render-depthmap (data-array depthmap))
        (write-png depthmap render-depth)))

    (write-png png file)
    (when display
      (sb-ext:run-program "/usr/bin/open" (list file))
      (when render-depth
        (sb-ext:run-program "/usr/bin/open" (list render-depth))))))
