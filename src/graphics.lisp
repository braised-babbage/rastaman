(in-package :rastaman)

(defvar *z-buffer*)
(defvar *viewport-matrix*)
(defvar *projection-matrix*)
(defvar *modelview-matrix*)

(defun float-to-unsigned-byte (f)
  (max 0 (min 255 (round (* f 256)))))

(defun triangle-bounding-box (image a b c)
  (let ((min-x (max 0 (truncate (min (vx3 a) (vx3 b) (vx3 c)))))
        (max-x (min (1- (image-width image))
                    (truncate (max (vx3 a) (vx3 b) (vx3 c)))))
        (min-y (max 0 (truncate (min (vy3 a) (vy3 b) (vy3 c)))))
        (max-y (min (1- (image-height image))
                    (truncate (max (vy3 a) (vy3 b) (vy3 c))))))
    (values min-x min-y max-x max-y)))


(defun image-width (image)
  (array-dimension image 1))

(defun image-height (image)
  (array-dimension image 0))

(defun draw-triangle (image shader a b c)
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
                      (frag-depth (max 0 (min 255 (round z)))))
                 (when (and (< frag-depth (aref *z-buffer* x y)))
                   (setf (aref *z-buffer* x y) frag-depth)
                   (multiple-value-bind (r g b)
                       (funcall (shader-fragment-program shader) q r s)
                     (set-pixel-color! image x y r g b))))))))

    (multiple-value-bind (min-x min-y max-x max-y)
        (triangle-bounding-box image a b c)
      (loop :for x :from min-x :upto max-x
            :do (loop :for y :from min-y :upto max-y
                      :do (draw-point x y))))))

(defun set-pixel-color! (image x y r g b)
  (let ((row (- (array-dimension image 0) (1+ y)))
        (col x))
    (setf (aref image row col 0) (float-to-unsigned-byte r))
    (setf (aref image row col 1) (float-to-unsigned-byte g))
    (setf (aref image row col 2) (float-to-unsigned-byte b))))
