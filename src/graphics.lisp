(in-package :rastaman)

(defvar *z-buffer*)
(setf (documentation '*z-buffer* 'variable)
      "The active buffer of depth values, used for rasterization.")

(alexandria:define-constant +z-buffer-depth+ 255
  :documentation "The depth of the Z buffer.")

(defvar *modelview-matrix*)
(setf (documentation '*modelview-matrix* 'variable)
      "The active model-view matrix, mapping object coordinates to camera (\"eye\") coordinates.")

(defvar *projection-matrix*)
(setf (documentation '*projection-matrix* 'variable)
      "The active projection matrix, mapping camera coordinates to clip coordinates.")

(defvar *viewport-matrix*)
(setf (documentation '*viewport-matrix* 'variable)
      "The active projection matrix, mapping clip coordinates to screen coordinates.")


(defun float-to-unsigned-byte (f)
  "Convert a floating point number in [0,1] to a corresponding unsigned byte."
  (max 0 (min 255 (round (* f 256)))))

(defun image-width (image)
  (array-dimension image 1))

(defun image-height (image)
  (array-dimension image 0))

(defun draw-triangle (image shader a b c)
  "Draw a triangle ABC to the provided IMAGE, according to SHADER."
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
                      (frag-depth (max 0 (min +z-buffer-depth+ (round z)))))
                 (when (and (< frag-depth (aref *z-buffer* x y)))
                   (setf (aref *z-buffer* x y) frag-depth)
                   (multiple-value-bind (r g b)
                       (funcall (shader-fragment-program shader) q r s)
                     (%set-pixel-color! image x y r g b))))))))

    (multiple-value-bind (min-x min-y max-x max-y)
        (clipped-bounding-box a b c
                              (image-width image) (image-height image))
      (loop :for x :from min-x :upto max-x
            :do (loop :for y :from min-y :upto max-y
                      :do (draw-point x y))))))

(defun %set-pixel-color! (image x y r g b)
  (let ((row (- (array-dimension image 0) (1+ y)))
        (col x))
    (setf (aref image row col 0) (float-to-unsigned-byte r))
    (setf (aref image row col 1) (float-to-unsigned-byte g))
    (setf (aref image row col 2) (float-to-unsigned-byte b))))
