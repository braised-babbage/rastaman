(in-package :rastaman)

(defparameter *eye*       (vec 0 0 3 1))
(defparameter *center*    (vec 0 0 0 1))
(defparameter *light-dir* (unit-vector (vec 1 1 1 0)))
(defparameter *up*        (vec 0 1 0 0))

(defparameter *object-path* #P"/Users/erik/Desktop/african_head.obj")

(defun render-model (obj image)
  (let ((shader (make-gouraud-shader obj)))
    (funcall (shader-init-program shader))
    (dotimes (i (length (wavefront-object-faces obj)))
      (let ((a (funcall (shader-vertex-program shader) i 0))
            (b (funcall (shader-vertex-program shader) i 1))
            (c (funcall (shader-vertex-program shader) i 2)))
        (draw-triangle image shader a b c)))))


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
