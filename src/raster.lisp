(in-package :rastaman)

(defparameter *stroke-color* (list 255 255 255))

(defun %draw-line-by-x-coords (image x0 y0 x1 y1)
  (assert (<= x0 x1))
  (let* ((dx (- x1 x0))
         (dy (- y1 y0))
         (derror2 (* 2 (abs dy)))
         (ystep (if (> y1 y0) 1 -1)))
    (loop :with y := y0
          :with error2 := 0
          :for x :from x0 :upto x1
          :do (set-pixel-color! image x y *stroke-color*)
          :do (progn
                (incf error2 derror2)
                (when (> error2 dx)
                  (incf y ystep)
                  (decf error2 (* 2 dx)))))))

(defun %draw-line-by-y-coords (image x0 y0 x1 y1)
  (assert (<= y0 y1))
  (let* ((dy (- y1 y0))
         (dx (- x1 x0))
         (derror2 (* 2 (abs dx)))
         (xstep (if (> x1 x0) 1 -1)))
    (loop :with x := x0
          :with error2 := 0
          :for y :from y0 :upto y1
          :do (set-pixel-color! image x y *stroke-color*)
          :do (progn
                (incf error2 derror2)
                (when (> error2 dy)
                  (incf x xstep)
                  (decf error2 (* 2 dy)))))))

;;; TODO: flip y coord
;;; TODO: clip to screen

(defvar *image-buffer)
(defvar *z-buffer*)
(defvar *world->screen*)
(defvar *draw-color*)

(defun draw-triangle (image a b c)
  ;; Note: now a,b,c are vec3f
  (check-type a vec3)
  (check-type b vec3)
  (check-type c vec3)
  (let ((pa (funcall *world->screen* a))
        (pb (funcall *world->screen* b))
        (pc (funcall *world->screen* c)))
    (flet ((draw-point (x y)
             (multiple-value-bind (q r s)
                 (barycentric-coordinates (vec2 x y) pa pb pc)
               (unless (or (minusp q) (minusp r) (minusp s))
                 ;; inside triangle, so draw it
                 (let ((z (+ (* q (vz3 a))
                             (* r (vz3 b))
                             (* s (vz3 c)))))
                   (when (> z (aref *z-buffer* x y))
                     (setf (aref *z-buffer* x y) z)
                     (set-pixel-color! image x y *draw-color*)))))))
      (let ((min-x (truncate (min (vx2 pa) (vx2 pb) (vx2 pc))))
            (max-x (truncate (max (vx2 pa) (vx2 pb) (vx2 pc))))
            (min-y (truncate (min (vy2 pa) (vy2 pb) (vy2 pc))))
            (max-y (truncate (max (vy2 pa) (vy2 pb) (vy2 pc)))))
        (loop :for x :from min-x :upto max-x
              :do (loop :for y :from min-y :upto max-y
                        :do (draw-point x y)))))))


(defun set-pixel-color! (image col row color)
  (setf (aref image row col 0) (elt color 0))
  (setf (aref image row col 1) (elt color 1))
  (setf (aref image row col 2) (elt color 2)))


;;; Demo

(defvar *object-path* #P"/Users/erik/Desktop/african_head.obj")

(defun color (r g b)
  (list (floor (min (* r 256) 255))
        (floor (min (* g 256) 255))
        (floor (min (* b 256) 255))))

(defun render-scene (file &key (display t) (width 800) (height 800))
  (let* ((png (make-instance 'png
                             :width (1+ width)
                             :height (1+ height)))
         (image (data-array png))
         (light-direction (vec3 0 0 -1))
         (obj (load-wavefront-object *object-path*)))
    (flet ((project (u)
             (let ((x (round (* (+ 1 (vx3 u))
                                width
                                0.5)))
                   (y (round (* (+ 1 (vy3 u))
                                height
                                0.5))))
               ;; NOTE: We reflect y coordinates.
               (vec2 x (- height y)))))
      (let ((*draw-color* (list 255 255 255))
            (*z-buffer* (make-array (list (1+ width) (1+ height))
                                    :element-type 'single-float
                                    :initial-element most-negative-double-float))
            (*world->screen* #'project))
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
                        (draw-triangle image a b c)))))))
    (write-png png file)
    (when display
      (sb-ext:run-program "/usr/bin/open" (list file)))))
