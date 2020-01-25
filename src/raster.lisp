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


(defun draw-line (image u v)
  "Draw a line from point U to V on IMAGE."
  (let ((x0 (vec2-x u)) (y0 (vec2-y u))
        (x1 (vec2-x v)) (y1 (vec2-y v)))
    (if (< (abs (- y0 y1))
           (abs (- x0 x1)))
        (if (< x0 x1)
            (%draw-line-by-x-coords image x0 y0 x1 y1)
            (%draw-line-by-x-coords image x1 y1 x0 y0))
        (if (< y0 y1)
            (%draw-line-by-y-coords image x0 y0 x1 y1)
            (%draw-line-by-y-coords image x1 y1 x0 y0)))))


(defun set-pixel-color! (image col row color)
  (setf (aref image row col 0) (elt color 0))
  (setf (aref image row col 1) (elt color 1))
  (setf (aref image row col 2) (elt color 2)))


;;; Demo

(defvar *object-path* #P"/Users/erik/Desktop/african_head.obj")

(defun project (u width height)
  (let ((x (round (* (+ 1d0 (vec3-x u))
                     width
                     0.5)))
        (y (round (* (+ 1d0 (vec3-y u))
                     height
                     0.5))))
    ;; NOTE: We reflect y coordinates.
    (vec2 x (- height y))))

(defun render-scene (file &key (display t) (width 800) (height 800))
  (let* ((png (make-instance 'png
                             :width (1+ width)
                             :height (1+ height)))
         (image (data-array png))
         (obj (load-wavefront-object *object-path*)))
    (let ((*stroke-color* (list 255 255 255)))
      (flet ((draw-centered-line (u v)
               (let ((pu (project u width height))
                     (pv (project v width height)))
                 ;; TODO: round to [0,width) x [0,height)
                 (draw-line image pu pv))))
        (loop :for (ia ib ic) :across (wavefront-object-faces obj)
              :for a := (elt (wavefront-object-vertices obj) ia)
              :for b := (elt (wavefront-object-vertices obj) ib)
              :for c := (elt (wavefront-object-vertices obj) ic)
              :do (progn
                    (draw-centered-line a b)
                    (draw-centered-line b c)
                    (draw-centered-line c a)))))
    (write-png png file)
    (when display
      (sb-ext:run-program "/usr/bin/open" (list file)))))
