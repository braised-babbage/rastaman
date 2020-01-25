(in-package :rastaman)

(defparameter *stroke-color* (list 255 255 255))

(defun %draw-line-by-x-coords (image x0 y0 x1 y1)
  (loop :for x :from x0 :upto x1
        :for s := (/ (- x x0) (- x1 x0))
        :for y := (round
                   (+ (* y0 (- 1d0 s))
                      (* y1 s)))
        :do (set-pixel-color! image x y *stroke-color*)))

(defun %draw-line-by-y-coords (image x0 y0 x1 y1)
  (loop :for y :from y0 :upto y1
        :for s := (/ (- y y0) (- y1 y0))
        :for x := (round
                   (+ (* x0 (- 1d0 s))
                      (* x1 s)))
        :do (set-pixel-color! image x y *stroke-color*)))


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
                 (unless (and (= (vec2-x pu) (vec2-x pv))
                              (= (vec2-y pu) (vec2-y pv)))
                   (draw-line image pu pv)))))
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
