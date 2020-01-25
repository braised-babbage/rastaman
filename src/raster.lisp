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


(defun draw-line (image x0 y0 x1 y1)
  "Draw a line from (X0,Y0) to (X1,Y1) on IMAGE."
  (if (< (abs (- y0 y1)) (abs (- x0 x1)) )
      (if (< x0 x1)
          (%draw-line-by-x-coords image x0 y0 x1 y1)
          (%draw-line-by-x-coords image x1 y1 x0 y0))
      (if (< y0 y1)
          (%draw-line-by-y-coords image x0 y0 x1 y1)
          (%draw-line-by-y-coords image x1 y1 x0 y0))))


(defun set-pixel-color! (image col row color)
  (setf (aref image row col 0) (elt color 0))
  (setf (aref image row col 1) (elt color 1))
  (setf (aref image row col 2) (elt color 2)))


;;; Demo

(defvar *object-path* #P"/Users/erik/Desktop/african_head.obj")

(defun render-scene (file &key (display t) (width 800) (height 800))
  (let* ((png (make-instance 'png
                             :width (1+ width)
                             :height (1+ height)))
         (image (data-array png))
         (obj (load-wavefront-object *object-path*)))
    (let ((*stroke-color* (list 255 255 255)))
      (flet ((draw-centered-line (u v)
               (let ((x0 (round (* (+ (elt u 0) 1d0) width 0.5)))
                     (y0 (- height      ; Flip vertically
                            (round (* (+ (elt u 1) 1d0) height 0.5))))
                     (x1 (round (* (+ (elt v 0) 1d0) width 0.5)))
                     (y1 (- height
                            (round (* (+ (elt v 1) 1d0) height 0.5)))))
                 ;; TODO: round to [0,width) x [0,height)
                 (unless (and (= x0 x1) (= y0 y1))
                   (draw-line image x0 y0 x1 y1)))))
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
