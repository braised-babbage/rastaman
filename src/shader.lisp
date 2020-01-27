(in-package :rastaman)

;;; Shaders
;;;
;;; A shader consists of three "programs":
;;;   1. INIT-PROGRAM, used to set up whatever states is needed.
;;;   2. VERTEX-PROGRAM, which (at a minimum) is responsible for transforming
;;;      the vertices of a triangle from world coordinates to screen coordinates.
;;;   3. FRAGMENT-PROGRAM, which is responsible for assigning a color to a
;;;      point expressed in barycentric coordinates.
;;;
;;; These three programs may operate on some shared state, which is assumed to exist
;;; on a per-triangle basis. Single threaded code may get by with a single shader
;;; shared across triangles (via INIT-PROGRAM), but conceptually one may consider
;;; each triangle to have its own independent shader program. 

(defstruct shader
  init-program
  vertex-program
  fragment-program)

(defun make-gouraud-shader (waveform-obj)
  (let (varying-intensity
        transform)
    (flet ((init-program ()
             (setf varying-intensity #(0.0 0.0 0.0))
             (setf transform (m*
                              *viewport-matrix*
                              *projection-matrix*
                              *modelview-matrix*)))

           (vertex-program (iface idx)
             (multiple-value-bind (v n _)
                 (vertex-data waveform-obj iface idx)
               (declare (ignore _))
               (setf (aref varying-intensity idx)
                     (max 0.0 (dot-product n *light-dir*)))
               (v4->v3 (m* transform v))))

           (fragment-program (q r s)
             (let ((intensity (+ (* (aref varying-intensity 0) q)
                                 (* (aref varying-intensity 1) r)
                                 (* (aref varying-intensity 2) s))))
               (values intensity intensity intensity))))
      (make-shader :init-program #'init-program
                   :vertex-program #'vertex-program
                   :fragment-program #'fragment-program))))



(defun mix (elts q r s)
  (+ (* (aref elts 0) q)
     (* (aref elts 1) r)
     (* (aref elts 2) s)))


(defun make-textured-gouraud-shader (waveform-obj tga)
  (let (varying-intensity
        varying-tex-u
        varying-tex-v
        transform)
    (labels ((color (u v)
               (let* ((header (targa:tga-header tga))
                      (width (targa:tga-header-width header))
                      (height (targa:tga-header-height header))
                      (x (min (1- width) (round (* u width))))
                      (y (min (1- height) (round (* (- 1 v) height)))))
                 (values-list (targa:tga-get-pixel tga x y))))

             (init-program ()
               (setf varying-intensity #(0.0 0.0 0.0))
               (setf varying-tex-u #(0.0 0.0 0.0))
               (setf varying-tex-v #(0.0 0.0 0.0))
               (setf transform (m*
                                *viewport-matrix*
                                *projection-matrix*
                                *modelview-matrix*)))

             (vertex-program (iface idx)
               (multiple-value-bind (v n tex)
                   (vertex-data waveform-obj iface idx)
                 (setf (aref varying-intensity idx)
                       (max 0.0 (dot-product n *light-dir*)))
                 (setf (aref varying-tex-u idx) (vx2 tex))
                 (setf (aref varying-tex-v idx) (vy2 tex))
                 (dehomogenize (m* transform v))))

             (fragment-program (q r s)
               (let ((intensity (mix varying-intensity q r s))
                     (u (mix varying-tex-u q r s))
                     (v (mix varying-tex-v q r s)))
                 (multiple-value-bind (r g b a)
                     (color u v)
                   (declare (ignore a))
                   (values (* intensity r 1/255)
                           (* intensity g 1/255)
                           (* intensity b 1/255))))))
      (make-shader :init-program #'init-program
                   :vertex-program #'vertex-program
                   :fragment-program #'fragment-program))))
