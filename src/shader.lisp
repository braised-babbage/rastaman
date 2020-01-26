(in-package :rastaman)

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
             (multiple-value-bind (v n)
                 (vertex-and-normal waveform-obj iface idx)
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
