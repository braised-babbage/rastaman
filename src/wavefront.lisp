;;; Wavefront OBJ reader
;;;
;;; This contains routines for reading Wavefront OBJ files. These
;;; are not intended to be fully general, but rather to support a
;;; basic subset of features.

(in-package :rastaman)

(defstruct wavefront-object
  vertices
  faces
  normals
  texture-coordinates)

(defstruct wavefront-face
  vertex-indices
  texture-indices)

(defun %parse-actual-vertex (line pos w)
  (multiple-value-bind (x pos)
      (read-from-string line t nil :start pos)
    (check-type x real)
    (multiple-value-bind (y pos)
        (read-from-string line t nil :start pos)
      (check-type y real)
      (multiple-value-bind (z pos)
          (read-from-string line t nil :start pos)
        (check-type z real)
        (values (vec x y z w)
                pos)))))

(defun parse-vertex (line &optional (pos 0))
  "Parse a vertex froma LINE, starting at position POS.

LINE should be a string containing something like
  v 0.000283538 -1 0.286843
  ^
  start position
where the three numbers represent the x,y,z coordinates of the vertex.

Returns a parsed vertex and the next position in the string."
  (unless (char= #\v (elt line pos))
    (error "Expected a 'v' at start of vertex definition line, but got ~A instead." line))
  (%parse-actual-vertex line (1+ pos) 1.0))

(defun parse-normal (line &optional (pos 0))
  (unless (string= "vn" (subseq line pos (+ 2 pos)))
    (error "Expected a 'vn' at start of vertex definition line, but got ~A instead." line))
  (%parse-actual-vertex line (+ 2 pos) 0.0))

(defun parse-texture-coordinate (line &optional (pos 0))
  (unless (string= "vt" (subseq line pos (+ 2 pos)))
    (error "Expected a 'vt' at start of texture coordinate definition, but got ~A instead." line))
  (vxy (%parse-actual-vertex line (+ 2 pos) 0.0)))

(defun parse-face (line &optional (pos 0))
  "Parse a face froma LINE, starting at position POS.

LINE should be a string containing something like
  f 986/1014/986 844/865/844 889/909/889
  ^
  start position
where the triples i/<omitted>/<omitted> represent indices into the corresponding vertex list.

Returns a parsed face and the next position in the string."
  (flet ((parse-index (pos)
           (multiple-value-bind (iv pos)
               (parse-integer line :start pos :junk-allowed t)
             (multiple-value-bind (tv pos)
                 (parse-integer line :start (1+ pos) :junk-allowed t)
               (multiple-value-bind (_ pos)
                   (parse-integer line :Start (1+ pos) :junk-allowed t)
                 (declare (ignore _))
                 (values (1- iv) (1- tv) pos))))))
    (unless (char= #\f (elt line pos))
      (error "Expected a 'f' in face definition line, but got ~A instead" (elt line pos)))
    (multiple-value-bind (ia ta pos)
        (parse-index (1+ pos))
      (multiple-value-bind (ib tb pos)
          (parse-index pos)
        (multiple-value-bind (ic tc pos)
            (parse-index pos)
          (values (make-wavefront-face :vertex-indices (vector ia ib ic)
                                       :texture-indices (vector ta tb tc))
                  pos))))))


(defun load-wavefront-object (path)
  "Load a Wavefront OBJ file from the specified PATH."
  (let ((vertices nil)
        (faces nil)
        (normals nil)
        (texture-coords nil))
    (with-open-file (stream path)
      (loop :for line := (read-line stream nil)
            :while line
            :do (cond ((starts-with-subseq "v " line)
                       (push (parse-vertex line) vertices))
                      ((starts-with-subseq "vn " line)
                       (push (parse-normal line) normals))
                      ((starts-with-subseq "vt " line)
                       (push (parse-texture-coordinate line) texture-coords))
                      ((starts-with-subseq "f " line)
                       (push (parse-face line) faces)))))
    (make-wavefront-object
     :vertices (map 'vector #'identity (nreverse vertices))
     :normals (map 'vector #'identity (nreverse normals))
     :texture-coordinates (map 'vector #'identity (nreverse texture-coords))
     :faces (map 'vector #'identity (nreverse faces)))))


;;;

(defun vertex-data (obj iface idx)
  (let* ((face (elt (wavefront-object-faces obj) iface))
         (ivertex (elt (wavefront-face-vertex-indices face) idx))
         (itexture (elt (wavefront-face-texture-indices face) idx)))
    (values
     (elt (wavefront-object-vertices obj) ivertex)
     (elt (wavefront-object-normals obj) ivertex)
     (elt (wavefront-object-texture-coordinates obj) itexture))))
