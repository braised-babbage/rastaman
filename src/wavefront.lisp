;;; Wavefront OBJ reader
;;;
;;; This contains routines for reading Wavefront OBJ files. These
;;; are not intended to be fully general, but rather to support a
;;; basic subset of features.

(in-package :rastaman)

(defstruct wavefront-object
  vertices
  faces)

(defun parse-vertex (line &optional (pos 0))
  "Parse a vertex froma LINE, starting at position POS.

LINE should be a string containing something like
  v 0.000283538 -1 0.286843
  ^
  start position
where the three numbers represent the x,y,z coordinates of the vertex.

Returns a parsed vertex and the next position in the string."
  (unless (char= #\v (elt line pos))
    (error "Expected a 'v' at start of vertex definition line, but got ~A instead" (elt line pos)))
  (multiple-value-bind (x pos)
      (read-from-string line t nil :start (1+ pos))
    (check-type x real)
    (multiple-value-bind (y pos)
        (read-from-string line t nil :start pos)
      (check-type y real)
      (multiple-value-bind (z pos)
          (read-from-string line t nil :start pos)
        (check-type z real)
        (values (vec3 x y z)
                pos)))))

(defun parse-face (line &optional (pos 0))
  "Parse a face froma LINE, starting at position POS.

LINE should be a string containing something like
  f 986/1014/986 844/865/844 889/909/889
  ^
  start position
where the triples i/<omitted>/<omitted> represent indices into the corresponding vertex list.

Returns a parsed face and the next position in the string."
  (flet ((parse-index (pos)
           (multiple-value-bind (i pos)
               (parse-integer line :start pos :junk-allowed t)
             (multiple-value-bind (_ pos)
                 (parse-integer line :start (1+ pos) :junk-allowed t)
               (declare (ignore _))
               (multiple-value-bind (_ pos)
                   (parse-integer line :Start (1+ pos) :junk-allowed t)
                 (declare (ignore _))
                 (values (1- i) pos))))))
    (unless (char= #\f (elt line pos))
      (error "Expected a 'f' in face definition line, but got ~A instead" (elt line pos)))
    (multiple-value-bind (ia pos)
        (parse-index (1+ pos))
      (multiple-value-bind (ib pos)
          (parse-index pos)
        (multiple-value-bind (ic pos)
            (parse-index pos)
          (values (list ia ib ic) pos))))))

(defun load-wavefront-object (path)
  "Load a Wavefront OBJ file from the specified PATH."
  (let ((vertices nil)
        (faces nil))
    (with-open-file (stream path)
      (loop :for line := (read-line stream nil)
            :while line
            :do (when (and (< 1 (length line))
                           (char= #\Space (elt line 1)))
                  (case (elt line 0)
                    (#\v (push (parse-vertex line) vertices))
                    (#\f (push (parse-face line) faces))))))
    (make-wavefront-object :vertices (map 'vector #'identity (nreverse vertices))
                           :faces (map 'vector #'identity (nreverse faces)))))
