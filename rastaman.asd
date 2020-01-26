(asdf:defsystem rastaman
  :author "Erik Davis"
  :license "MIT"
  :description "A software rasterizer."
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "geometry")
               (:file "wavefront")
               (:file "raster"))
  :depends-on (:alexandria
               :zpng
               :3d-vectors
               :3d-matrices))
