(asdf:defsystem rastaman
  :author "Erik Davis"
  :license "MIT"
  :description "A software rasterizer."
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "geometry")
               (:file "wavefront")
               (:file "graphics")
               (:file "shader")
               (:file "demo"))
  :depends-on (:alexandria
               :abstract-classes
               :targa
               :zpng
               :3d-vectors
               :3d-matrices))
