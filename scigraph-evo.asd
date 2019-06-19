
(defsystem #:scigraph-evo
    :depends-on (#:mcclim #:scigraph-evo/dwim)
    :serial t
    :components ((:file "scigraph/package")
                 (:file "scigraph/random")
                 (:file "scigraph/menu-tools")
                 (:file "scigraph/basic-classes")
                 (:file "scigraph/draw")
                 (:file "scigraph/mouse")
                 (:file "scigraph/color")
                 (:file "scigraph/basic-graph")
                 (:file "scigraph/graph-mixins")
                 (:file "scigraph/axis")
                 (:file "scigraph/moving-object")
                 (:file "scigraph/symbol")
                 (:file "scigraph/graph-data")
                 (:file "scigraph/legend")
                 (:file "scigraph/graph-classes")
                 (:file "scigraph/present")
                 (:file "scigraph/annotations")
                 (:file "scigraph/annotated-graph")
                 (:file "scigraph/contour")
                 (:file "scigraph/equation")
                 (:file "scigraph/popup-accept")
                 (:file "scigraph/popup-accept-methods")
                 (:file "scigraph/frame")
                 (:file "scigraph/demo-frame")
                 (:file "scigraph/showcase")))

(defsystem #:scigraph-evo/dwim
  :depends-on (#:mcclim)
  :components ((:file "dwim/package")
               (:file "dwim/macros")
               (:file "dwim/tv")               
               (:file "dwim/present")
               (:file "dwim/extensions")
               (:file "dwim/wholine")))

(defsystem #:scigraph-evo/contrib
  :depends-on (#:scigraph-evo #:local-time)
  :serial t
  :components ((:file "contrib/timestamp")
               (:file "contrib/pie")))
