#+named-readtables
(named-readtables:in-readtable :standard)

(asdf:defsystem :com.andrewsoutar.bootstrap
  :description "Machinery for bootstrapping things"
  :version "0.1.0"
  :author ("Andrew Soutar <andrew@andrewsoutar.com>")
  :maintainer "Andrew Soutar <andrew@andrewsoutar.com>"
  :defsystem-depends-on (:asdf-package-system)
  :class :package-inferred-system
  :depends-on (:uiop :com.andrewsoutar.bootstrap/bootstrap))
