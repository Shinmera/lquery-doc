#|
  This file is a part of lQuery-Doc
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.lib.lquery.doc.asd
  (:use :cl :asdf))
(in-package :org.tymoonnext.radiance.lib.lquery.doc.asd)

(defsystem lquery-doc
  :name "lQuery-Doc"
  :version "1.1.0"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "Artistic"
  :description "An lQuery extension to easily create documentation for packages."
  :components ((:file "lquery-doc"))
  :depends-on (:lquery :alexandria :closer-mop))
