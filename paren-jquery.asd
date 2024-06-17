(defsystem paren-jquery
  :version "0.2.0"
  :author "Dmitrii Kosenkov"
  :license "MIT"
  :depends-on ("parenscript")
  :description "Parenscript macros for jQuery"
  :homepage "https://github.com/Junker/paren-jquery"
  :source-control (:git "https://github.com/Junker/paren-jquery.git")
  :components ((:file "package")
               (:file "paren-jquery")))
