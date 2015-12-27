(defsystem ideas-test
  :author "nicola spesivcev <nicola.spesivcev@gmail.com>"
  :license "MIT"
  :description "Tests for ideas."
  :depends-on (:ideas
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "ideas")))))
