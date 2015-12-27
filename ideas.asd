;; (load #p"~/quicklisp/setup.lisp")
(ql:quickload "asdf-linguist" :silent t)

(asdf:defsystem #:ideas
  :author "nicola spesivcev <nicola.spesivcev@gmail.com>"
  :maintainer "nicola spesivcev <nicola.spesivcev@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage "https://github.com/eehah5ru/ideas"
  ;; :bug-tracker "https://github.com/eehah5ru/ideas/issues"
  ;; :source-control (:git "git@github.com:eehah5ru/ideas.git")
  :depends-on (:lucerne)
  :defsystem-depends-on (:asdf-linguist)
  :components ((:module "assets"
                :components
                ((:module "css"
                  :components
                  ((:sass "style")))
                 (:module "js"
                  :components
                  ((:static-file "scripts.js")))))
               (:module "src"
                :serial t
                :components
                ((:file "models")
		 (:file "ideas"))))
  :description "ideas permutator"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op ideas-test))))

