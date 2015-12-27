(in-package :cl-user)
(defpackage ideas-test
  (:use :cl :fiveam))
(in-package :ideas-test)

(def-suite tests
  :description "ideas tests.")
(in-suite tests)

(test simple-test
  (is
   (equal 1 1))
  (is-true
   (and t t)))

(run! 'tests)
