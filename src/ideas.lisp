(in-package :cl-user)
(defpackage :ideas
  (:use :cl :lucerne :ideas-models)
  (:export :app)
  (:documentation "Main ideas code."))
(in-package :ideas)
(annot:enable-annot-syntax)

;;; App

(defapp app
  :middlewares ((clack.middleware.static:<clack-middleware-static>
                 :root (asdf:system-relative-pathname :ideas #p"assets/")
                 :path "/static/")))

;;; Templates

(djula:add-template-directory
 (asdf:system-relative-pathname :ideas #p"templates/"))

(defparameter +index+ (djula:compile-template* "index.html"))
(defparameter +error+ (djula:compile-template* "index.html"))

;;
;;
;; routines
;;
;;

(defun render-index (&key first-idea second-idea)
  (render-template (+index+)
		   :first-idea first-idea
		   :second-idea second-idea))

(defun render-error (&key text)
  (render-template (+index+)
		   :text text))


;;; views

;;
;;
;; view random paar
;;
;;
@route app "/"
(defview index ()
  (handler-case (let* ((paar (get-random-ideas-paar))
		       (f-idea (idea-text (first-idea paar)))
		       (s-idea (idea-text (second-idea paar))))
		  (render-index :first-idea f-idea
				:second-idea s-idea))
    (storage-not-found (e) (let ((text (with-output-to-string (s)
					 (format s "hmm: ~a"
						 (storage-not-found-path e)))))
			     (render-error :text text)))
    (storage-is-empty () (let ((text "storage is empty"))
			   (render-error :text text)))))

;;
;;
;; create new one
;;
;;
@route app (:post "/")
(defview create-idea ()
  (with-params (new-idea)
    (let ((new-idea-o (make-instance 'idea :text new-idea)))
      (handler-case (progn
		      (validate new-idea-o)
		      (save-new-idea new-idea-o)
		      (redirect "/"))
	(validation-error (e) (render-error :text (validation-error-text e)))
	(idea-app-error () (render-error :text "Hmm..."))))))
