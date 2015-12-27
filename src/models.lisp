(in-package :cl-user)
(defpackage :ideas-models
  (:use :cl)
  (:documentation "Model for ideas app")
   ;; idea model
  (:export :idea
	   :idea-text)
  ;; ideas paar
  (:export :ideas-paar
	   :first-idea
	   :second-idea)
  ;; conditions
  (:export :storage-is-empty
	   :storage-not-found-path
	   :storage-not-found
	   :idea-text-is-empty
	   :validation-error
	   :validation-error-text
	   :idea-app-error)
  ;; functions
  (:export :get-random-ideas-paar
	   :validate
	   :save-new-idea))

(in-package :ideas-models)

(defclass idea ()
  ((text :accessor idea-text
	 :initarg :text
	 :type string))
  (:documentation "idea representation"))

(defclass ideas-paar ()
  ((first :accessor first-idea
	  :initarg :first
	  :type idea)
   (second :accessor second-idea
	   :initarg :second
	   :type idea))
  (:documentation "a paar of ideas representation"))

(defclass empty-ideas-paar ()
  ()
  (:documentation "empty paar"))

(defclass storage ()
  ((ideas :writer set-storage-ideas
	  :initform (list)
	  :type list)
   (is-loaded :reader storage-loaded-p
	      :writer storage-is-loaded
	      :initform nil
	      :type boolean))
  (:documentation "storage representation"))

;;
;;
;; conditions
;;
;;

(define-condition idea-app-error (error) ())

;; base condition for storage errors
(define-condition backend-error (idea-app-error) ())

(define-condition storage-not-found (backend-error)
  ((path :reader storage-not-found-path
	 :type pathname
	 :initarg :path)))

(define-condition storage-is-empty (backend-error) ())

(define-condition validation-error (idea-app-error)
  ((text :reader validation-error-text
	 :type string
	 :initform ""
	 :initarg :text)))

(define-condition idea-text-is-empty (validation-error)
  ((text :initform "idea's text is empty")))

;;
;;
;; params
;;
;;
(defparameter *storage* (make-instance 'storage))

(defparameter *storage-filename* #p"~/tmp/ideas-storage.txt")

;;
;; main getter
;;
(defun get-random-ideas-paar ()
  "gets random paar of ideas from storage"
  (make-instance 'ideas-paar
		 :first (get-random-idea *storage*)
		 :second (get-random-idea *storage*)))


;;
;;
;; main creator of new idea
;;
;;
(defgeneric save-new-idea (an-idea)
  (:documentation "saves new idea to storage file and syncs ideas with file"))

(defmethod save-new-idea ((an-idea idea))
  (add-idea *storage* an-idea)
  (save-storage *storage*))

;;
;;
;; validators
;;
;;
(defgeneric validate (obj)
  (:documentation "validator for idea"))

(defmethod validate ((an-idea idea))
  (validate-idea-text an-idea))

;;
;;
;; validate routines
;;
;;
(defun validate-idea-text (an-idea)
  (when (not (slot-boundp an-idea 'text))
    (error 'idea-text-is-empty))
  
  (when (= 0 (length (idea-text an-idea)))
    (error 'idea-text-is-empty))
  t)


;;
;; getter for ideas from storage
;;
(defgeneric storage-ideas (s)
  (:documentation "getter for ideas slot"))

(defmethod storage-ideas ((s storage))
  (when (not (storage-loaded-p s))
    (load-storage s))
  (slot-value s 'ideas))

;;
;; add new idea to storage
;;
(defgeneric add-idea (s i)
  (:documentation "add new idea to storage"))

(defmethod add-idea ((s storage) (i idea))
  (push i (slot-value s 'ideas)))

;;
;; loads storage
;;
(defgeneric load-storage (to-storage)
  (:documentation "load storage from file"))

(defmethod load-storage ((s storage))
  (handler-case (with-open-file (in *storage-filename*
		      :direction :input
		      :if-does-not-exist :error)
    (do ((line (read-line in nil)
	       (read-line in nil)))
	((null line))
      (add-idea s
		(make-instance 'idea :text line)))
    (setf (slot-value s 'is-loaded) t))
  (file-error () (error 'storage-not-found :path *storage-filename*))))


;;
;; saves storage
;;
(defgeneric save-storage (from-storage)
  (:documentation "saves storage to file"))

(defmethod save-storage ((s storage))
  (handler-case (with-open-file (out *storage-filename*
				     :direction :output
				     :if-exists :supersede)
		  (dolist (i (storage-ideas s))
		    (format out "~A~%" (idea-text i))))))

;;
;; gets random idea from storage
;;
(defgeneric get-random-idea (from-storage)
  (:documentation "gets random idea from storage"))

(defmethod get-random-idea ((s storage))
  (if (null (storage-ideas s))
      (error 'storage-is-empty)
      (nth (random (length (storage-ideas s)))
	   (storage-ideas s))))

;; (defun get-storage ()
;;   "get strage and if needed load it from file"
;;   (if (storage-loaded-p)
;;       *storage*
;;       (load-storage)))





