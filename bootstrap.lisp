#+named-readtables
(named-readtables:in-readtable :standard)

(uiop:define-package :com.andrewsoutar.bootstrap
    (:documentation "Machinery for bootstrapping things")
  (:nicknames :com.andrewsoutar.bootstrap/bootstrap)
  (:use :cl)
  (:use :asdf :alexandria :anaphora :bordeaux-threads :named-readtables)
  (:import-from :uiop #:register-image-dump-hook #:*lisp-interaction*)
  (:import-from :asdf/system #:component-entry-point)
  (:use :com.andrewsoutar.brace-lambda)
  (:export #:defbootstrap #:boot #:build #:cleanup))
(in-package :com.andrewsoutar.bootstrap)

(in-readtable brace-lambda)

(defclass boot-form ()
  ((name :type symbol
         :initarg :name
         :accessor name)
   (init-fun :type (function ())
             :initform {() (values)}
             :accessor init-fun)
   (fini-fun :type (function)
             :initform {funcall 'values}
             :accessor fini-fun)
   (direct-deps :type list
                :initform nil
                :accessor direct-deps)
   (indirect-deps :type list
                  :initform nil
                  :accessor indirect-deps)
   (direct-rdeps :type list
                 :initform nil
                 :accessor direct-rdeps)
   (indirect-rdeps :type list
                   :initform nil
                   :accessor indirect-rdeps)

   (ephemeral :type boolean
              :initform nil
              :accessor ephemeral)

   (state :type (member :unloaded :loading :loaded :unloading)
          :initform :unloaded
          :accessor state)
   (data :initform nil
         :accessor data)))

(defmethod print-object ((object boot-form) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~S" (name object))))

(defvar *bootstrap-forms* (make-hash-table :test 'eq :weakness :key))

(defvar *accumulator*)

(defmacro modifying-boot-form ((var boot-form &key create) &body body)
  (once-only (boot-form)
    `(let ((,var (if (typep ,boot-form 'boot-form)
                     ,boot-form
                     ,(let ((getter `(gethash ,boot-form *bootstrap-forms*)))
                        (if create
                            `(sor ,getter
                                  (setf it (make-instance 'boot-form
                                                          :name ,boot-form)))
                            getter)))))
       ,@body)))

(defun walk (thing old-state intermediate-state new-state deps-thunk call-thunk)
  (modifying-boot-form (boot-form thing)
    (cond
      ((eq (state boot-form) old-state)
       (setf (state boot-form) intermediate-state)
       (let ((thunk (funcall call-thunk boot-form))
             (data (data boot-form))
             (end-state old-state))
         (unwind-protect
              (progn
                (mapcar {walk %* old-state intermediate-state
                              new-state deps-thunk call-thunk}
                        (funcall deps-thunk boot-form))
                (format t "~A ~S~%" intermediate-state boot-form)
                (setf (data boot-form)
                      (multiple-value-list (apply thunk data)))
                (setf end-state new-state))
           (setf (state boot-form) end-state)
           (if (eq end-state new-state)
               (push (name boot-form) *accumulator*)
               (format t "FAILURE: ~A ~S~%" intermediate-state boot-form)))))
      ((eq (state boot-form) intermediate-state)
       (error "Dependency cycle detected for form: ~S" boot-form))
      ((eq (state boot-form) new-state))
      (t (error "Confuzzled: found state ~A but we are ~A"
                (state boot-form) intermediate-state)))))

(defvar *bootstrap-mutex* (make-recursive-lock))

(macrolet ((shared (old-state intermediate-state new-state deptype fun)
             `(let ((*accumulator* ()))
                (with-recursive-lock-held (*bootstrap-mutex*)
                  (walk thing ,old-state ,intermediate-state ,new-state
                        {append (,(symbolicate 'direct- deptype) %*)
                                (,(symbolicate 'indirect- deptype) %*)}
                        ',fun))
                (nreverse *accumulator*))))
  (defun boot (thing)
    (shared :unloaded :loading :loaded deps init-fun))
  (defun cleanup (thing)
    (shared :loaded :unloading :unloaded rdeps fini-fun)))

(defun %defbootstrap (name init-fun fini-fun &key before after ephemeral)
  (let ((before (ensure-list before))
        (after (ensure-list after)))
    (with-recursive-lock-held (*bootstrap-mutex*)
      (modifying-boot-form (this-form name :create t)
        (let ((unloaded-modules (cleanup this-form)))
          (macrolet ((frob (place new existing)
                       `(progn
                          ,@ (mapcar
                              {((%1 %2 %3 &optional %4))
                               `(mapcar
                                 {modifying-boot-form (other-form %1
                                                       :create ,%4)
                                   ,(funcall %1 `(,place other-form) 'name)}
                                 (set-difference ,%2 ,%3))}
                              `((,{`(deletef ,%1 ,%2)} ,existing ,new)
                                (,{`(push ,%2 ,%1)} ,new ,existing t))))))
            (frob indirect-rdeps after (direct-deps this-form))
            (frob indirect-deps before (direct-rdeps this-form)))
          (when init-fun
            (setf (init-fun this-form) init-fun))
          (when fini-fun
            (setf (fini-fun this-form) fini-fun))
          (setf (direct-deps this-form) after
                (indirect-deps this-form) before
                (ephemeral this-form) ephemeral)
          (mapcar 'boot unloaded-modules))))))

(defmacro defbootstrap (name (&rest options) &body body)
  `(%defbootstrap ',name ,(first body) ,(second body)
                  ,@ (mapcar {`',%1} options)))

(defun cleanup-ephemeral ()
  (mapcar {progn (cleanup %1) (setf (data %1) nil)}
          (remove-if-not 'ephemeral
                         (hash-table-values *bootstrap-forms*))))

(defun cleanup-all ()
  (mapcar 'cleanup (hash-table-values *bootstrap-forms*)))

(register-image-dump-hook 'cleanup-ephemeral)

#+sbcl (pushnew 'cleanup-all sb-ext:*exit-hooks*)

(defclass bootstrap-system (package-inferred-system)
  ((main :initarg :main
         :initform :main
         :reader system-main)))

(defmethod component-entry-point ((component bootstrap-system))
  (let* ((main (system-main component))
         (main-bootstrap
           (cond
             ((and (symbolp main)
                   (not (keywordp main))
                   (not (null (symbol-package main))))
              main)
             (t (apply {find-symbol (string %1) %2}
                       (if (consp main)
                           `(,(second main) ,(first main))
                           `(,main ,(string-upcase
                                     (component-name component)))))))))
    (if main-bootstrap
        {unwind-protect
             (progn
               (com.andrewsoutar.bootstrap:boot main-bootstrap)
               (loop (sleep 36400)))
          (com.andrewsoutar.bootstrap::cleanup-all)}
        (error "Unable to find entry point ~S for ~S" main component))))

(defmethod perform :before ((o program-op) (c bootstrap-system))
  (setf *lisp-interaction* nil))

(setf (find-class 'asdf::bootstrap-system) (find-class 'bootstrap-system))
