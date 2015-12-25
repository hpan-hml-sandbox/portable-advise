;; kenobi - portable advise.lisp

(in-package #:cl-user)

(deftype function-name ()
  '(or symbol (cons symbol t)))

;; trivial portable prototype of an 'advise' interface

(defstruct (advise-record
             (:conc-name #:advised-)
             (:constructor
              make-advise-record (name env original-function)))
  (original-function
   #'(lambda ()
       (error "Default advised-original-function"))
   :type function)
  ;; NB: no portable 'environment' type.
  ;; 'env' slot may only be useful for debugging
  (env)
  (name #:unnamed
        :type function-name))

(defconstant %advise-expand% 8)

(defvar %advise-rec%
  (make-array %advise-expand%
              :element-type 'advise-record
              :adjustable t
              :fill-pointer 0))

(defun get-advise (name env &optional (errorp t))
  (declare (type function-name name)
           (ignore env) ;; FIXME
           (values (or null advise-record)))
  ;; FIXME: lock %ADVISE-REC% (READ, NON-RECURSIVE) in this function
  (labels ((test (a b)
             (etypecase a
               (symbol (and (symbolp b)
                            (eq a b)))
               (cons (and (consp b)
                          (test (car a) (car b))
                          (test (cdr a) (cdr b)))))))
    ;; (test '(a a) '(a b)) ;; => NIL
    ;; (test '(a a) '(a a)) ;; => T
    ;; (test '(a a a) '(a a b)) ;; => NIL
    ;; (test '(a a b) '(a a b)) ;; => T
    (let ((o (find name %advise-rec%
                   :test (etypecase name
                           (symbol #'eq)
                           (cons #'test))
                   :key #'advised-name)))
      (cond
        (o (values o))
        (errorp (error "ADVISE-RECORD NOT FOUND: ~S" name))
        (t (values nil nil))))))

(defun register-advise (name env form)
  ;; FIXME: lock %ADVISE-REC% (WRITE) in this function
  (let ((adv (get-advise name env nil)))
    (cond
      ((and adv (not (eq form (advised-original-function adv))))
       (error "ALREAY ADVISED: ~S => ~S" name adv))
      (t (let ((adv (make-advise-record name env form)))
           (vector-push-extend adv %advise-rec% %advise-expand%)
           (values adv))))))

(defun unregister-advise (name env)
  ;; FIXME: lock %ADVISE-REC% (WRITE) in this function
  (let ((adv (get-advise name env nil)))
    (cond
      (adv
       (setf %advise-rec% (delete adv %advise-rec% :test #'eq))
       (values adv))
      (t (error "NOT ADVISED: ~S" name)))))


  

(defmacro defadvise-macro (name lambda &key before after &environment env)
  ;; FIXME: This does not destructure LAMBDA for the BEFORE, AFTER functions,
  ;;        and neither for the call to the advised macro function.

  (let ((%name (gensym "%name-"))
        (%env (gensym "%env-"))
        (%form (gensym "%form-"))
        )
    
    `(let* ((,%name (quote ,name))
            (,%env ,env)
            (,%form
             (eval (function-lambda-expression
                    ;; FIXME: ^ THIS might not always "work out"
                    (macro-function ,%name ,%env)))))

       ;; FIXME: store ,%FORM on hash of (,%NAME ,%ENV)
       (register-advise ,%name ,%env ,%form)
           
       (defmacro ,name ,lambda
         ,@(when before
                 `((funcall ,before ,@lambda)))
         (unwind-protect
              (funcall (eval ,%form)
                       (list ,%name ,@lambda)
                       ,%env)
           ,@(when after
                   `((funcall ,after  ,@lambda))))))))


(defmacro unadvise-macro (name &environment env)
  (let ((%name (gensym "%name-"))
        (%adv (gensym "%adv-"))
        (%fn (gensym "%fn-")))
    `(let* ((,%name (quote ,name))
            (,%adv (unregister-advise ,%name ,env))
            (,%fn (advised-original-function ,%adv)))
       (setf (macro-function ,%name) ,%fn)
       (compile ,%name ,%fn)
       (values ,%fn))))

#|

(defmacro frob-trace (ctrl &rest args)
  `(progn 
     (write-char #\Newline *trace-output*)
     (format *trace-output* ,ctrl ,@args)
     (write-char #\Newline *trace-output*)
     (finish-output *trace-output*)))

(defmacro quux (arg)
  `(progn (frob-trace "GOT ARG QUUX ~S" ,arg)
          (list :quux ,arg)))

#+test (quux 5)

(defadvise-macro quux (%arg)
  :before (lambda (%argtoo)
            (frob-trace "GOT ARGTOO BEFORE ~s" %argtoo))
  :after  (lambda (%argtoo)
            (frob-trace "GOT ARGTOO AFTER ~s" %argtoo))
  )
             

#+test (quux 12)
;; ^FIXME: *trace-output* shows order of eval: :BEFORE, :AFTER, :DURING

(unadvise-macro quux)

#+test (quux 28)



|#

;; Prototypes



#+NIL
(defmacro compute-defkind (name &environment env)
  ;; endeavor to compute a discrete 'kind' of a function's definition
  ;; 
  ;; NB: This does not compute 'definitino source kind' - e.g.
  ;; whether a function is defined of an accessor, constructor, or
  ;; other non-DEFUN source - such that may serve to require an
  ;; implementation-specific reflection, and may require more than
  ;; only a function's name, to compute.
  (let ((%name (gensym "%name-"))
        (%env (gensym "%env-"))
        (%fn (gensym "%fn-")))
    `(let ((,%name (quote ,name))
           (,%env ,env))
       (declare (type (or symbol (cons symbol t)) ,%name))
       (cond
         ((macro-function ,%name ,%env)
          (values 'defmacro))
         ((and (consp ,%name)
               (eq (car ,%name) 'setf))
          ;; ??
          (values 'defsetf))
         ((and (consp ,%name)
               (eq (car ,%name) 'lambda))
          ;; ??
          (values 'lambda))
         (t
          (let ((,%fn (fdefinition ,%name)))
            (typecase ,%fn
              (generic-function
               (values 'defgeneric))
              (standard-object ;; funcallable-standard-object portably
               ;; ??
               (values 'defclass))
              (function
               (values 'defun))
              (t
               (values :unknown)))))))))
