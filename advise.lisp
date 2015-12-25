;; kenobi - portable advise.lisp

(in-package #:cl-user)

;; trivial portable 'advise' interface

(defvar %advise% (make-hash-table :test 'eq))

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
             (function-lambda-expression (macro-function ,%name ,%env))))

       ;; FIXME: store ,%FORM on hash of (,%NAME ,%ENV)
       (setf (gethash ,%name  %advise%)
             ,%form)
           
       (defmacro ,name ,lambda
         ,@(when before
                 `((funcall ,before ,@lambda)))
         (unwind-protect
              (funcall (eval ,%form)
                       (list ,%name ,@lambda)
                       ,%env)
           ,@(when after
                   `((funcall ,after  ,@lambda))))))))


(defmacro unadvise (name &environment env)
  (declare (ignore env))
  (let ((%name (gensym "%name-"))
        (%adv (gensym "%adv-"))
        (%undef (gensym "%undef-"))
        )
    `(let* ((,%name (quote ,name))
            (,%undef (gensym "%_%undef"))
            (,%adv (gethash ,%name %advise% ,%undef)))
       (cond
         ((eq ,%adv ,%undef)
          (warn "~S not advised" ,%name))
         (t
          (remhash ,%name %advise%)
          (let ((,%adv (eval ,%adv)))
            (setf (macro-function ,%name) ,%adv)
            (compile ,%name ,%adv)))
         ))))

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

(unadvise quux)

#+test (quux 28)

(macroexpand (quote
(defadvise-macro quux (%arg)
  :before (lambda (%argtoo)
            (warn "GOT ARGTOO BEFORE ~s" %argtoo))
  :after  (lambda (%argtoo)
            (warn "GOT ARGTOO AFTER ~s" %argtoo))
  )
))

|#

;; Prototypes

#+NIL
(deftype function-name ()
  '(or symbol (cons symbol t)))

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
