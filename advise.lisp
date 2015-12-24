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
