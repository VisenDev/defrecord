(defpackage #:defrecord
  (:use #:cl)
  (:import-from #:closer-mop
                #:class-slots
                #:slot-definition-name
                #:slot-definition-initform
                #:slot-definition-type
                #:ensure-finalized)
  (:import-from #:alexandria
                #:symbolicate
                #:compose
                #:curry
                #:with-gensyms)
  (:export #:defrecord))
(in-package #:defrecord)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *defrecord-representation* :class)

  (defun get-slot-definition-form (slot)
    (list (slot-definition-name slot)
          (slot-definition-initform slot)
          :type
          (slot-definition-type slot)
          :documentation (documentation slot t)))

  (defun get-mixin-slot-definition-forms (mixins)
    (mapcar (compose #'ensure-finalized #'find-class) mixins)
    (mapcar #'get-slot-definition-form
            (mapcan (compose #'class-slots #'find-class) mixins)))

  (defun normalize-record-slot-definition (slot)
    (etypecase slot
      (symbol `(,slot nil))
      (cons slot)))

  (defun coerce-into-class-slot-definition (accessor-prefix record-slot-definition)
    (if (symbolp record-slot-definition)
        `(,record-slot-definition
          :accessor ,(symbolicate accessor-prefix record-slot-definition))
        (destructuring-bind
            (slot-name default-value &key type documentation)
            record-slot-definition
          `(,slot-name :accessor ,(symbolicate accessor-prefix slot-name)
                       :initform ,default-value
                       :type ,(or type t)
                       :documentation ,(or documentation ""))))))

(defmacro defrecord (name (&key mixins accessor-prefix) &body slots)
  (unless accessor-prefix
    (setf accessor-prefix (symbolicate name '-)))
  (let* ((normalized-slots (mapcar #'normalize-record-slot-definition slots))
         (mixin-slots (get-mixin-slot-definition-forms mixins))
         (all-slots (concatenate 'list normalized-slots mixin-slots))
         (instance (gensym))
         (macrolet-bindings
           (mapcar (lambda (slot)
                     `(,(first slot)
                       (,(symbolicate accessor-prefix
                                      (first slot))
                        ,Instance)))
                   slots)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defmacro ,(symbolicate 'with- name '-slots) (instance &body body)
         `(let ((,',instance ,instance))
           (symbol-macrolet ,',macrolet-bindings
              ,@body)))
       ,(ecase *defrecord-representation*
          (:struct
           `(defstruct ,name ,@all-slots))
          (:class
           (let ((class-slots
                   (mapcar (curry #'coerce-into-class-slot-definition
                                  accessor-prefix)
                           all-slots))
                 (keyword-args
                   (mapcar (lambda (slot) (list (first slot) (second slot)))
                           all-slots)))
             `(progn
                (defclass ,name () ,class-slots)
                (defun ,(symbolicate 'make- name)
                    (&key ,@keyword-args)
                  (let ((result (make-instance ',name)))
                    ,@(loop :for slot :in all-slots
                            :for slot-name = (first slot)
                            :collect `(setf
                                       (slot-value result ',slot-name)
                                       ,slot-name))
                    result)))))))))

(setf *defrecord-representation* :class)
(defrecord vec2 ()
  (x 0.0 :type float)
  (y 0.0 :type float))

(defrecord size ()
  (w 0.0 :type float)
  (h 0.0 :type float))

(defrecord rect (:mixins (vec2 size)))
