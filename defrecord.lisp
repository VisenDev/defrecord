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
  (:export #:defrecord
           #:*record-representation*))
(in-package #:defrecord)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *record-representation* :class) ;; can be :class or :struct

  (defun get-mop-slot-definition-form (mop-slot)
    (list (slot-definition-name mop-slot)
          (slot-definition-initform mop-slot)
          :type
          (slot-definition-type mop-slot)))

  (defun get-mixin-slot-definition-forms (mixins)
    (mapcar (compose #'ensure-finalized #'find-class) mixins)
    (mapcar #'get-mop-slot-definition-form
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
            (slot-name default-value &key type)
            record-slot-definition
          `(,slot-name :accessor ,(symbolicate accessor-prefix slot-name)
                       :initform ,default-value
                       :type ,(or type t)))))

  (defun generate-class-slot-definitions (accessor-prefix slots)
    (mapcar (curry #'coerce-into-class-slot-definition
                   accessor-prefix)
            slots))

  (defun generate-make-function-keyword-args (slots)
    (mapcar (lambda (slot) (list (first slot) (second slot)))
            slots)))

(defmacro defrecord (name
                     (&key mixins accessor-prefix
                        (representation *record-representation*))
                     &body slots)
  (unless accessor-prefix
    (setf accessor-prefix (symbolicate name '-)))

  ;; Local Variables
  (let* ((normalized-slots (mapcar #'normalize-record-slot-definition slots))
         (mixin-slots (get-mixin-slot-definition-forms mixins))
         (all-slots (concatenate 'list normalized-slots mixin-slots))
         (make-function-name (symbolicate 'make- name)))

    ;; The Generated Form
    `(progn

       ;; Swap between Record Representation Types
       ,(ecase representation

          ;; Struct
          (:struct
           `(eval-when (:compile-toplevel :load-toplevel :execute)
              (defstruct (,name (:conc-name ,accessor-prefix)) ,@all-slots)))

          ;; Class
          (:class
           `(progn
              (eval-when (:compile-toplevel :load-toplevel :execute)
                (defclass ,name () ,(generate-class-slot-definitions accessor-prefix all-slots)))

              ;; Class Make Function
              (defun ,make-function-name (&key ,@(generate-make-function-keyword-args all-slots))
                (let ((result (make-instance ',name)))
                  ,@(loop :for slot :in all-slots
                          :for slot-name = (first slot)
                          :collect `(setf
                                     (slot-value result ',slot-name)
                                     ,slot-name))
                  result))))))))


;;; DEMO
(defrecord vec2 (:accessor-prefix vec-)
  (x 0.0 :type float)
  (y 0.0 :type float))

(defrecord size ()
  (w 0.0 :type float)
  (h 0.0 :type float))

(defrecord rectangle (:mixins (vec2 size)
                      :accessor-prefix rect-
                      :representation :struct))

