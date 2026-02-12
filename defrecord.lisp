(defpackage #:defrecord
  (:use #:cl)
  (:import-from #:closer-mop
                #:class-slots
                #:slot-definition-name
                #:slot-definition-initform
                #:slot-definition-type
                #:slot-definition-doc)
  (:import-from #:alexandria
                #:symbolicate)
  (:export #:defrecord))
(in-package #:defrecord)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *defrecord-representation* :class)

  (defun construct-slot-definition-form (slot)
    (list (slot-definition-name slot)
          (slot-definition-initform slot)
          :type
          (slot-definition-type slot)
          :documentation
          (slot-definition-documentation))
    )

  (defun get-slot-forms (mixins)
    ()
    )
  )
(defmacro defrecord (name (&key includes) &body slots)
  (ecase *defrecord-representation*
    (:struct
     `(defstruct
          ,name ,@slots
          ,@(mapcan (lambda (parent)
                      (mapcar (lambda (slot)
                                `(,(slot-definition-name slot)
                                  ,(slot-definition-initform slot)
                                  :type
                                  ,(slot-definition-type slot)))
                              (class-slots (find-class parent))))
             includes)))

    (:class
     `(progn (defclass ,name ,includes
               ,(loop :for slot :in slots
                      :collect
                      (etypecase slot
                        (symbol slot)
                        (cons
                         (destructuring-bind
                             (slot-name default-value &key type documentation)
                             slot
                           `(,slot-name :accessor ,(symbolicate name '- slot-name)
                                        :initform ,default-value
                                        :type ,(or type t)
                                        :documentation ,(or documentation ""))
                           )))))
             (defun ,(symbolicate 'make- name)
                 (&key
                    ,@(mapcar (lambda (slot)
                                `(,(slot-definition-name slot)
                                  ,(slot-definition-initform slot)))
                              (mapcan #'class-slots includes))
                    ,@(mapcar (lambda (slot)
                                (destructuring-bind
                                    (slot-name default-value &rest _)
                                    slot
                                  (declare (ignore _))
                                  `(,slot-name ,default-value)))
                              slots))
               (let ((result (make-instance ',name)))
                 (loop :for (name value) :in args :by #'cddr
                       :do (setf (slot-value result name) value))
                 result))))))

(setf *defrecord-representation* :class)
(defrecord vec2 ()
  (x 0.0 :type float)
  (y 0.0 :type float))

(defrecord size ()
  (w 0.0 :type float)
  (h 0.0 :type float))

(defrecord rect (:includes (vec2 size)))
