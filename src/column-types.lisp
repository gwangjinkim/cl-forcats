(in-package #:cl-forcats)

(defmethod cl-vctrs-lite:column-p ((x factor)) t)
(defmethod cl-vctrs-lite:as-col ((x factor)) x)
(defmethod cl-vctrs-lite:value-type ((x factor)) :factor)
(defmethod cl-vctrs-lite:col-type ((x factor)) :factor)
(defmethod cl-vctrs-lite:col-length ((x factor)) (length (factor-data x)))
(defmethod cl-vctrs-lite:col-ref ((x factor) i)
  (unless (and (integerp i) (<= 0 i) (< i (length (factor-data x))))
    (error "col-ref: index ~s out of range for factor length ~d" i (length (factor-data x))))
  (let ((code (aref (factor-data x) i)))
    (if (or (null code) (eql code 0) (cl-vctrs-lite:na-p code))
        cl-vctrs-lite:*na*
        (aref (factor-levels x) (1- code)))))
(defmethod cl-vctrs-lite:col->list ((x factor))
  (loop for i below (cl-vctrs-lite:col-length x) collect (cl-vctrs-lite:col-ref x i)))
(defmethod cl-vctrs-lite:vec-prototype ((x factor))
  (cl-vctrs-lite:make-prototype :factor :levels (factor-levels x) :ordered (factor-ordered x)))

(defmethod cl-vctrs-lite:restore-column :around ((values vector) (prototype cl-vctrs-lite:column-prototype))
  (if (eq (cl-vctrs-lite:prototype-type prototype) :factor)
      (let* ((levels (or (cl-vctrs-lite:prototype-levels prototype)
                         (remove-duplicates (remove cl-vctrs-lite:*na* (coerce values 'list))
                                            :test #'equal :from-end t)))
             (codes (map 'vector
                         (lambda (x)
                           (if (cl-vctrs-lite:na-p x) 0
                               (let ((index (position x levels :test #'equal)))
                                 (unless index
                                   (error 'cl-vctrs-lite:cast-error :from :string :to :factor :value x))
                                 (1+ index)))) values)))
        (make-factor codes :levels levels :ordered (cl-vctrs-lite:prototype-ordered prototype)))
      (call-next-method)))
