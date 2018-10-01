;;; chn-lib --- Component compounds of a wild law

(use-package dash)
(use-package dash-functional)
(use-package s)
(use-package f)

(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

(defun file-if-exists (path)
  (if (file-exists-p path) path nil))


(provide 'chn-lib)
