(defun day-15-2020-1 ()
  (play-it (input) 2020))

(defun day-15-2020-2 ()
  (play-it (input) 30000000))

(defun play-it (input repeat)
  (destructuring-bind (start-value hash) (get-last-value-and-hash input)
    (loop repeat (- repeat (length input)) with value = start-value with start = (1- (length input))
          for last-index = (last-spoken-index hash value)
          count 1 into step
          do (setf (gethash value hash) (+ start step))
             (setf value (if (= 0 last-index) last-index (- (+ start step) last-index)))
          finally (return value))))

(defun last-spoken-index (hash value)
  (let ((last (gethash value hash)))
    (if (null last) 0 last)))

(defun get-last-value-and-hash (list)
  (let ((hash (make-hash-table :test #'equal)))
    (loop for i in (butlast list)
          count 1 into index
          do (setf (gethash i hash) index)
          finally (return (list (first (last list)) hash)))))

(defun input () '(11 0 1 10 5 19))

(defun example () '(0 3 6))
