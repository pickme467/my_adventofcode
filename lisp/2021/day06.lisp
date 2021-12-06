(defun simulate-day (input)
  (loop for n in input
        when (> n 0) collect (1- n)
          else when (equal 0 n) append '(6 8)))

(defun simulate-80-days (input)
  (loop for i from 1 to 80
        for j = (simulate-day input) then (simulate-day j)
        finally (return (length j))))

(defun list-to-array (input)
  (let ((eldery-array (make-array 9 :initial-element 0)))
    (loop for i in input
          do (setf (aref eldery-array i) (1+ (aref eldery-array i)))
          finally (return eldery-array))))

(defun simulate-array-day (array)
  (let ((eldery-array (make-array 9 :initial-element 0)))
    (loop for v across array
          for i = 0 then (1+ i)
          when (equal 0 i) do
            (setf (aref eldery-array 6) v)
            (setf (aref eldery-array 8) v)
          else do
            (setf (aref eldery-array (1- i)) (+ v (aref eldery-array (1- i))))
          finally (return eldery-array))))

(defun simulate-256-array-days (input)
  (loop for i from 1 to 256
        for j = (simulate-array-day input) then (simulate-array-day j)
        finally (return (loop for n across j sum n))))

(defun day06-1 () (assert (= 389726 (simulate-80-days (input)))))
(defun day06-2 () (assert (= 1743335992042
                             (simulate-256-array-days (list-to-array (input))))))

(defun input ()
  '(
    1 1 1 1 1 1 1 4 1 2 1 1 4 1 1 1 5 1 1 1 1 1 1 1 1 1 1 1 1 5
    1 1 1 1 3 1 1 2 1 2 1 3 3 4 1 4 1 1 3 1 1 5 1 1 1 1 4 1 1 5
    1 1 1 4 1 5 1 1 1 3 1 1 5 3 1 1 1 1 1 4 1 1 1 1 1 2 4 1 1 1
    1 4 1 2 2 1 1 1 3 1 2 5 1 4 1 1 1 3 1 1 4 1 1 1 1 1 1 1 4 1
    1 4 1 1 1 1 1 1 1 2 1 1 5 1 1 1 4 1 1 5 1 1 5 3 3 5 3 1 1 1
    4 1 1 1 1 1 1 5 3 1 2 1 1 1 4 1 3 1 5 1 1 2 1 1 1 1 1 5 1 1
    1 1 1 2 1 1 1 1 4 3 2 1 2 4 1 3 1 5 1 2 1 4 1 1 1 1 1 3 1 4
    1 1 1 1 3 1 3 3 1 4 3 4 1 1 1 1 5 1 3 3 2 5 3 1 1 3 1 3 1 1
    1 1 4 1 1 1 1 3 1 5 1 1 1 4 4 1 1 5 5 2 4 5 1 1 1 1 5 1 1 2
    1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 5 1 1 1 1 1 1 3 1 1 2 1 1
    ))
