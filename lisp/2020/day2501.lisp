(defun day-25-2020-1 ()
  (transformation (first (input)) (loop-size (second (input)))))

(defun loop-size (key)
  (loop for value = 1 then (mod (* value 7) 20201227)
        for loop = 0 then (1+ loop)
        when (equal key value)
          return loop))

(defun transformation (key times)
  (loop for value = 1 then (mod (* value key) 20201227)
        for loop from 1 to times
        finally (return value)))

(defun input ()
  '(6929599
    2448427))

(defun example ()
  '(5764801
    17807724))
