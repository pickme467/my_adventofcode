(defun new-hash ()
  (make-hash-table :test #'equal))

(defun set-hash (key value hash)
  (setf (gethash key hash) value))

(defun to-hash (input)
  (let ((hash (new-hash)))
    (loop for line in input
          for y = 0 then (1+ y)
          do (loop for n across line
                   for x = 0 then (1+ x)
                   do (set-hash (list x y) (parse-integer (string n)) hash))
          finally (return hash))))

(defun step-a-step (hash)
  (loop for k being the hash-keys in hash using (hash-value v)
        do (set-hash k (1+ v) hash)
        finally (return hash)))

(defun flash-octopus (hash)
  (loop for k being the hash-keys in hash using (hash-value v)
        with flashed = 0
        when (> v 9) do
          (increase-neighbours k hash)
          (set-hash k 0 hash)
          (setf flashed (1+ flashed))
        finally (return flashed)))

(defun increase-neighbours (k hash)
  (destructuring-bind (x y) k
    (let ((neighbours
            (list (list (1- x) (1- y))
                  (list x (1- y))
                  (list (1+ x) (1- y))
                  (list (1- x) y)
                  (list x y)
                  (list (1+ x) y)
                  (list (1- x) (1+ y))
                  (list  x (1+ y))
                  (list (1+ x) (1+ y)))))
      (loop for n in neighbours
            for v = (gethash n hash)
            when (and (not (null v)) (> v 0))
              do (set-hash n (1+ v) hash)))))

(defun flash-all-octopuses (hash)
  (loop for prev-flashed = 0 then flashed
        for flashed = (flash-octopus hash) then (+ flashed (flash-octopus hash))
        when (equal flashed prev-flashed) return flashed))

(defun flash-times (times hash)
  (loop for x from 1 to times
        do (step-a-step hash)
        sum (flash-all-octopuses hash)))

(defun step-until-all-flash (hash)
  (loop for n = 1 then (1+ n)
        do (step-a-step hash)
        when (= 100 (flash-all-octopuses hash))
          return n))

(defun day11-1 () (assert (= 1679 (flash-times 100 (to-hash (input))))))

(defun day11-2 () (assert (= 519 (step-until-all-flash (to-hash (input))))))

(defun input ()
  '("1553421288"
    "5255384882"
    "1224315732"
    "4258242274"
    "1658564216"
    "6872651182"
    "5775552238"
    "5622545172"
    "8766672318"
    "2178374835"))
