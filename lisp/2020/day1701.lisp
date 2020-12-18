(defun day-17-2020-1 ()
    (count-it (input) 3))

(defun day-17-2020-2 ()
    (count-it (input) 4))

(defun count-it (input dimension)
  (let ((hash (parse-input input dimension)))
    (loop repeat 6
          for to-refer = hash then to-change
          for to-change = (copy-hash to-refer)
          do (loop for coords in (all-coords to-refer dimension)
                   do (new-state coords to-change to-refer))
          finally (return (hash-table-count to-change)))))

(defun all-coords (hash dimension)
  (make-combination
   (loop for (mi ma) on (get-boundaries hash dimension) by #'cddr collect (range mi ma)) 0 () ()))

(defun range (start end) (loop for i from start to end collecting i))

(defun get-boundaries (hash dimensions)
  (loop for i from 1 to dimensions
        for min-max = (loop for key being the hash-key in hash
                            minimize (nth (1- i) key) into min
                            maximize (nth (1- i) key ) into max
                            finally (return (list (1- min) (1+ max))))
        collect min-max into lists
        finally (return (apply #'concatenate 'list lists))))

(defun get-value (coords hash) (or (gethash coords hash) 0))

(defun new-state (coords change hash)
  (let ((neighbours (count-neighbours coords hash)))
    (cond ((and (equal 1 (get-value coords hash)) (not (member neighbours '(2 3))))
           (remhash coords change))
          ((and (equal 0 (get-value coords hash)) (equal 3 neighbours))
           (setf (gethash coords change) 1)))))

(defun add-or-not (coords given-coords hash)
  (if (equal coords given-coords) 0 (get-value given-coords hash)))

(defun count-neighbours (coords hash)
  (let* ((sets (loop for i in coords collect (list (1- i) i (1+ i))))
         (combinations (make-combination sets 0 () ())))
    (reduce #'+ (mapcar (lambda (x) (add-or-not coords x hash)) combinations))))

(defun make-combination (list index partial output)
  (if (= index (length list)) (setf output (push partial output)))
  (loop for i in (nth index list)
        do (setf output (append (make-combination list (1+ index) (append partial (list i)) output))))
  output)

(defun copy-hash (hash)
  (let ((new (make-hash-table :test #'equal)))
    (loop for k being the hash-key in hash using (hash-value v)
          do (setf (gethash k new) v)
          finally (return new))))

(defun parse-input (input dimension)
  (let ((hash (make-hash-table :test #'equal)))
    (loop for ch across input
          for x = 0 then (if (equal #\newline ch) -1 (1+ x))
          for y = 0 then (if (equal #\newline ch) (1+ y) y)
          when (equal #\# ch)
            do (setf (gethash (extend-dimension dimension x y) hash) 1)
          finally (return hash))))

(defun extend-dimension (dimension x y)
  (concatenate 'list (list x y) (make-list (- dimension 2) :initial-element 0)))

(defun input ()
  "####...#
......##
####..##
##......
..##.##.
#.##...#
....##.#
.##.#.#.")

(defun example ()
  ".#.
..#
###")
