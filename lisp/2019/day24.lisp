(defun input ()
  "..#.#
#####
.#...
...#.
##...")

(defun sample1 ()
  "....#
#..#.
#..##
..#..
#....")

(defun string-to-hash (input)
  (let ((hash (newhash))
        (x 0)
        (y 0))
    (loop for c across input
          for cc = (char-code c) do
            (cond
              ((= 10 cc)
               (setf x 0)
               (incf y))
              (t
               (setstate (list x y) (if (char= c #\#) 'bug 'empty) hash)
               (incf x))))
    hash))

(defun do-step (hash)
  (let ((next-step (newhash)))
    (loop for k being the hash-keys in hash do
      (sethash k (bug-or-empty k hash) next-step))
    next-step))

(defun bug-or-empty (key hash)
  (let* ((vectors '((-1 0) (1 0) (0 -1) (0 1)))
         (state (loop for v in vectors collecting (getstate (mapcar #'+ key v) hash)))
         (bug-or-empty (getstate key hash)))
    (cond ((equal 'bug bug-or-empty)
           (if (bug-dies-p state) 'empty 'bug))
          (t
           (if (bug-hatches-p state) 'bug 'empty)))))

(defun do-recursive-step (hash-list)
  (let* ((total-list (append (list (newhash) (newhash)) hash-list (list (newhash) (newhash))))
         (to-execute
           (loop for p in total-list
                 for c in (cdr total-list)
                 for n in (cddr total-list)
                 collecting (list p c n))))
    (loop for x in to-execute collecting (get-next-step x))))

(defun get-next-step (neighbours)
  (destructuring-bind (prev current next) neighbours
    (let ((next-hash (newhash)))
      (loop for k being the hash-keys in (total-hash) do
        (if (not (equal k '(2 2)))
            (sethash k (bug-or-empty-recursive k prev current next) next-hash)))
      next-hash)))

(defun do-n-recursive-steps (hash-list n)
  (let ((output ())
        (next-step hash-list))
    (loop for k from 1 to n do
      (setf next-step (do-recursive-step next-step))
      (setf output (push next-step output)))
    output))

(defun count-bugs (hash-list)
  (loop for h in hash-list sum
                           (loop for k being the hash-keys in h sum (if (equal 'bug (getstate k h)) 1 0))))

(defun total-hash ()
  (let ((h (newhash)))
    (loop for y from 0 to 4 do
      (loop for x from 0 to 4 do
        (sethash (list x y) 'total-hash h)))
    h))

(defun bug-or-empty-recursive (key down current up)
  (destructuring-bind (up-list current-vector down-list)
      (key-to-neighbours key)
    (let* ((upper-state
             (loop for v in up-list collecting (getstate v up)))
           (current-state
             (loop for v in current-vector collecting (getstate (mapcar #'+ key v) current)))
           (lower-state
             (loop for v in down-list collecting (getstate v down)))
           (total-state (append upper-state current-state lower-state))
           (bug-or-empty (getstate key current)))
      (cond
        ((equal 'bug bug-or-empty)
         (if (bug-dies-p total-state) 'empty 'bug))
        (t
         (if (bug-hatches-p total-state) 'bug 'empty))))))

(defun key-to-neighbours (key)
  (cond
    ((equal key '(0 0)) '(((2 1) (1 2)) ((1 0) (0 1)) ()))
    ((equal key '(0 1)) '(((1 2)) ((0 -1) (0 1) (1 0)) ()))
    ((equal key '(0 2)) '(((1 2)) ((0 -1) (0 1) (1 0)) ()))
    ((equal key '(0 3)) '(((1 2)) ((0 -1) (0 1) (1 0)) ()))
    ((equal key '(0 4)) '(((1 2) (2 3)) ((0 -1) (1 0)) ()))
    ((equal key '(1 0)) '(((2 1)) ((-1 0) (0 1) (1 0)) ()))
    ((equal key '(1 1)) '(() ((-1 0) (1 0) (0 -1) (0 1)) ()))
    ((equal key '(1 2)) '(() ((-1 0) (0 -1) (0 1)) ((0 0) (0 1) (0 2) (0 3) (0 4))))
    ((equal key '(1 3)) '(() ((-1 0) (1 0) (0 -1) (0 1)) ()))
    ((equal key '(1 4)) '(((2 3)) ((-1 0) (0 -1) (1 0)) ()))
    ((equal key '(2 0)) '(((2 1)) ((-1 0) (0 1) (1 0)) ()))
    ((equal key '(2 1)) '(() ((-1 0) (1 0) (0 -1)) ((0 0) (1 0) (2 0) (3 0) (4 0))))
    ((equal key '(2 2)) (assert nil))
    ((equal key '(2 3)) '(() ((-1 0) (1 0) (0 1)) ((0 4) (1 4) (2 4) (3 4) (4 4))))
    ((equal key '(2 4)) '(((2 3)) ((-1 0) (0 -1) (1 0)) ()))
    ((equal key '(3 0)) '(((2 1)) ((-1 0) (0 1) (1 0)) ()))
    ((equal key '(3 1)) '(() ((-1 0) (1 0) (0 -1) (0 1)) ()))
    ((equal key '(3 2)) '(() ((1 0) (0 -1) (0 1)) ((4 0) (4 1) (4 2) (4 3) (4 4))))
    ((equal key '(3 3)) '(() ((-1 0) (1 0) (0 -1) (0 1)) ()))
    ((equal key '(3 4)) '(((2 3)) ((-1 0) (0 -1) (1 0)) ()))
    ((equal key '(4 0)) '(((2 1) (3 2)) ((-1 0) (0 1)) ()))
    ((equal key '(4 1)) '(((3 2)) ((-1 0) (0 -1) (0 1)) ()))
    ((equal key '(4 2)) '(((3 2)) ((-1 0) (0 -1) (0 1)) ()))
    ((equal key '(4 3)) '(((3 2)) ((-1 0) (0 -1) (0 1)) ()))
    ((equal key '(4 4)) '(((2 3) (3 2)) ((-1 0) (0 -1)) ()))
    (t (assert nil))))

(defun newhash ()
  (make-hash-table :test #'equal))

(defun setstate (key value hash)
  (setf (gethash key hash) value))

(defun getstate (key hash)
  (let ((value (gethash key hash)))
    (if (null value) 'empty
        value)))

(defun bug-dies-p (list)
  (if (= 1 (loop for x in list counting (equal 'bug x))) nil t))

(defun bug-hatches-p (list)
  (let ((bugs (loop for x in list counting (equal 'bug x))))
    (cond ((= 1 bugs) t)
          ((= 2 bugs) t)
          (t nil))))

(defun print-hash (hash)
  (loop for y from 0 to 4 do
    (loop for x from 0 to 4 do
      (format t "~a" (bug-to-char (getstate (list x y) hash))))
    (format t "~%")))

(defun bug-to-char (bug-or-empty)
  (if (equal 'bug bug-or-empty) #\# #\.))

(defun same-states (one two)
  (= (hash-table-count one)
     (loop for k being the hash-key in one
           for i = 1 then (1+ i) until (not (equal (getstate k one) (getstate k two))) maximizing i)))

(defun find-same-pattern ()
  (let ((previous nil))
    (loop named outer for s = (string-to-hash (input)) then (do-step s) do
      (loop for p in previous do
        (if (same-states p s)
            (return-from outer s)))
      (setf previous (push s previous)))))

(defun calculate-biodiversity-range (hash)
  (let ((sum 0))
    (loop for k being the hash-keys in hash using (hash-value v) do
      (if (equal 'bug v) (setf sum (+ sum (power-of k)))))
    sum))

(defun power-of (key)
  (destructuring-bind (x y) key
    (expt 2 (+ x (* 5 y)))))

;; day 24, part 1
(time (assert (= 18407158 (calculate-biodiversity-range (find-same-pattern)))))

;; day 24, part 2
(time (assert (= 1998
                 (count-bugs
                  (car (do-n-recursive-steps (list (string-to-hash (input))) 200))))))
