(defun day-11-2020-1 ()
  (stabilize (input) #'nearest-neighbours))

(defun day-11-2020-2 ()
  (stabilize (input) #'furtherst-neighbours))

(defun stabilize (input update-function)
  (loop for i = input then k for k = (new-state i update-function)
        when (equal-inputs i k) return (count-occupied k)))

(defun count-occupied (input)
  (loop for v being the hash-value in input count (equal v #\#)))

(defun equal-inputs (a b)
  (loop for ak being the hash-keys in a
        when (not (equal (gethash ak a) (gethash ak b)))
          return nil
        finally (return t)))

(defun new-state (input update-function)
  (loop for ak being the hash-keys in input
          using (hash-value v) with output = (make-hash-table :test #'equal)
        do (setf (gethash ak output) (funcall update-function ak v input))
        finally (return output)))

(defun nearest-neighbours (ak v input)
  (cond ((and (equal v #\L)
              (equal 0 (neighbour-count ak input #'nearest))) #\#)
        ((and (equal v #\#)
              (>= (neighbour-count ak input #'nearest) 4)) #\L)
        (t v)))

(defun furtherst-neighbours (ak v input)
  (cond ((and (equal v #\L)
              (equal 0 (neighbour-count ak input #'further))) #\#)
        ((and (equal v #\#)
              (>= (neighbour-count ak input #'further) 5)) #\L)
        (t v)))

(defun neighbour-count (coords input function)
  (loop for i in (list
                  (lambda (x y) (list (1- x) y))
                  (lambda (x y) (list (1+ x) y))
                  (lambda (x y) (list x (1- y)))
                  (lambda (x y) (list x (1+ y)))
                  (lambda (x y) (list (1- x) (1- y)))
                  (lambda (x y) (list (1- x) (1+ y)))
                  (lambda (x y) (list (1+ x) (1- y)))
                  (lambda (x y) (list (1+ x) (1+ y))))
        collecting (funcall function coords i input) into count-list
        finally (return (reduce #'+ count-list))))

(defun further (pos fun input)
  (loop for i = (apply fun pos) then (apply fun i)
        when (equal #\# (gethash i input))
          return 1
        when (member (gethash i input) '(#\L nil))
          return 0))

(defun nearest (pos fun input)
  (let ((value (gethash (apply fun pos) input)))
    (cond ((equal value #\#) 1)
          (t 0))))

(defun make-hash (input)
  (let ((output (make-hash-table :test #'equal)))
    (loop for i in input for x = 0 then (1+ x) with y = 0
          when (not (equal i #\linefeed))
            do (setf (gethash (list x y) output) i)
          else do (incf y) (setf x -1)
          finally (return output))))

(defun input ()
  (make-hash (concatenate 'list "LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLL.LLLLL.LLLLL.LLLL.LLLL.LLLLLLLLLL.LLLLLL.LLLLL
LLLLL.LLLL.LLLLL.LLLLL.L.LLLLLL.LLLL.LLLLLL.LL....LLLLLLLL.LLLLLLLLL.LLLL.LLLLLLLLLL.LLLLLLL
LLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLL.LLLLLLLL.LLLLL.LLLLL.LL.LLLLLL.L.
LLLLLLLLLL.LLL...LLLLL.LLLLLLLL.L.LLLLLLLLLLLLL.LLLL.LLLLL.LLLLLLLLL.LLLL.LLLLL.LLLLLL.LLLLL
LLLLLLLLLL.LLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLL.LLLLL..LLLL.LLLL.LL...LLLLLLLLLL.LLLLLL.LLLLL
LL.LLLLLLLLLLLLL.LLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLL.L.LLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLL
LLLLLLLLLL.L.LLLL.LLLLLLLLLLLLL.LLLLLL.LLLLLLL.LLLLLLLLLLL.LLLLLLLLL.LLLL.LLLLL.LLLLLL.LLLLL
.L...L...LLLL..LLL......L.LLL.L.L.....L...L..L.L................L...L...L.LLLL.......LLL....
L..LLLLLLL.LLLLL.LLLLL.LLLLLLLLLLL.LLLLLLLLLLL.LLLLL.LLLLL.LLLLLLLL..LLLLL.LLLL.LLLLLL.LLL.L
LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLL.LLLLLL.LLL.L
LLL.LLLLLL.LLLLL.LLLLL.LLLLLLLL.LLLLLLLLLLLLLL.LLL.LLLLLLL.LLLLLL.LLLLLLLLLLLLL.LLLLLL....LL
LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLLLL.LLLLLLL.LL.LLLL.L.L.LLL
LLLLLL.LLL.L.LL.LL.LLL.LLLLLLLL.LLLL.LLLLL.LLL.LLLLLLLLLLL.LLLLLLLLL.LLLL.LLLLL.LLLLLL.LLLLL
LLLLLLLLLL.LLLLLLLL.LL.LL.LLLLL.LLLL.LLLLLLLLLLLLLLL..LLLL.LLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLL
.LL.....LLLL.L..L..L..L...L.L..L..L.L....LL..L...LLL...LL.....L..LL....L...LLL..L...LLL..LLL
LLLLLLLLLL.LLLLL.LLLLL.LLLLLL.L.LLLL.LLLLLLLL..LLLLL.LLLLL.L.LLLL.LLLLLLL.LLLLLLLLLLLL.LLLLL
LLLLLLLLLLLL.LLL.LLL.LL.LLLLLLL.LLLL.LLLLLLLLL..LLLL.LLLLLLLLLLLL.LL.LLLLLLLL.LLLLLLLLLLLLLL
LLLLLLLLLLLLLLLL.LLLLL.LLLLLLLL.LLLL.LLLLLLLL..LLLLLLLLLLLLLLL.LLLLL.LLLL.LLLLL.LLL.LL.LLLLL
LLLLLL.L..LLLLLL.LLLLLLLL.LLLLLLLLLLLLL.LLLLLL.LLLLL.LLLLLLLLL.LLLLL.LLLLLLLLLL.LLLL.LLLLLLL
LLLLLLLLLL.LLLLLLLLLLL..LLLLLLL.LLLL.LLLLLLLLL.LLLLL.LLLL..LLLLLLLL..LLLL.LLLLL.LLLLLLLLLLLL
LLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLLLLLLLL.L..LL.LL.LLLLLL.LLLL.LLLLLLLLLLLLLLLLLL
LLLLLLLLLL.LLLLLLLLLLLL.LLL.LLL.LLLLLLLLLLLLLL.LLLLL.LLLLL.LLLLLLLLL.L.LL.LLLLLLLLLLLLLLLLLL
L.LLLLLLLL.LLL.LL.LL.LLLLLLLLLL.LLLLL.LLLLLLLL.LLLLLLLLLLL.LLLLLLLLL.LLLL.LLLLLL.LLLLL.LLLLL
L..LL...L.L..L..L.L..L.L.L..L.LL.L....LL....LL.L...L..LL..LLL....L...LL...L..L.L....L....LLL
LLLLLLLLLL.LLLLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLL
LLLLLLLLLL.LLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLL.LLLLLLLLL..LLL.L.LLLLLLLLLL.LLLLL
LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLL.LLLL.LLLLLLLLL.LLLLL.LLLLLLLLLLL.LLLLLLLL.LLLLL.LLLL.LLLLL.L
LLLLLLLLLL.LLLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLL.L.LLLLL.LLLLL.LLLLLLLLLLLLLL.LLLLLLLLLLLL.LLLLL
LLLLLLLLLLLLLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLLL.L.LLLLL.LLLLL.L.LLLL.LLLLLL.LLLLLLLLLLLLLLLLLLL
.LL......LLL....LL.L....LLL.L.L....L.LLL....L....L..L...L.......LL.....L...L.L..L.LLLL......
LLLLLLLLLL.LLLLL.LLLLL.LLLLLLLL.L.LL.LLLLLLLLLLLLLLL.LLLLLLL.LL.LLLLLLL.L.LL.LL.LLL.LLLLLLLL
LLLLLLLLLL.LLLLL.L.LLL.LLLLLL.L.LLLL.LLLLLLLLLLLLLLL.LLLLL.LLLLLLLLL.LLLL.LLLLL.LLLLLL.LLLLL
LLLLLLLLLL.LLLLL.LLLLL.LLLLLLLL.LLLL.LLLLLLLLLLLL.L..LLLLLLLLLLLLLLL..LLL.LL.LLLLLLLLL.LLLLL
LLLLLLLLLL.LLLLLLLLLLL.LLLLLLLL..LLL.LLLLLLLLL.LLLLL.LLLLLLLLLL.LLLL.LLLL.LLLLL.LLL.LL.L.LLL
LLLLLLLLLL.LLLLLLLLLL.LLLLLL.LL.LLLL.LLLLLL.LL.LLLLLLL.LLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLL
LLLLLLL.LL.LLLLL.LLLLL.L.LLLL.L.LLLL.LLLLLLLLL.LLLLL.LLLLL.LLLLLLLLL.LLLL.LLLLL.LLLLLL.LLLLL
LLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLL.LLLL.LLLL.LLLLL.LLLLL.LLLLLLLLL.LLL..LLLLL.LLLLLLLLLLLL
LLLLLLLL.LLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLL.L.LLLLL.L.LLLL.LLLLL
L...L.L...L...L.LL.........LL..........LLL.....LLL.....L...L.L..LL......L.L..LL.LL...L.L....
.LLLLLLLLL.LLL.L.LLLL..L.LLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLL.LLLLLLLLL.LLLL.LLLLL.LLLLLL.LLLLL
LLLLLLLLLL.LL..LLLL.LL.L.LLLLLL.LLLL.LLLLLLLLLLLLLLL.LLLLL.LLLLLLLLL.LLL..LLLLLLLLLLLL.LLLLL
LLLLLLL.LLLLLLLL.LLLLL.LLL.LLLL.LLLL.L.LLLLLLL.LLLLLLLLLLL.LLLLLLLLL.LLLL.LLLLL.LLL.LL..LLLL
LLLLLLLLLLLLLLLL.LL.LLLLLLLLLLL.LLLL.LLLLLLLLL.LLLLL.LLLLLLLLLLLLLLL.LLLL.LLLLL.LLLLLL.LLLLL
LLL.LLL.LL.LLLLLL.LLLLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLL..LLLLLLLLLLLLLL.LLLLL.LLLLLL.LLLLL
.L.LLLLLLL.LL.LL.LLL.L.LLLLLLLL..LLL..LL.LLLLLLLLLLL.LLLLL.LLLLLLLLL.LLLL.LLLLL.LLLLLL.LLLLL
.LLLLLLLLL..LLLLL.LLL..LLLLLLLL.LLL..LLLLLLLLLLLLLLLLLLLLL.LLLLLLLL..LLLL.LLLLLLLLLLLL..LLLL
LLLLLLLLLL.LLLLL.LLLLL.LLLLLLLL.LLLL.LLLLLLLLL.LLLLLLLLLL..LLLLLLLLL.LLLL.LLLLL.LLLLLL.LLLLL
L...L..L.....LL.L...........L...........LL.....L.L.L..L.........LLLLL...L.L..LL..L..L......L
LLLLLLLLLLLLLLLL.LL.LL.LLLLLLLL.LLLL.LLLLL..LLLLLLLLLLLLLL.LLLLLLLLL.LLLL.LLLLL.LLLLLLLLLLLL
LLLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLL..LLLL.LL.LL.L.LLLLLLLLLL
LLLLLLLLLL.LLLLL.LLLLL.LLLLLLLL.LLLLLLLLLLL.LL.LLLLL.LLLLL.LLLLLLLLL.LLLL.LLLLL.LLLLLL.LLL.L
LLLLLLLLLL.L.LL.LLLLLLLLLLLLLLL.LLLL..LLLLLLLL.LLLLL.LLLLL.LLLLLLLLL..LLL.LLLLLLLLLLLL.LLL.L
L.LLLLLLLL.LLL.L.LLLLLLLLLLL.LL.LLLLLLLLLLLLLL.LLL.L.LLLLL.L.LLLLLLL.LLLLLLLLLL.LLLLLL.LLLLL
LLLLLLLL.L.LLLLL.LLLLL...LL.LLL.LLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLLLLL.LLLLLLL.LLLL
LLLLL.LL..LLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLL.LLLLL.L.LLLLLLLLLLLLLLLLLLLLLLLL.LLLLLL.LLLLL
LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLL.LLLLLLLLLL.LLL.LLLLL.L.LLL.LLLLLLLLL.LLLL.LLLLL..L.LLL.LLLLL
LL...L.L..L.L....LL...LLLL..L...............L...........L.L.LL..L..L.LLL...LL......LLL..L..L
LLLLLLLLLL.LLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLL..LLLLL.LLLLL
.LL.LLLLLLLLLLLLLLLLL.L.LLLLLLL.LLL.LLLLLLLLLL.LLLLL.LLLLLLLLLLLLLLL.LLL...LLLL.LLLLLLLLLLLL
.LLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLLL.LLLLLL.LLLL.LLL.LLLLLLLLLLLLL.LLLLLLLLLL.LLLLLLLL.LLLLL
LLLLLLLLLL.LLLLL.LLLLL.LLLLLLLL.LLLL.LLLLLLLLLLL.LLL..LLLL.LL.L.LLLLLLLLLLLLLLL.LLLLLLLLL.LL
LLLLLLLLLL.LLLLLL.LLLLLLLLL.LLLLLL.L.LL.LL.LLLLLLLLL.LLLLLLLLLLLLLLL.LLLL.LLLLLLLLLL.LLLLLLL
LLLLLLLLLL.LLLLL.LLLLLLLLLLL.LL.LLLLLLLLLLL.LL.LL.LLLLLLLL..LLLLLLLL.LLLL.LLLLL.LLLLLLLL.LLL
LL.LLLLLLL.LLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLL.LLL.LLLLLLLLLL.L.LLLLL
...L.L.....L.L..LL.LL..L.L...LLLL.......L.LLL...LL..L.L.L..L...L..L......LLL...L.L...L...L..
LL.LLLLLL..LLLLL.LLL.LLLLLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLL.LLLLLLLLLLLLLL..LLLLLLLLLLL.LLLLL
LLLLLLLLL.LLLLLL.LLLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLLLL.LLLLL..LLLLLLLL.LL.L.LLLLLLLLLLLLLLLLLL
LLLLLLLLLLLL.LLLLLLLLL.LLLLLLLL.LL..LLLLLLLLLLLLLLLL.LLLLL.LL.LLLLLL.LLLL.LLL.L.LLLLLL.LLLLL
LLLLLLLLLL.LLLLLLLLLLL.LLLLLLLLLLLL..LLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLLLL.LLLLL.L.LLLLLLLLLL
L.LLLLLLLLLLLLLL.LLLLLLLLLLLLLL.L..L.LLLLLLLLL.LL.LL.LLLLLLLLL.LLLLLLLLLL.LLLLL.LLLLLL.LLLLL
LLLLLLLLLL.LLLLL.LL.LL.LLLLLLL..LL.L.LLLLLL.LL.LLLLLLLLLLL..LLLLLLLL.LLLLLLLLLL.LLLLL..LLLLL
LLLLLLLLLLLLLLLL.LLLLL.LLLLLLL..LLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLLLL.LLLLL.LLLLLLLLLL.LLLLLLL
LLLL.LLLLL.LLLLL.LLLLL.LLLLLLLL.LLLL.LL.LLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLL.L..LLL.LLLLL
L.LL.....LLLL.LL.LLL..L....LLL.L..LLL.....L.L.L...L.L.L..L.L......L.....LL..LL..L..LL.....L.
LLLLLLLLLL.LLLL.LLLLLL.LLLLLLLL.LLLL.LLLL.LLLLLLLLLL.LLLLL.LLLLL.L.L.LLLLLLLLLLLLLLLLL.LLLLL
LLLLL.LL...LLLLLLLLLLL.LL.LLLLL.L.LLLLLLLLLLLL.LLLLLLLLLL..LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
LLLLLLLLLL.LLLLLL.LLLL.LLLLLLLL.LLLL.LLLLLLLLLLLLLL..LLLLLLLLLL.LLLL.LLL.L.LLLL.LLLLLLLLLLLL
LLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLL.LLLLL.LLLLLLLLLLLLLL.LLLLL.LLLLLL.LLLLL
LLL.LLLLLL.LLLLLLLLLLLLL.LLLLLL.LLL..LL.LLLLLLLLLL.L.LLLLL.LLLLLL.L...LLL.LLLLLLLLLLLLLLLLLL
LLLLLLLLLLL.LLLLLLLLLL.LLLLLLL.L.LLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLL.LLLLLL
LLLLLLLLLLLLLL...LLLLLL.LLLLLLLLLLLL.LLLLLLLLLL.LLLL.LLLLL.LLLLLLLLLLLLLL.LLLLL.LLLLLL.LLLLL
L.......LL.L...LL...L......LL.L.....LL..L...L..L.LL........L.L..L.L.LL...L..L...LL..L.LLLL..
LLLLLLLLLL.LLLLL.L.LLLLLLLLLLLL..LLL.LLLLLLLLL.LLLLL.LLLLL.LLLLL.LLL.LLLL.LLLLL.LLLLLLLL.LLL
LLLLLLLLLL.LLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLL.LLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLL.LLLLLL.LLLLL
LLLLLLLLLL.LLL.LLLLLLL.LLLLLLLL.LL.L.LLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLL.LLLLLL.LLLLL
LLLLLLLLLL.LLLLL.LLLLL.LLLLLLLL.LLLL.LLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLL.LLLLL
LLLL.LLLL.LLLLLL.LLL.L.LLLLLLL..LLLL.LLLLLLLLLLLLLLL.LLLLL.L.LLLLLLL.LLLLLLLLLL.LLLLLLLLLLLL
LLLLLLLLLL.LLLLL.LLLLL.LLLLLLLL.LLLL.LLLLLL.LL.LLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.L
LLLLLL.LLL.LLLLL.L.LLL.LLLLLLLL.LLLL.LLLLLLLLL.LLLLL.LLLLL.LLLLLLLLLLLLLL.LLLLL.LLLLLL.L.LLL
LLLLLLLLLL.LLLL.LLLLLLLLLLL.LLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLL.LLLLLL.LLLLL
LL.LLLLLLL.LLLLL..LLLL..LLLLLLL.LLLL.LLLLLLLLLLLLL.L.LLLLL.LLLLLLLLL.LLLL.LLLLL.LLLLLLLLLLLL
.L.L.L.LL..L.L.......L.....LLLL.L.....L.L.L...L.L..L.L..L..L.....LL.LL.L.......LL.L.L.....LL
LLLLLLLLLL.LLLLL.LLLLL.LLLLLLLL.LLLL.LLLLLLLLLLLLLLL.LLLLL.LLLLLLLLL.LLLL.LLLLLL.LLLLLLLLLLL
LLLLLLLLLL.LL.LL.LLLLL.LLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLL..LLLLLLLLLLLLLLL.LLLLL
LLLLLLLLLLLL.LLLLLLLLLLLLLLLL.L.LLLL.LL.LLLLLLLLLLLL.LLLLL.LLLLLLLLL.LLLL.LLLLLLLL.LLLLLLLLL
LLLLLLLLLLLL.LLL.LLLLL.LLLL.LLL.LLLL.LLLL.LLLL.LLLLL..L.LLLLL.LLLLLL.LLLL.LLLLL.LLLLLL.LLLLL
L.LLLLLLLL.LLLLL.LLLLL.LLLLLLLL.LLLL..LLLLLLLL.LLLLL.LLLLL.LLLLLLLLL.LLLL.LLLLL..LLLLL.LLLLL
LLLLLLLLLLLLLLLL.LLLLLLLLL.LLLL.LLLLL.LLLLLLLL.LLLLLLLLLLL.LLLLLLLLLLLLLL.LLLLL..LLL.L.LLLLL
LLLLLLLLLL.LLLLL.LLLLLLL.LLLLLL.LL.LLLLLLLLLLL.LLLLL.LLLLL.LLLLLLLLL.LLLL.LLLLL.LLL.LLLLLLLL")))

(defun example ()
  (make-hash (concatenate 'list "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")))
