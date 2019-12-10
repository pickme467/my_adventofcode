(defun input ()
".###.###.###.#####.#
#####.##.###..###..#
.#...####.###.######
######.###.####.####
#####..###..########
#.##.###########.#.#
##.###.######..#.#.#
.#.##.###.#.####.###
##..#.#.##.#########
###.#######.###..##.
###.###.##.##..####.
.##.####.##########.
#######.##.###.#####
#####.##..####.#####
##.#.#####.##.#.#..#
###########.#######.
#.##..#####.#####..#
#####..#####.###.###
####.#.############.
####.#.#.##########.")

(defun sample1 ()
  "......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####")

(defun sample2 ()
  "#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.")

(defun sample3 ()
  ".#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..")

(defun sample4 ()
  ".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##")

(defun sethash (key value hash)
  (setf (gethash key hash) value))

(defun copy-hash (source)
  (let ((new-hash (make-hash-table :test #'equal)))
    (maphash #'(lambda (key value) (sethash key value new-hash)) source)
    new-hash))

(defun make-coordinates (data)
  (let ((table (make-hash-table :test #'equal))
        (x 0) (y 0))
    (map 'list #'(lambda (c)
                   (if (equal #\Newline c)
                       (progn
                         (setf x 0)
                         (incf y))
                       (progn
                         (if (equal #\# c) (sethash (list x y) c table))
                         (incf x)))) data)
    table))

(defun show-hash (hash)
  (maphash #'(lambda (k v) (format t "~a ~a~%" k v)) hash))

(defun same-line-p (a b c)
  (let ((ax (nth 0 a))
        (ay (nth 1 a))
        (bx (nth 0 b))
        (by (nth 1 b))
        (cx (nth 0 c))
        (cy (nth 1 c)))
    (cond
      ((or (equal a b) (equal a c) (equal b c)) t)
      ((and (equal ax bx) (equal ax cx)) t)
      ((and (not (equal bx ax)) (not (equal cx ax)) (equal (/ (- by ay) (- bx ax)) (/ (- cy ay) (- cx ax)))) t)
      (t nil))))

(defun distance (a b)
  (let ((ax (nth 0 a))
        (ay (nth 1 a))
        (bx (nth 0 b))
        (by (nth 1 b)))
    (+ (expt (- bx ax) 2) (expt (- by ay) 2))))

(defun count-visible (hash)
  (let ((result ()))
    (maphash #'(lambda (point value)
                 (declare (ignore value))
                 (let ((found (find-visible-from-point point hash)))
                   (push (list (hash-table-count found) point) result))) hash)
    (setf result (sort result #'> :key #'car))
    (car result)))

(defun find-visible-from-point (point hash)
  (let ((result (make-hash-table :test #'equal)))
    (maphash #'(lambda (new-point value)
                 (declare (ignore value))
                 (put-if-new-or-closer point new-point result)) hash)
    result))

(defun put-if-new-or-closer (from-point to-point hash)
  (cond
    ((equal from-point to-point) 'skip)
    (t (let ((to-insert to-point)
             (to-remove ())
             (done-p nil))
         (maphash #'(lambda (other-point value)
                      (declare (ignore value))
                      (cond ((equal t done-p) 'skip)
                            (t
                             (if (same-line-p from-point to-point other-point)
                                 (progn
                                   (cond ((> (max (distance from-point to-point)
                                                  (distance from-point other-point))
                                             (distance to-point other-point))
                                          (setf to-insert to-point)
                                          (setf done-p t)
                                          (if
                                           (> (distance from-point to-point)
                                              (distance from-point other-point))
                                           (progn
                                             (setf to-insert to-point)
                                             (setf to-remove other-point))
                                           (progn
                                             (setf to-insert other-point)
                                             (setf to-remove to-point)))))))))) hash)
         (sethash to-insert 'point hash)
         (if (not (null to-remove))
             (remhash to-remove hash))))))

(defun vaporize (point hash)
  (let ((list (convert-to-list hash)))
    (cartesian-to-polar point list)))

(defun convert-to-list (hash)
  (let ((list ()))
    (maphash #'(lambda (point value)
                 (declare (ignore value))
                 (setf list (push point list))) hash)
    list))

(defun cartesian-to-polar (a list)
  (sort
   (map 'list #'(lambda (b)
                  (let ((quarter (find-quarter a b))
                        (alpha (find-alpha a b))
                        (distance (distance a b)))
                    (list quarter alpha distance b))) list)
   #'polar-less-p))

(defun polar-less-p (a b)
  (let ((qa (nth 0 a))
        (aa (nth 1 a))
        (da (nth 2 a))
        (qb (nth 0 b))
        (ab (nth 1 b))
        (db (nth 2 b)))
    (or (< qa qb)
        (and (= qa qb) (< aa ab))
        (and (= qa qb) (= aa ab) (< da db)))))

(defun find-quarter (a b)
  (let*  ((dx (- (nth 0 b) (nth 0 a)))
          (dy (- (nth 1 b) (nth 1 a))))
    (cond
      ((and (>= dx 0) (< dy 0)) 1)
      ((and (>= dx 0) (>= dy 0)) 2)
      ((and (< dx 0) (>= dy 0)) 3)
      (t 4))))

(defun find-alpha (a b)
  (let ((dx (abs (- (nth 0 b) (nth 0 a))))
        (dy (abs (- (nth 1 b) (nth 1 a)))))
    (if (not (= 0 dx))
        (atan (/ dy dx))
        (/ pi 2))))

(defun find-200th (point input)
  (let* ((list (cartesian-to-polar point (convert-to-list (find-visible-from-point point (make-coordinates input)))))
         (element-200 (nth 199 list))
         (coordinates (nth 3 element-200))
         (x (nth 0 coordinates))
         (y (nth 1 coordinates)))
    (+ (* x 100) y)))

;; validations
(assert (equal '(33 (5 8)) (count-visible (make-coordinates (sample1)))))
(assert (equal '(35 (1 2)) (count-visible (make-coordinates (sample2)))))
(assert (equal '(41 (6 3)) (count-visible (make-coordinates (sample3)))))
(assert (equal '(210 (11 13)) (count-visible (make-coordinates (sample4)))))

;; day 10 part 1
(time (assert (equal '(214 (8 16)) (count-visible (make-coordinates (input))))))

;; day 10 part 2
(time (assert (= 502 (find-200th '(8 16) (input)))))
