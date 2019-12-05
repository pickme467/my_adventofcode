(defun input () (list 1 0 0 3 1 1 2 3 1 3 4 3 1 5 0 3 2 13 1 19 1 6 19 23 2 23 6 27 1 5 27 31 1 10 31 35 2 6 35 39 1 39 13 43 1 43 9 47 2 47 10 51 1 5 51 55 1 55 10 59 2 59 6 63 2 6 63 67 1 5 67 71 2 9 71 75 1 75 6 79 1 6 79 83 2 83 9 87 2 87 13 91 1 10 91 95 1 95 13 99 2 13 99 103 1 103 10 107 2 107 10 111 1 111 9 115 1 115 2 119 1 9 119 0 99 2 0 14 0))

(defun do-add (start list)
  (let ((x (nth (+ start 1) list)) (y (nth (+ start 2) list)) (pos (nth (+ start 3) list)))
    (setf (nth pos list) (+ (nth x list) (nth y list)))))

(defun do-multiply (start list)
  (let ((x (nth (+ start 1) list)) (y (nth (+ start 2) list)) (pos (nth (+ start 3) list)))
    (setf (nth pos list) (* (nth x list) (nth y list)))))

(defun input-mod (one two)
  (let ((i (input)))
    (setf (nth 1 i) one)
    (setf (nth 2 i) two)
    i))

(defun calculate (start input)
  (let ((h (nth start input)))
    (if (= h 99)
        (car input)
        (progn
          (if (= h 1)
              (do-add start input)
              (if (= h 2)
                  (do-multiply start input)
                  input))
          (calculate (+ 4 start) input)))))

(defun next-x (x y)
  (if (= 99 y)
      (if (> 99 x)
          (1+ x)
          0)
      x))

(defun next-y (y)
  (if (> 99 y)
      (1+ y)
      0))

(defun calculate-with-reset ()
  (do* ((x 12 (next-x x y))
        (y 2 (next-y y))
        (data (input-mod x y)
              (input-mod x y))
        (result (calculate 0 data)
                (calculate 0 data)))
       ((= 19690720 result) (+ (* 100 x ) y))))

;; day 2 part 1
(time (assert (= 3850704 (calculate 0 (input-mod 12 2)))))

;; day 2 part 2
(time (assert (= 6718 (calculate-with-reset))))
