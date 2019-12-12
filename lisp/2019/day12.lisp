(defun input () '(((-10 -13 7) (0 0 0))
                  ((1 2 1) (0 0 0))
                  ((-15 -3 13) (0 0 0))
                  ((3 7 -4) (0 0 0))))

(defun velocity (a b)
  (cond
    ((< a b) 1)
    ((> a b) -1)
    (t 0)))

(defun new-velocity (list-of-positions)
  (let ((calculating-for (car list-of-positions))
        (rest (cdr list-of-positions))
        (calculated ()))
    (dolist (second-position rest)
      (let ((velocity-change (velocity-change calculating-for second-position)))
        (setf calculated (push velocity-change calculated))))
    (apply #'mapcar #'+ (nth 1 calculating-for) calculated)))


(defun velocity-change (position-velocity1 position-velocity2)
  (mapcar #'velocity (car position-velocity1) (car position-velocity2)))

(defun new-position (position velocity)
  (mapcar #'+ position velocity))

(defun one-step (input)
  (let ((result ()))
    (dolist (element input)
      (let* ((other-elements (remove element input))
             (position (nth 0 element))
             (all-elements (concatenate 'list (list element) other-elements))
             (next-velocity
              (new-velocity all-elements))
             (next-position
              (new-position position next-velocity)))
        (setf result (push (list next-position next-velocity) result))))
    (reverse result)
    result))

(defun multiple-steps (input time)
  (let ((next input))
    (dotimes (n time)
      (setf next (one-step next)))
    next))

(defun calculate-energy (list)
  (reduce #'(lambda (acc y) (+ acc (power y))) list :initial-value 0))

(defun power (x)
  (reduce #'* (map 'list #'(lambda (list) (apply #'+ (map 'list #'abs list))) x)))

(defun find-same (input)
  (let ((first input))
    (do ((next (one-step first) (one-step next))
         (count 1 (1+ count)))
        ((equal input next) count))))

(defun get-nth-only (nth input)
  (map 'list #'(lambda (point-velocity)
                 (let* ((point (car point-velocity))
                        (velocity (car (cdr point-velocity)))
                        (nth-value (nth nth point))
                        (zero-point (make-list (length point) :initial-element 0)))
                   (setf (nth nth zero-point) nth-value)
                   (list zero-point velocity))) input))

(defun calculate-repeat-count (input)
  (let ((number-of-coordinates (length (nth 0 (car input))))
        (result ()))
    (dotimes (n number-of-coordinates)
      (setf result (push (find-same (get-nth-only n input)) result)))
    (apply #'lcm result)))

;; day 12 part 1
(time (assert (= 8454 (calculate-energy (multiple-steps (input) 1000)))))

;; day 12 part 2
(time (assert (= 362336016722948 (calculate-repeat-count (input)))))
