(defun input () '(((-10 -13 7) (0 0 0))
                  ((1 2 1) (0 0 0))
                  ((-15 -3 13) (0 0 0))
                  ((3 7 -4) (0 0 0))))

(defun input2 () '(((-10 0 0) (0 0 0))
                  ((1 0 0) ( 0 0))
                  ((-15 0 0) (0 0 0))
                  ((3 0 0) (0 0 0))))

(defun input3 () '(((0 -13 0) (0 0 0))
                  ((0 2 0) ( 0 0))
                  ((0 -3 0) (0 0 0))
                  ((0 7 0) (0 0 0))))

(defun input4 () '(((0 0 7) (0 0 0))
                  ((0 0 1) ( 0 0))
                  ((0 0 13) (0 0 0))
                  ((0 0 -4) (0 0 0))))

(defun velocity (a b)
  (cond
    ((< a b) 1)
    ((> a b) -1)
    (t 0)))

(defun new-velocity (position-velocity1 position-velocity2 position-velocity3 position-velocity4)
  (let* ((velocity-change1
          (velocity-change position-velocity1 position-velocity2))
         (velocity-change2
          (velocity-change position-velocity1 position-velocity3))
         (velocity-change3
          (velocity-change position-velocity1 position-velocity4)))
    (mapcar #'+ velocity-change1 velocity-change2 velocity-change3 (nth 1 position-velocity1))))


(defun velocity-change (position-velocity1 position-velocity2)
  (mapcar #'velocity (car position-velocity1) (car position-velocity2)))

(defun new-position (position velocity)
  (mapcar #'+ position velocity))

(defun one-step (input times)
  (let ((steps (list input)))
    (dotimes (n times)
      (let* ((current-step (car steps))
             (next-velocity1 (new-velocity (nth 0 current-step) (nth 1 current-step) (nth 2 current-step) (nth 3 current-step)))
             (next-position1 (new-position (nth 0 (nth 0 current-step)) next-velocity1))
             (next-velocity2 (new-velocity (nth 1 current-step) (nth 0 current-step) (nth 2 current-step) (nth 3 current-step)))
             (next-position2 (new-position (nth 0 (nth 1 current-step)) next-velocity2))
             (next-velocity3 (new-velocity (nth 2 current-step) (nth 1 current-step) (nth 0 current-step) (nth 3 current-step)))
             (next-position3 (new-position (nth 0 (nth 2 current-step)) next-velocity3))
             (next-velocity4 (new-velocity (nth 3 current-step) (nth 1 current-step) (nth 2 current-step) (nth 0 current-step)))
             (next-position4 (new-position (nth 0 (nth 3 current-step)) next-velocity4)))
        (setf steps (push (list (list next-position1 next-velocity1)
                                (list next-position2 next-velocity2)
                                (list next-position3 next-velocity3)
                                (list next-position4 next-velocity4)) steps))))
    steps))

(defun sample () '(((-1 0 2) (0 0 0))
                   ((2 -10 -7) (0 0 0))
                   ((4 -8 8) (0 0 0))
                   ((3 5 -1) (0 0 0))))

(defun sample2 () '(((-1 0 0) (0 0 0))
                   ((2 0 0) (0 0 0))
                   ((4 0 0) (0 0 0))
                   ((3 0 0) (0 0 0))))

(defun sample3 () '(((0 0 0) (0 0 0))
                   ((0 -10 0) (0 0 0))
                   ((0 -8 0) (0 0 0))
                   ((0 5 0) (0 0 0))))

(defun sample4 () '(((0 0 2) (0 0 0))
                   ((0 0 -7) (0 0 0))
                   ((0 0 8) (0 0 0))
                   ((0 0 -1) (0 0 0))))

(defun calculate-energy (list)
  (reduce #'(lambda (acc y) (+ acc (power y))) list :initial-value 0))

(defun power (x)
  (reduce #'* (map 'list #'(lambda (list) (apply #'+ (map 'list #'abs list))) x)))

;; day 12 part 1
(time (assert (= 8454 (calculate-energy (car (one-step (input) 1000))))))

(time (assert (equal (sample) (car (one-step (sample) 2772)))))

(defun find-same (input)
  (let ((first input))
    (do ((next (car (one-step first 1)) (car (one-step next 1)))
         (count 1 (1+ count)))
        ((equal input next) count))))
