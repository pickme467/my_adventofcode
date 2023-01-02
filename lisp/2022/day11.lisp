(defun get-items (monkey input)
  (cadadr (assoc monkey input)))

(defun get-operation (monkey input)
  (cadr (caddr (assoc monkey input))))

(defun get-test (monkey input)
  (cadr (cadddr (assoc monkey input))))

(defun append-item (item monkey input)
  (let ((items (get-items monkey input)))
    (setf (car (cdr (assoc monkey input))) (list 'starting (append items (list item))))))

(defun nullify-items (monkey input)
  (setf (car (cdr (assoc monkey input))) '(starting ())))

(defun do-math (op n)
  (destructuring-bind (op-name &optional value) op
    (cond
      ((equal op-name 'multiply) (* value n))
      ((equal op-name 'plus) (+ value n))
      ((equal op-name 'power) (* n n)))))

(defun do-test (test n)
  (destructuring-bind (mod-value true-value false-value) test
    (if (equal 0 (mod n mod-value)) true-value false-value)))

(defun run-a-round (input)
  (loop for (m rest) in input
        collect (list m (length (get-items m input)))
        do (loop for i in (get-items m input)
                 for op = (get-operation m input)
                 for test = (get-test m input)
                 for worry-level = (do-math op i)
                 for paced-level = (floor worry-level 3)
                 for test-result = (do-test test paced-level)
                 do (append-item paced-level test-result input))
        do (nullify-items m input)))

(defun run-rounds (rounds input)
  (let ((input-copy (copy-tree input)))
    (loop for r from 1 to rounds append (run-a-round input-copy))))

(defun find-max-two (result)
  (loop for (m v) in result
        for current = (getf output m 0)
        with output = '()
        do (setf (getf output m) (+ current v))
        finally (let ((sorted (sort output #'>)))
                  (return (* (first sorted) (second sorted))))))

(defun run-a-stress-round (input)
  (loop for (m rest) in input
        collect (list m (length (get-items m input)))
        do (loop for i in (get-items m input)
                 for op = (get-operation m input)
                 for test = (get-test m input)
                 for worry-level = (do-modulo-math op i)
                 for test-result = (do-modulo-test test worry-level)
                 do (append-item worry-level test-result input))
        do (nullify-items m input)))

(defun run-stress-rounds (rounds input)
  (let ((input-copy (copy-tree input)))
    (loop for r from 1 to rounds append (run-a-stress-round input-copy))))

(defun do-modulo-math (op n)
  (destructuring-bind (op-name &optional value) op
    (cond
      ((equal op-name 'multiply)
       (multiply-modulos (make-modulo-number value) (make-modulo-number n)))
      ((equal op-name 'plus)
       (add-modulos (make-modulo-number value) (make-modulo-number n)))
      ((equal op-name 'power)
       (multiply-modulos (make-modulo-number n) (make-modulo-number n))))))

(defun do-modulo-test (test n)
  (destructuring-bind (mod-value true-value false-value) test
    (if (equal 0 (cadr (assoc mod-value n))) true-value false-value)))

(defun make-modulo-number (number)
  (cond
    ((listp number) number)
    (t (let ((modulos '(2 3 5 7 11 13 17 19 23)))
         (loop for modulo in modulos collecting (list modulo (mod number modulo)))))))

(defun operate-on-modulos (operation modulo-one -modulo-two)
  (mapcar #'(lambda (one two)
              (list (car one) (mod (funcall operation (cadr one) (cadr two)) (car one))))
          modulo-one -modulo-two))

(defun add-modulos (modulo-one modulo-two)
  (operate-on-modulos #'+ modulo-one modulo-two))

(defun multiply-modulos (modulo-one modulo-two)
  (operate-on-modulos #'* modulo-one modulo-two))

(defun day11-part1 ()
  (assert (equal 110264 (find-max-two (run-rounds 20 (input))))))

(defun day11-part2 ()
  (assert (equal 23612457316 (find-max-two (run-stress-rounds 10000 (input))))))

(defun input ()
  '(
    (0 . ((Starting (65 78))
          (Operation (multiply 3))
          (Test (5 2 3))))
    (1 . ((Starting (54 78 86 79 73 64 85 88))
          (Operation (plus 8))
          (Test (11 4 7))))
    (2 . ((Starting (69 97 77 88 87))
          (Operation (plus 2))
          (Test (2 5 3))))
    (3 . ((Starting (99))
          (Operation (plus 4))
          (Test (13 1 5))))
    (4 . ((Starting (60 57 52))
          (Operation (multiply 19))
          (Test (7 7 6))))
    (5 . ((Starting (91 82 85 73 84 53))
          (Operation (plus 5))
          (Test (3 4 1))))
    (6 . ((Starting (88 74 68 56))
          (Operation (power))
          (Test (17 0 2))))
    (7 . ((Starting (54 82 72 71 53 99 67))
          (Operation (plus 1))
          (Test (19 6 0))))))
