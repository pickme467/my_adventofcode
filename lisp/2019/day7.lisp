(defun input () (list
                 3 8 1001 8 10 8 105 1 0 0 21 38 55 64 81 106 187
                 268 349 430 99999 3 9 101 2 9 9 1002 9
                 2 9 101 5 9 9 4 9 99 3 9 102 2 9 9 101 3 9 9 1002 9 4 9 4 9 99 3 9 102
                 2 9 9 4 9 99 3 9 1002 9 5 9 1001 9 4 9 102 4 9 9 4 9 99 3 9 102 2 9 9
                 1001 9 5 9 102 3 9 9 1001 9 4 9 102 5 9 9 4 9 99 3 9 1002 9 2 9 4 9 3 9
                 101 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 2 9 4 9 3 9 1001 9 2 9 4
                 9 3 9 101 1 9 9 4 9 3 9 1001 9 1 9 4 9 3 9 1001 9 2 9 4 9 3 9 101 1
                 9 9 4 9 3 9 1001 9 1 9 4 9 99 3 9 1002 9 2 9 4 9 3 9 101 2 9 9 4 9
                 3 9 1001 9 1 9 4 9 3 9 101 1 9 9 4 9 3 9 101 2 9 9 4 9 3 9 101 2 9
                 9 4 9 3 9 1001 9 1 9 4 9 3 9 101 1 9 9 4 9 3 9 102 2 9 9 4 9 3 9
                 101 2 9 9 4 9 99 3 9 1002 9 2 9 4 9 3 9 101 2 9 9 4 9 3 9 102 2 9 9
                 4 9 3 9 101 2 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 1002 9 2 9 4 9 3 9 1002
                 9 2 9 4 9 3 9 101 2 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 101 1 9 9 4 9
                 99 3 9 102 2 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 1002 9 2 9 4 9 3 9 102 2
                 9 9 4 9 3 9 102 2 9 9 4 9 3 9 101 2 9 9 4 9 3 9 101 1 9 9 4 9 3
                 9 101 1 9 9 4 9 3 9 1001 9 1 9 4 9 3 9 102 2 9 9 4 9 99 3 9 101 1 9
                 9 4 9 3 9 1002 9 2 9 4 9 3 9 102 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9
                 101 1 9 9 4 9 3 9 102 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1002 9 2 9 4
                 9 3 9 101 1 9 9 4 9 3 9 102 2 9 9 4 9 99))

(defun sample1 () (list 3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0))

(defun sample2 () (list 3 23 3 24 1002 24 10 24 1002 23 -1 23
                        101 5 23 23 1 24 23 23 4 23 99 0 0))

(defun sample3 () (list 3 31 3 32 1002 32 10 32 1001 31 -2 31 1007 31 0 33
                        1002 33 7 33 1 33 31 31 1 32 31 31 4 31 99 0 0 0))

(defun get-value (pos-or-value mode list)
  (if (= 0 mode)
      (nth pos-or-value list)
      pos-or-value))

(defun get-nth (index mode start list)
  (let ((x (nth (+ start index) list)))
    (get-value x mode list)))

(defun do-three-argument (fn p1 p2 start list)
  (let ((x (get-nth 1 p1 start list))
        (y (get-nth 2 p2 start list))
        (pos (get-nth 3 1 start list)))
    (setf (nth pos list) (funcall fn x y))))

(defun do-add (p1 p2 start list)
  (do-three-argument #'+ p1 p2 start list))

(defun do-multiply (p1 p2 start list)
  (do-three-argument #'* p1 p2 start list))

(defun do-input (input start list)
  (let ((x (get-nth 1 1 start list)))
    (setf (nth x list) input)))

(defun do-output (p1 start list)
  (get-nth 1 p1 start list))

(defun do-jump-if-true (p1 p2 start list)
  (let ((x (get-nth 1 p1 start list)) (y (get-nth 2 p2 start list)))
    (if (/= 0 x)
        y
        (+ start 3))))

(defun do-jump-if-false (p1 p2 start list)
  (let ((x (get-nth 1 p1 start list)) (y (get-nth 2 p2 start list)))
    (if (= 0 x)
        y
        (+ start 3))))

(defun do-less-than (p1 p2 start list)
  (let ((x (get-nth 1 p1 start list)) (y (get-nth 2 p2 start list)) (pos (get-nth 3 1 start list)))
    (if (< x y)
        (setf (nth pos list) 1)
        (setf (nth pos list) 0))))

(defun do-equals (p1 p2 start list)
  (let ((x (get-nth 1 p1 start list)) (y (get-nth 2 p2 start list)) (pos (get-nth 3 1 start list)))
    (if (= x y)
        (setf (nth pos list) 1)
        (setf (nth pos list) 0))))

(defun calculate (start list input output)
  (let ((command (nth start list)))
    (multiple-value-bind (opcode p1 p2 p3) (decode-command command)
      (declare (ignore p3))
      (cond
        ((= opcode 99)
         output)
        ((= opcode 1)
         (do-add p1 p2 start list)
         (calculate (+ 4 start) list input output))
        ((= opcode 2)
         (do-multiply p1 p2 start list)
         (calculate (+ 4 start) list input output))
        ((= opcode 3)
         (do-input (pop input) start list)
         (calculate (+ 2 start) list input output))
        ((= opcode 4)
         (calculate (+ 2 start) list input (do-output p1 start list)))
        ((= opcode 5)
         (calculate (do-jump-if-true p1 p2 start list) list input output))
        ((= opcode 6)
         (calculate (do-jump-if-false p1 p2 start list) list input output))
        ((= opcode 7)
         (do-less-than p1 p2 start list)
         (calculate (+ 4 start) list input output))
        ((= opcode 8)
         (do-equals p1 p2 start list)
         (calculate (+ 4 start) list input output))
        (t output))
      )))

(defun decode-command (command)
  (let* ((opcode (rem command 100))
         (p1 (rem (/ (- command opcode) 100) 10))
         (p2 (rem (/ (- command opcode (* 100 p1)) 1000) 10))
         (p3 (rem (/ (- command opcode (* 100 p1) (* 1000 p2)) 10000) 10)))
    (values opcode p1 p2 p3)))

(defun amplifier-values (input-fun m1 m2 m3 m4 m5)
  (let* ((a1-out (calculate 0 (funcall input-fun) (list m1 0) 0))
         (a2-out (calculate 0 (funcall input-fun) (list m2 a1-out) 0))
         (a3-out (calculate 0 (funcall input-fun) (list m3 a2-out) 0))
         (a4-out (calculate 0 (funcall input-fun) (list m4 a3-out) 0))
         (a5-out (calculate 0 (funcall input-fun) (list m5 a4-out) 0)))
    a5-out))

(defun find-max-amplification (input-fun)
  (let ((output ())
        (vector-output ()))
    (dotimes (m1 5)
      (dotimes (m2 5)
        (dotimes (m3 5)
          (dotimes (m4 5)
            (dotimes (m5 5)
              (if (= 5 (length (delete-duplicates (list m1 m2 m3 m4 m5))))
                  (let ((result (amplifier-values input-fun m1 m2 m3 m4 m5)))
                    (push (vector result m1 m2 m3 m4 m5) vector-output)
                    (push result output))))))))
    (apply #'max  output)))

(defun find-max-amplification-boosted (input-fun)
    (let ((output ())
        (vector-output ()))
    (dotimes (m1 5)
      (dotimes (m2 5)
        (dotimes (m3 5)
          (dotimes (m4 5)
            (dotimes (m5 5)
              (if (= 5 (length (delete-duplicates (list m1 m2 m3 m4 m5))))
                  (let ((result
                         (amplifier-values input-fun (+ 5  m1) (+ 5 m2)
                                           (+ 5 m3) (+ 5 m4) (+ 5 m5))))
                    (push (vector result (+ 5  m1) (+ 5 m2)
                                  (+ 5 m3) (+ 5 m4) (+ 5 m5)) vector-output)
                    (push result output))))))))
    (apply #'max  output)))

;; verifications
(time (assert (= 43210 (amplifier-values #'sample1 4 3 2 1 0))))
(time (assert (= 54321 (amplifier-values #'sample2 0 1 2 3 4))))
(time (assert (= 65210 (amplifier-values #'sample3 1 0 4 3 2))))

;; day 7 part 1
(time (assert (= 117312 (find-max-amplification #'input))))

;; part 2
(defun calculate-with-interrupt (context)
  (let ((new-context (process-command context)))
    (if (equal 'continue (elt new-context 0))
        (calculate-with-interrupt new-context)
        new-context)))

(defun feedback-loop (p1 p2 p3 p4 p5)
  (let ((c1 (vector 'continue 0 (input) 0 0))
        (c2 (vector 'continue 0 (input) 0 0))
        (c3 (vector 'continue 0 (input) 0 0))
        (c4 (vector 'continue 0 (input) 0 0))
        (c5 (vector 'continue 0 (input) 0 0)))
    (setf c1 (calculate-with-interrupt c1))
    (setf (elt c1 4) p1)
    (setf c1 (calculate-with-interrupt c1))
    (setf c2 (calculate-with-interrupt c2))
    (setf (elt c2 4) p2)
    (setf c2 (calculate-with-interrupt c2))
    (setf c3 (calculate-with-interrupt c3))
    (setf (elt c3 4) p3)
    (setf c3 (calculate-with-interrupt c3))
    (setf c4 (calculate-with-interrupt c4))
    (setf (elt c4 4) p4)
    (setf c4 (calculate-with-interrupt c4))
    (setf c5 (calculate-with-interrupt c5))
    (setf (elt c5 4) p5)
    (setf c5 (calculate-with-interrupt c5))
    (setf (elt c1 4) 0)
    (setf c1 (calculate-with-interrupt c1))
    (vector c1 c2 c3 c4 c5)
    ))
(defun process-command (context)
  (let* ((disposition (elt context 0))
         (start (elt context 1))
         (list (elt context 2))
         (output (elt context 3))
         (input (elt context 4)) (command (nth start list)))
    (if (equal disposition 'input)
        (progn
          (do-input input start list)
          (setf start (+ 2 start))
          (setf command (nth start list))))
    (multiple-value-bind (opcode p1 p2 p3) (decode-command command)
      (declare (ignore p3))
      (cond
        ((= opcode 99)
         (vector 'done output))
        ((= opcode 1)
         (do-add p1 p2 start list)
         (vector 'continue (+ 4 start) list output 0))
        ((= opcode 2)
         (do-multiply p1 p2 start list)
         (vector 'continue (+ 4 start) list output 0))
        ((= opcode 3)
         (vector 'input start list output 0))
        ((= opcode 4)
         (vector 'output (+ 2 start) list (do-output p1 start list) 0))
        ((= opcode 5)
         (vector 'continue (do-jump-if-true p1 p2 start list) list output 0))
        ((= opcode 6)
         (vector 'continue (do-jump-if-false p1 p2 start list) list output 0))
        ((= opcode 7)
         (do-less-than p1 p2 start list)
         (vector 'continue (+ 4 start) list output 0))
        ((= opcode 8)
         (do-equals p1 p2 start list)
         (vector 'continue (+ 4 start) list output 0))
        (t (vector 'error output)) ))))
