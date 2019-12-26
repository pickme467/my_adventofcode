(defun input ()
  '(109 424 203 1 21102 11 1 0 1106 0 282 21102 18 1 0 1106 0 259 1201 1 0 221 203 1 21102 31 1 0 1106 0 282 21101 38 0 0 1106 0 259 21002 23 1 2 22102 1 1 3 21101 1 0 1 21102 57 1 0 1105 1 303 2102 1 1 222 20101 0 221 3 20101 0 221 2 21102 259 1 1 21101 80 0 0 1106 0 225 21101 0 44 2 21102 91 1 0 1105 1 303 1201 1 0 223 20101 0 222 4 21101 0 259 3 21102 225 1 2 21101 225 0 1 21102 118 1 0 1105 1 225 21002 222 1 3 21101 100 0 2 21101 133 0 0 1105 1 303 21202 1 -1 1 22001 223 1 1 21101 148 0 0 1106 0 259 2102 1 1 223 20102 1 221 4 21002 222 1 3 21102 1 12 2 1001 132 -2 224 1002 224 2 224 1001 224 3 224 1002 132 -1 132 1 224 132 224 21001 224 1 1 21102 1 195 0 106 0 108 20207 1 223 2 21002 23 1 1 21102 -1 1 3 21101 0 214 0 1105 1 303 22101 1 1 1 204 1 99 0 0 0 0 109 5 2102 1 -4 249 21201 -3 0 1 22101 0 -2 2 22101 0 -1 3 21101 0 250 0 1105 1 225 22102 1 1 -4 109 -5 2106 0 0 109 3 22107 0 -2 -1 21202 -1 2 -1 21201 -1 -1 -1 22202 -1 -2 -2 109 -3 2106 0 0 109 3 21207 -2 0 -1 1206 -1 294 104 0 99 21202 -2 1 -2 109 -3 2105 1 0 109 5 22207 -3 -4 -1 1206 -1 346 22201 -4 -3 -4 21202 -3 -1 -1 22201 -4 -1 2 21202 2 -1 -1 22201 -4 -1 1 22102 1 -2 3 21101 0 343 0 1105 1 303 1105 1 415 22207 -2 -3 -1 1206 -1 387 22201 -3 -2 -3 21202 -2 -1 -1 22201 -3 -1 3 21202 3 -1 -1 22201 -3 -1 2 21201 -4 0 1 21101 0 384 0 1106 0 303 1106 0 415 21202 -4 -1 -4 22201 -4 -3 -4 22202 -3 -2 -2 22202 -2 -4 -4 22202 -3 -2 -3 21202 -4 -1 -2 22201 -3 -2 1 22102 1 1 -4 109 -5 2106 0 0))

(defun newhash ()
  (make-hash-table :test #'equal))

(defun sethash (key value hash)
  (setf (gethash key hash) value))

(defun copyhash (hash)
  (let ((new (newhash)))
    (maphash #'(lambda (k v)
                 (sethash k v new)) hash)
    new))

(defun get-value (pos-or-value mode relative program)
  (let ((value
         (cond
           ((= 0 mode)
            (if (> 0 pos-or-value)
                (error "negative pointer not allowed in positional mode")
                (gethash pos-or-value program)))
           ((= 2 mode)
            (let ((address (+ relative pos-or-value)))
              (if (> 0 address)
                  (error "negative pointer not allowed in relative mode")
                  (gethash address  program))))
           (t pos-or-value))))
    (if (null value) 0 value)))

(defun get-at-index (index start program)
  (gethash (+ start index) program))

(defun get-nth (index mode relative start program)
  (get-value (get-at-index index start program) mode relative program))

(defun get-nth-write (index mode relative start program)
  (let ((pos (get-at-index index start program)))
    (cond
      ((= 0 mode) pos)
      ((= 2 mode) (+ pos relative))
      (t (error "mode not allowed for write parameter")))))

(defun do-three-argument (fn p1 p2 p3 relative start program)
  (let ((x (get-nth 1 p1 relative start program))
        (y (get-nth 2 p2 relative start program))
        (pos (get-nth-write 3 p3 relative start program)))
    (let ((value (funcall fn x y)))
      (sethash pos value program))))

(defun do-add (p1 p2 p3 relative start program)
  (do-three-argument #'+ p1 p2 p3 relative start program))

(defun do-multiply (p1 p2 p3 relative start program)
  (do-three-argument #'* p1 p2 p3 relative start program))

(defun do-input (input mode relative start program)
  (let ((x (get-nth-write 1 mode relative start program)))
    (if (null input) (error "nil input"))
    (sethash x input program)))

(defun do-output (p1 relative start program)
  (let ((value (get-nth 1 p1 relative start program)))
    value))

(defun do-jump-if-true (p1 p2 relative start program)
  (let ((x (get-nth 1 p1 relative start program)) (y (get-nth 2 p2 relative start program)))
    (if (/= 0 x)
        y
        (+ start 3))))

(defun do-jump-if-false (p1 p2 relative start program)
  (let ((x (get-nth 1 p1 relative start program)) (y (get-nth 2 p2 relative start program)))
    (if (= 0 x)
        y
        (+ start 3))))

(defun do-less-than (p1 p2 p3 relative start program)
  (let ((x (get-nth 1 p1 relative start program))
        (y (get-nth 2 p2 relative start program))
        (pos (get-nth-write 3 p3 relative start program)))
    (if (< x y)
        (sethash pos 1 program)
        (sethash pos 0 program))))

(defun do-equals (p1 p2 p3 relative start program)
  (let ((x (get-nth 1 p1 relative start program))
        (y (get-nth 2 p2 relative start program))
        (pos (get-nth-write 3 p3 relative start program)))
    (if (= x y)
        (sethash pos 1 program)
        (sethash pos 0 program))))

(defun do-relative (p1 relative start program)
  (let ((value (get-nth 1 p1 relative start program)))
    (+ relative value)))

(defun calculate (input start program relative output)
  (let ((command (gethash start program)))
    (multiple-value-bind (opcode p1 p2 p3) (decode-command command)
      (cond
        ((= opcode 99)
         (values 'done (list output start program relative)))
        ((= opcode 1)
         (do-add p1 p2 p3 relative start program)
         (calculate input (+ 4 start) program relative output))
        ((= opcode 2)
         (do-multiply p1 p2 p3 relative start program)
         (calculate input (+ 4 start) program relative output))
        ((= opcode 3)
         (if (null input)
             (values 'input (list output start program relative output))
             (progn
               (do-input (pop input) p1 relative start program)
               (calculate input (+ 2 start) program relative output))))
        ((= opcode 4)
         (let ((new-output (push (do-output p1 relative start program) output)))
           (calculate input (+ 2 start) program relative new-output)))
        ((= opcode 5)
         (calculate input (do-jump-if-true p1 p2 relative start program) program relative output))
        ((= opcode 6)
         (calculate input (do-jump-if-false p1 p2 relative start program) program relative output))
        ((= opcode 7)
         (do-less-than p1 p2 p3 relative start program)
         (calculate input (+ 4 start) program relative output))
        ((= opcode 8)
         (do-equals p1 p2 p3 relative start program)
         (calculate input (+ 4 start) program relative output))
        ((= opcode 9)
         (calculate input (+ 2 start) program (do-relative p1 relative start program) output))
        (t output)))))

(defun decode-command (command)
  (let* ((opcode (rem command 100))
         (p1 (rem (/ (- command opcode) 100) 10))
         (p2 (rem (/ (- command opcode (* 100 p1)) 1000) 10))
         (p3 (rem (/ (- command opcode (* 100 p1) (* 1000 p2)) 10000) 10)))
    (values opcode p1 p2 p3)))

(defun program-to-hash (list)
  (let ((i 0)
        (hash (newhash)))
    (dolist (k list)
      (sethash i k hash)
      (incf i))
    hash))

(defun fifty-by-fifty-beam (input)
  (let ((sum (newhash)))
    (loop for x from 0 to 49 do
         (loop for y from 0 to 49 do
              (if (= 1 (get-result-for x y input))
                  (sethash (list x y) 1 sum))))
    sum))

(defun get-result-for (x y input)
  (multiple-value-bind (done result)
      (calculate (list x y) 0 (program-to-hash input) 0 ())
    (declare (ignore done))
    (caar result)))

(defun beam-width (y input)
  (let* ((start
          (loop for d = 0 then (1+ d) do
               (if (= 1 (get-result-for d y input)) (return d))))
         (end
          (loop for d = (1+ start) then (1+ d) do
             (if (= 0 (get-result-for d y input)) (return d)))))
    (list start end)))

(defun beam-height (y shift input)
  (let ((position
         (destructuring-bind (start end) (beam-width y input)
           (declare (ignore start))
           (loop for d = y then (1+ d) do
              (if (= 0 (get-result-for (- end shift) d input)) (return d))))))
    (- position y)))

(defun find-100-square (start input)
  (let* ((y
          (loop for a = start then (1+ a) do
               (if (>= (beam-height a 100 input) 100) (return a))))
         (x (- (car (last (beam-width y input))) 100)))
    (format t "x: ~a, y: ~a~%" x y)
    (+ y (* 10000 x))))

;; day 19, part 1
(time (assert (= 147 (hash-table-count (fifty-by-fifty-beam (input))))))


;; day 19, part 2
(time (assert (= 13280865 (find-100-square 864 (input)))))
