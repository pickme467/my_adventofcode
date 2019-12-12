(defun input () (list 3 8 1005 8 330 1106 0 11 0 0 0 104 1 104 0 3 8 102 -1 8
                      10 1001 10 1 10 4 10 108 0 8 10 4 10 1001 8 0 28 1 1103 17
                      10 1006 0 99 1006 0 91 1 102 7 10 3 8 1002 8 -1 10 101 1 10
                      10 4 10 108 1 8 10 4 10 1002 8 1 64 3 8 102 -1 8 10 1001
                      10 1 10 4 10 108 0 8 10 4 10 102 1 8 86 2 4 0 10 1006
                      0 62 2 1106 13 10 3 8 1002 8 -1 10 1001 10 1 10 4 10 1008 8
                      0 10 4 10 101 0 8 120 1 1109 1 10 1 105 5 10 3 8 102 -1
                      8 10 1001 10 1 10 4 10 108 1 8 10 4 10 1002 8 1 149 1 108
                      7 10 1006 0 40 1 6 0 10 2 8 9 10 3 8 102 -1 8 10 1001
                      10 1 10 4 10 1008 8 1 10 4 10 1002 8 1 187 1 1105 10 10 3
                      8 102 -1 8 10 1001 10 1 10 4 10 1008 8 1 10 4 10 1002 8 1
                      213 1006 0 65 1006 0 89 1 1003 14 10 3 8 102 -1 8 10 1001 10 1
                      10 4 10 108 0 8 10 4 10 102 1 8 244 2 1106 14 10 1006 0 13
                      3 8 102 -1 8 10 1001 10 1 10 4 10 108 0 8 10 4 10 1001 8
                      0 273 3 8 1002 8 -1 10 1001 10 1 10 4 10 108 1 8 10 4 10
                      1001 8 0 295 1 104 4 10 2 108 20 10 1006 0 94 1006 0 9 101 1
                      9 9 1007 9 998 10 1005 10 15 99 109 652 104 0 104 1 21102 937268450196 1 1
                      21102 1 347 0 1106 0 451 21101 387512636308 0 1 21102 358 1 0 1105 1 451 3 10
                      104 0 104 1 3 10 104 0 104 0 3 10 104 0 104 1 3 10 104 0
                      104 1 3 10 104 0 104 0 3 10 104 0 104 1 21101 0 97751428099 1 21102 1
                      405 0 1105 1 451 21102 1 179355806811 1 21101 416 0 0 1106 0 451 3 10 104 0
                      104 0 3 10 104 0 104 0 21102 1 868389643008 1 21102 439 1 0 1105 1 451 21102
                      1 709475853160 1 21102 450 1 0 1105 1 451 99 109 2 22102 1 -1 1 21101 0 40
                      2 21101 482 0 3 21102 1 472 0 1105 1 515 109 -2 2106 0 0 0 1 0
                      0 1 109 2 3 10 204 -1 1001 477 478 493 4 0 1001 477 1 477 108 4
                      477 10 1006 10 509 1101 0 0 477 109 -2 2105 1 0 0 109 4 2101 0 -1
                      514 1207 -3 0 10 1006 10 532 21101 0 0 -3 21202 -3 1 1 22101 0 -2 2
                      21101 1 0 3 21101 0 551 0 1105 1 556 109 -4 2106 0 0 109 5 1207 -3
                      1 10 1006 10 579 2207 -4 -2 10 1006 10 579 22102 1 -4 -4 1105 1 647 21201
                      -4 0 1 21201 -3 -1 2 21202 -2 2 3 21101 0 598 0 1106 0 556 22101 0
                      1 -4 21102 1 1 -1 2207 -4 -2 10 1006 10 617 21101 0 0 -1 22202 -2 -1
                      -2 2107 0 -3 10 1006 10 639 22102 1 -1 1 21102 1 639 0 105 1 514 21202
                      -2 -1 -2 22201 -4 -2 -4 109 -5 2105 1 0))

(defun sethash (key value hash)
  (setf (gethash key hash) value))

(defun copy-hash (source)
  (let ((new-hash (make-hash-table :test #'equal)))
    (maphash #'(lambda (key value) (sethash key value new-hash)) source)
    new-hash))

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
         'done)
        ((= opcode 1)
         (do-add p1 p2 p3 relative start program)
         (calculate input (+ 4 start) program relative output))
        ((= opcode 2)
         (do-multiply p1 p2 p3 relative start program)
         (calculate input (+ 4 start) program relative output))
        ((= opcode 3)
         (if (null input)
             (list start program relative output)
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

(defun list-to-hash (list)
  (let ((address 0)
        (hash (make-hash-table :test #'equal)))
    (dolist (element list)
      (sethash address element hash)
      (incf address))
    hash))

(defun remember-paintings (start input)
  (let ((result (make-hash-table :test #'equal))
        (current-location '(0 0 up)))
    (do ((context (calculate (list start) 0 (list-to-hash input) 0 ())
                  (apply #'calculate (list (get-color current-location result)) context)))
        ((equal 'done context) (values (hash-table-count result) result))
      (let ((output (nth 3 context)))
        (if  (= 0 (mod (length output) 2))
             (let ((color (nth 1 output))
                   (direction (nth 0 output)))
               (paint current-location color result)
               (setf current-location (update-location current-location direction))))))))

(defun paint (location color hash)
  (let ((x (nth 0 location))
        (y (nth 1 location)))
    (sethash (list x y) color hash)))

(defun get-color (location hash)
  (let* ((x (nth 0 location))
         (y (nth 1 location))
         (color (gethash (list x y) hash)))
    (if (null color) 0 color)))

(defun update-location (location direction)
  (let* ((x (nth 0 location))
         (y (nth 1 location))
         (new-heading (find-new-heading (nth 2 location) direction)))
    (cond
      ((equal new-heading 'up)
       (list x (1- y) new-heading))
      ((equal new-heading 'right)
       (list (1+ x) y new-heading))
      ((equal new-heading 'down)
       (list x (1+ y) new-heading))
      ((equal new-heading 'left)
       (list (1- x) y new-heading)))))

(defun find-new-heading (current direction)
  (let ((directions-right (list 'up 'right 'down 'left 'up))
        (directions-left (list 'up 'left 'down 'right 'up)))
    (cond
      ((= 0 direction) (nth (1+ (position current directions-left)) directions-left))
      (t (nth (1+ (position current directions-right)) directions-right)))))

(defun make-paint (hash)
  (let ((max-x 0)
        (max-y 0))
    (maphash #'(lambda (key value)
                 (declare (ignore value))
                 (let ((x (nth 0 key))
                       (y (nth 1 key)))
                   (if (< max-x x) (setf max-x x))
                   (if (< max-y y) (setf max-y y)))) hash)
    (dotimes (y (1+ max-y))
      (dotimes (x (1+ max-x))
        (format t "~a" (color-to-space-or-hash (get-color (list x y) hash))))
      (format t "~%"))))

(defun color-to-space-or-hash (color)
  (if (= 0 color) #\  #\#))

;; ;; day 11 part 1
(time (assert (= 1564 (remember-paintings 0 (input)))))

;; ;; day 11 part 1
(time (multiple-value-bind (result hash) (remember-paintings 1 (input))
        (assert (= 248 result))
        (make-paint hash)))
