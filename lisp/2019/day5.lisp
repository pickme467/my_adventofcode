(defun input () (list 3 225 1 225 6 6 1100 1 238 225
                      104 0 1101 33 37 225 101 6 218 224 1001
                      224 -82 224 4 224 102 8 223 223 101 7
                      224 224 1 223 224 223 1102 87 62 225 1102
                      75 65 224 1001 224 -4875 224 4 224 1002 223
                      8 223 1001 224 5 224 1 224 223 223 1102
                      49 27 225 1101 6 9 225 2 69 118 224
                      101 -300 224 224 4 224 102 8 223 223 101
                      6 224 224 1 224 223 223 1101 76 37 224
                      1001 224 -113 224 4 224 1002 223 8 223 101
                      5 224 224 1 224 223 223 1101 47 50 225
                      102 43 165 224 1001 224 -473 224 4 224 102
                      8 223 223 1001 224 3 224 1 224 223 223
                      1002 39 86 224 101 -7482 224 224 4 224 102
                      8 223 223 1001 224 6 224 1 223 224 223
                      1102 11 82 225 1 213 65 224 1001 224 -102
                      224 4 224 1002 223 8 223 1001 224 6 224
                      1 224 223 223 1001 14 83 224 1001 224 -120
                      224 4 224 1002 223 8 223 101 1 224 224
                      1 223 224 223 1102 53 39 225 1101 65 76
                      225 4 223 99 0 0 0 677 0 0 0
                      0 0 0 0 0 0 0 0 1105 0 99999
                      1105 227 247 1105 1 99999 1005 227 99999 1005 0
                      256 1105 1 99999 1106 227 99999 1106 0 265 1105
                      1 99999 1006 0 99999 1006 227 274 1105 1 99999
                      1105 1 280 1105 1 99999 1 225 225 225 1101
                      294 0 0 105 1 0 1105 1 99999 1106 0
                      300 1105 1 99999 1 225 225 225 1101 314 0
                      0 106 0 0 1105 1 99999 1107 677 226 224
                      1002 223 2 223 1005 224 329 101 1 223 223
                      8 677 226 224 102 2 223 223 1006 224 344
                      1001 223 1 223 108 677 677 224 1002 223 2
                      223 1006 224 359 1001 223 1 223 1108 226 677
                      224 102 2 223 223 1006 224 374 1001 223 1
                      223 1008 677 226 224 102 2 223 223 1005 224
                      389 101 1 223 223 7 226 677 224 102 2
                      223 223 1005 224 404 1001 223 1 223 1007 677
                      677 224 1002 223 2 223 1006 224 419 101 1
                      223 223 107 677 226 224 102 2 223 223 1006
                      224 434 101 1 223 223 7 677 677 224 1002
                      223 2 223 1005 224 449 101 1 223 223 108
                      677 226 224 1002 223 2 223 1006 224 464 101
                      1 223 223 1008 226 226 224 1002 223 2 223
                      1006 224 479 101 1 223 223 107 677 677 224
                      1002 223 2 223 1006 224 494 1001 223 1 223
                      1108 677 226 224 102 2 223 223 1005 224 509
                      101 1 223 223 1007 226 677 224 102 2 223
                      223 1005 224 524 1001 223 1 223 1008 677 677
                      224 102 2 223 223 1005 224 539 1001 223 1
                      223 1107 677 677 224 1002 223 2 223 1006 224
                      554 1001 223 1 223 1007 226 226 224 1002 223
                      2 223 1005 224 569 1001 223 1 223 7 677
                      226 224 1002 223 2 223 1006 224 584 1001 223
                      1 223 108 226 226 224 102 2 223 223 1005
                      224 599 1001 223 1 223 8 677 677 224 102
                      2 223 223 1005 224 614 1001 223 1 223 1107
                      226 677 224 102 2 223 223 1005 224 629 1001
                      223 1 223 8 226 677 224 102 2 223 223
                      1006 224 644 1001 223 1 223 1108 226 226 224
                      1002 223 2 223 1006 224 659 101 1 223 223
                      107 226 226 224 1002 223 2 223 1006 224 674
                      1001 223 1 223 4 223 99 226))

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

(defun do-save (input start list)
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
         (do-save input start list)
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

;; day 5 part 1
(time (assert (= 16209841 (calculate 0 (input) 1 0))))

;; day 5 part 2
(time (assert (= 8834787 (calculate 0 (input) 5 0))))
