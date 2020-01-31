(defun input ()
"deal with increment 2
cut 3310
deal with increment 13
cut -9214
deal with increment 14
deal into new stack
deal with increment 26
deal into new stack
deal with increment 62
cut -1574
deal with increment 74
cut -7102
deal with increment 41
cut 7618
deal with increment 70
cut 7943
deal into new stack
deal with increment 52
cut -3134
deal with increment 21
deal into new stack
deal with increment 20
deal into new stack
deal with increment 61
cut -2810
deal with increment 60
cut 3355
deal with increment 13
cut 3562
deal with increment 55
cut 2600
deal with increment 47
deal into new stack
cut -7010
deal with increment 34
cut 1726
deal with increment 61
cut 2805
deal with increment 39
cut 1907
deal into new stack
cut 3915
deal with increment 14
cut -6590
deal into new stack
deal with increment 73
deal into new stack
deal with increment 31
cut 1000
deal with increment 3
cut 8355
deal with increment 2
cut -5283
deal with increment 50
cut -7150
deal with increment 71
cut 6728
deal with increment 58
cut -814
deal with increment 14
cut -8392
deal with increment 71
cut 7674
deal with increment 46
deal into new stack
deal with increment 55
cut 7026
deal with increment 17
cut 1178
deal with increment 10
cut -8205
deal with increment 27
cut -55
deal with increment 44
cut -2392
deal into new stack
cut 7385
deal with increment 36
cut -399
deal with increment 74
cut 6895
deal with increment 20
cut 4346
deal with increment 15
cut -4088
deal with increment 3
cut 1229
deal with increment 59
cut 4708
deal with increment 62
cut 2426
deal with increment 30
cut 7642
deal with increment 73
cut 9049
deal into new stack
cut -3866
deal with increment 68
deal into new stack
cut 1407")

(ql:quickload "cl-utilities")

(defun newhash ()
  (make-hash-table :test #'equal))

(defun sethash (key value hash)
  (setf (gethash key hash) value)
  hash)

(defun string-to-commands (input map)
  (loop for command in (cl-utilities:split-sequence #\NewLine input)
        collecting (parse command map)))

(defun parse (command map)
  (cond
    ((equal command "deal into new stack") (list (gethash 'deal-new-stack map) nil))
    ((not (null (parse-if-command-with-number "cut " command (gethash 'cut map))))
     (parse-if-command-with-number "cut " command (gethash 'cut map)))
    ((not (null (parse-if-command-with-number "deal with increment " command
                                              (gethash 'deal-with map))))
     (parse-if-command-with-number "deal with increment " command (gethash 'deal-with map)))
    (t (list command))))

(defun part-one-map ()
  (let ((h (newhash)))
    (sethash 'deal-new-stack #'deal-into-new-stack h)
    (sethash 'cut #'cut h)
    (sethash 'deal-with #'deal-with-increment h)
    h))

(defun parse-if-command-with-number (command string type)
  (cond
    ((not (null (search command string)))
     (list type (parse-integer (subseq string (+ (length command) (search command string))))))
    (t nil)))

(defun deal-into-new-stack (stack value)
  (declare (ignore value))
  (reverse stack))

(defun cut (stack value)
  (if (< value 0)
      (setf value (+ (length stack) value)))
   (let ((s1 (subseq stack 0 value))
         (s2 (subseq stack value)))
     (append s2 s1)))

(defun deal-with-increment (stack value)
  (let ((h (newhash))
        (l (length stack)))
    (loop for i in stack
       for index = 0 then (mod (+ index value) l)
       do (sethash index i h))
    (loop for index from 0 to (1- l) collecting (gethash index h))))

(defun execute-sequence (sequence collection)
  (loop for (command param) in sequence do
       (setf collection (funcall command collection param)))
  collection)

(defun sample1 ()
  (list (list #'deal-with-increment 7)
        (list #'deal-into-new-stack)
        (list #'deal-into-new-stack)))

(defun sample2 ()
  (list (list #'cut 6)
        (list #'deal-with-increment 7)
        (list #'deal-into-new-stack)))

(defun sample3 ()
  (list (list #'deal-with-increment 7)
        (list #'deal-with-increment 9)
        (list #'cut -2)))

(defun sample4 ()
  (list (list #'deal-into-new-stack)
        (list #'cut -2)
        (list #'deal-with-increment 7)
        (list #'cut 8)
        (list #'cut -4)
        (list #'deal-with-increment 7)
        (list #'cut 3)
        (list #'deal-with-increment 9)
        (list #'deal-with-increment 3)
        (list #'cut -1)))

(defun input-collection (&optional (end 10007))
  (loop for i from 0 to (1- end) collecting i))

(defun iterate (input collection)
  (let ((commands (string-to-commands input (part-one-map))))
    (loop for i = 0 then (1+ i)
       for s = collection then (execute-sequence commands s)
       do
         (if (and (> i 0) (= (nth 2020 s) 2020))
             (progn
               (format t "i: ~a~%coll:~%~a~%" i s)
               (return (list i s)))))))


(defun iterate-n-times-and-get-kth (input collection n k)
  (let ((commands (string-to-commands input (part-one-map))))
    (loop for i from 0 to (1- n)
       for s = collection then (execute-sequence commands s) collecting (nth k s))))

(defun track-index (index commands length)
  (let ((next-index index))
    (loop for (function param) in commands do
         (setf next-index (funcall function param next-index length)))
    next-index))

(defun part-two-map ()
(let ((h (newhash)))
  (sethash 'deal-new-stack #'reverse-index h)
  (sethash 'cut #'cut-n-index h)
  (sethash 'deal-with #'increment-n-index h)
  h))

(defun reverse-index (ignore index length)
  (declare (ignore ignore))
  (- length (1+ index)))

(defun cut-n-index (n index length)
  (mod (- index n) length))

(defun increment-n-index (n index length)
  (mod (* index n) length))

(defun shuffle-times ()
  101741582076661)

(defun collection-lenght ()
  119315717514047)

(defun time-test (d length)
  (loop for i = 0 then (1+ i)
        for index = d
          then (track-index index (string-to-commands (input) (part-two-map)) length)
        until (and (= index d) (> i 0))
       maximizing i))


;; day 22, part 1
(time (assert
       (= 1498 (search '(2019)
                       (execute-sequence (string-to-commands (input) (part-one-map))
                                         (input-collection))))))

;; day 22, part 1 (no-list solution)
(time (assert (= 1498 (track-index 2019 (string-to-commands (input) (part-two-map)) 10007))))
