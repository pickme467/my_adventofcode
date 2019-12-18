(defun input ()
  "59790132880344516900093091154955597199863490073342910249565395038806135885706290664499164028251508292041959926849162473699550018653393834944216172810195882161876866188294352485183178740261279280213486011018791012560046012995409807741782162189252951939029564062935408459914894373210511494699108265315264830173403743547300700976944780004513514866386570658448247527151658945604790687693036691590606045331434271899594734825392560698221510565391059565109571638751133487824774572142934078485772422422132834305704887084146829228294925039109858598295988853017494057928948890390543290199918610303090142501490713145935617325806587528883833726972378426243439037")

(defun char-to-number (char)
  (- (char-code char) (char-code #\0)))

(defun number-to-char (number)
  (code-char (+ (char-code #\0) number)))

(defun string-to-number-list (string)
  (map 'list #'char-to-number string))

(defun newhash ()
  (make-hash-table :test #'equal))

(defun sethash (key value hash)
  (setf (gethash key hash) value))

(defun string-to-hash (string)
  (let ((hash (newhash)))
    (dotimes (n (length string))
      (let ((number (char-to-number (elt string n))))
        (if (/= 0 number) (sethash n number hash))))
    hash))

;; iteration 1-based index 0-based
(defun get-pattern-for-iteration-index (iteration index)
 (let* ((simple-pattern '(0 1 0 -1))
        (4-index (mod (floor (1+ index) iteration) 4)))
   (nth (mod 4-index 4) simple-pattern)))

(defun iterate (hash iteration)
  (let ((sum 0))
    (maphash #'(lambda (index value)
                 (let ((multiplier (get-pattern-for-iteration-index iteration index)))
                   (setf sum (+ sum (* value multiplier))))) hash)
    (abs (rem sum 10))))

(defun go-n-steps (steps signal)
  (let ((output (string-to-hash signal))
        (size (length signal)))
      (dotimes (k steps)
        (let ((hash output))
          (setf output (newhash))
          (dotimes (n size)
            (sethash n (iterate hash (1+ n)) output))))
      output))

(defun get-8-digit-answer (steps input)
 (let ((answer-hash (go-n-steps steps input)))
    (map 'string #'number-to-char (get-n 8 answer-hash))))

(defun get-n (times hash)
  (let ((list ()))
    (dotimes (n times)
      (push (gethash n hash) list))
    (reverse list)))

(defun pyramid-sum (list)
  (let* ((sum (reduce #'+ list))
         (output (list (mod sum 10))))
    (dotimes (n (1- (length list)))
      (setf sum (- sum (car list)))
      (setf output (push (mod sum 10) output))
      (setf list (cdr list)))
    output))

(defun get-offset (input)
  (parse-integer (subseq input 0 7)))

(defun make-offset-list (input multiplier)
  (let* ((offset (get-offset input))
         (original-size (length input))
         (multiplied-size (* original-size multiplier))
         (final-size (- multiplied-size offset))
         (remainder (mod final-size original-size))
         (full-sets (/ (- final-size remainder) original-size))
         (output (subseq input (- original-size remainder))))
    (dotimes (n full-sets)
      (setf output (concatenate 'string output input)))
    (string-to-number-list output)))

(defun get-n-digit-answer-part-2 (n input)
  (let ((list (make-offset-list input 10000)))
    (dotimes (k 100)
      (setf list (reverse (pyramid-sum list))))
    (map 'string #'number-to-char (subseq list 0 n))))

;; day 16 part 1
(time (assert (string= "40580215" (get-8-digit-answer 100 (input)))))

;; day 16 part 2
(time (assert (string= "22621597" (get-n-digit-answer-part-2 8 (input)))))
