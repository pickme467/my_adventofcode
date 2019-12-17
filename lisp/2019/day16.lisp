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

(defun geth (n hash)
  (let ((x (gethash n hash)))
    (if (null x) 0 x)))

(defun seth (k v hash)
  (if (= 0 v) nil (sethash k v hash)))

(defun string-to-hash (string)
  (let ((hash (newhash)))
    (dotimes (n (length string))
      (let ((number (char-to-number (elt string n))))
        (if (/= 0 number) (seth n number hash))))
    hash))

;; iteration 1-based index 0-based
(defun get-pattern-for-iteration-index (iteration index)
 (let* ((simple-pattern '(0 1 0 -1))
        (4-index (mod (floor (1+ index) iteration) 4)))
   (nth (mod 4-index 4) simple-pattern)))

(defun iterate (number hash iteration)
  (let ((sum 0))
    (dotimes (n number)
      (let ((multiplier (get-pattern-for-iteration-index iteration n)))
        (if (/= 0 multiplier)
            (let ((element (geth n hash)))
              (setf sum (+ sum (* element multiplier)))))))
    (abs (rem sum 10))))

(defun go-n-steps (steps signal size)
  (let ((output (string-to-hash signal)))
      (dotimes (k steps)
        (let ((hash output))
          (setf output (newhash))
          (dotimes (n size)
            (seth n (iterate size hash (1+ n)) output))))
      output))

(defun get-8-digit-answer (steps input size)
 (let ((answer-hash (go-n-steps steps input size)))
    (map 'string #'number-to-char (get-n 8 answer-hash))))

(defun get-n (times hash)
  (let ((list ()))
    (dotimes (n times)
      (push (geth n hash) list))
    (reverse list)))

;; day 16 part 1
(time (assert (string= "40580215" (get-8-digit-answer 100 (input) (length (input))))))

(time (assert (string= "12" (get-8-digit-answer 100 (input) (* 10000 (length (input)))))))
