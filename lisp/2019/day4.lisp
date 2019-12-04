(defun input () (vector 152085 670283))

(defun count-matches (fun)
  (let ((sum 0) (start (elt (input) 0)) (end (elt (input) 1)))
    (dotimes (n (+ 1 (- end start)))
      (if (funcall fun (+ start n))
            (incf sum)))
    sum))

(defun matches (n)
  (let ((num-str (write-to-string n)))
    (and (six-digit num-str) (increasing num-str) (two-the-same num-str))))

(defun six-digit (n)
  (= 6 (length n)))

(defun two-the-same (n)
  (two-the-same3 n 1 nil))

(defun two-the-same3 (n next verdict)
  (cond
    ((>= next (length n)) verdict)
    ((char= (elt n (1- next)) (elt n next))  t)
    (t (two-the-same3 n (1+ next) verdict))))

(defun increasing (n)
  (increasing3 n 1 t))

(defun increasing3 (n next verdict)
  (cond
    ((>= next (length n)) verdict)
    ((char> (elt n (1- next)) (elt n next)) nil)
    (t (increasing3 n (1+ next) verdict))))

(defun exact-two-the-same (n)
  (exact-two-the-same2 n 0))

(defun exact-two-the-same2 (n current)
  (cond
    ((> current 3) nil)
    ((and
      (= current 0)
      (char= (elt n current) (elt n (1+ current)))
      (char/= (elt n (1+ current)) (elt n (+ current 2))))
     t)
    ((and
      (= current 3)
      (char/= (elt n current) (elt n (1+ current)))
      (char= (elt n (1+ current)) (elt n (+ current 2))))
     t)
    ((and
      (< current 3)
      (char/= (elt n current) (elt n (1+ current)))
      (char/= (elt n (+ current 2)) (elt n (+ current 3)))
      (char= (elt n (1+ current)) (elt n (+ current 2))))
      t)
    (t (exact-two-the-same2 n (1+ current)))))

(defun matches-exact-two (n)
  (let ((num-str (write-to-string n)))
    (and (six-digit num-str) (increasing num-str) (exact-two-the-same num-str))))

;; part 1
(assert (= 1764 (count-matches #'matches)))

;; part 2
(assert (= 1196 (count-matches #'matches-exact-two)))
