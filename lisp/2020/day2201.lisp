(defun day-22-2020-1 ()
  (play (input) #'play-round))

(defun day-22-2020-2 ()
  (play (input) #'play-round-recursive))

(defun play (input playing-rules)
  (let ((player-1 (rest (rest (first input))))
        (player-2 (rest (rest (second input)))))
    (second (play-full-game (list player-1 player-2) playing-rules))))

(defun play-full-game (input rules)
  (destructuring-bind (player-1 player-2) input
    (loop for history = nil then (append (list (list p1 p2)) history)
          for (p1 p2) = (funcall rules (list player-1 player-2))
            then (funcall rules (list p1 p2))
          when (= 0 (length p1))
            return (list 'second (result p1 p2))
          when (or (= 0 (length p2)) (member (list p1 p2) history :test #'equal))
            return (list 'first (result p1 p2)))))

(defun result (p1 p2)
  (loop for i in (reverse (append p1 p2)) for k = 1 then (1+ k) sum (* i k)))

(defun play-round (input)
  (return-list (first-or-second input) input))

(defun first-or-second (input)
  (if (> (first (first input)) (first (second input))) 'first 'second))

(defun return-list (result input)
  (destructuring-bind (p1 p2) input
    (if (equal 'first result)
        (list (append (rest p1) (list (first p1) (first p2))) (rest p2))
        (list (rest p1) (append (rest p2) (list (first p2) (first p1)))))))

(defun play-round-recursive (input)
  (destructuring-bind (p1 p2) input
    (cond ((and (<= (first p1) (length (rest p1)))
                (<= (first p2) (length (rest p2))))
           (return-list
            (first (play-full-game (list (subseq p1 1 (1+ (first p1)))
                                         (subseq p2 1 (1+ (first p2))))
                                   #'play-round-recursive))
            input))
          (t (play-round input)))))

(defun input ()
  '(
    (Player 1
     18
     19
     16
     11
     47
     38
     6
     27
     9
     22
     15
     42
     3
     4
     21
     41
     14
     8
     23
     30
     40
     13
     35
     46
     50)
    (
     Player 2
     39
     1
     29
     20
     45
     43
     12
     2
     37
     33
     49
     32
     10
     26
     36
     17
     34
     44
     25
     28
     24
     5
     48
     31
     7)))

(defun example ()
  '((Player 1 9 2 6 3 1)
    (Player 2 5 8 4 7 10)))

(defun example-2 ()
  '((Player 1
     43
     19)
    (Player 2
     2
     29
     14)))
