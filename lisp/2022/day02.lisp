(defun count-score (input)
  (loop for (a x) in input summing (rules a x)))

(defun count-score-with-strategy (input)
  (loop for (a x) in input summing (rules a (strategy-selection a x))))

(defun rules (a x)
  (+ (selection-score x) (winning-score a x)))

(defun selection-score (x)
  (let ((scores '((X . 1) (Y . 2) (Z . 3))))
    (cdr (assoc x scores :test #'equal))))

(defun winning-score (a x)
  (let ((scores
          '(((A X) . 3) ((B Y) . 3) ((C Z) . 3)
            ((A Y) . 6) ((B Z) . 6) ((C X) . 6)
            ((A Z) . 0) ((B X) . 0) ((C Y) . 0))))
    (cdr (assoc (list a x) scores :test #'equal))))

(defun strategy-selection (a x)
  (let ((strategies
          '(((A X) . Z) ((B X) . X) ((C X) . Y)
            ((A Y) . X) ((B Y) . Y) ((C Y) . Z)
            ((A Z) . Y) ((B Z) . Z) ((C Z) . X))))
    (cdr (assoc (list a x) strategies :test #'equal))))

(defun day02-part1 ()
  (assert (equal 13005 (count-score (input)))))

(defun day02-part2 ()
  (assert (equal 11373 (count-score-with-strategy (input)))))

(defun input ()
  '(
    (B Z)
    (B Z)
    (B Y)
    (C Y)
    (B Y)
    (C Y)
    (C Y)
    (A Z)
    (B Y)
    (A Y)
    (C X)
    (A Y)
    (A Y)
    (C Y)
    (A Y)
    (B Y)
    (B Y)
    (C Y)
    (A Y)
    (A Y)
    (A Y)
    (C Y)
    (A Y)
    (B Y)
    (A Y)
    (B Y)
    (C Y)
    (B X)
    (A X)
    (A Y)
    (C X)
    (B Y)
    (C Y)
    (B Y)
    (B X)
    (C X)
    (B Y)
    (B Y)
    (A Y)
    (A X)
    (A X)
    (A Y)
    (B Y)
    (C Y)
    (C X)
    (A X)
    (B Y)
    (C Z)
    (B Y)
    (C X)
    (A Y)
    (A X)
    (C X)
    (A Y)
    (A Y)
    (A Y)
    (A Y)
    (B X)
    (C Y)
    (C Z)
    (B Y)
    (C Y)
    (A Y)
    (A Y)
    (B Y)
    (A Y)
    (C X)
    (B Y)
    (C Y)
    (C Y)
    (A Y)
    (C X)
    (B Y)
    (A Y)
    (B Y)
    (C Y)
    (C Y)
    (A Y)
    (B Y)
    (C Y)
    (C Y)
    (C Y)
    (A X)
    (A X)
    (B Y)
    (B Y)
    (C Z)
    (C Y)
    (B Y)
    (B Y)
    (B Y)
    (C Y)
    (A Y)
    (C Y)
    (A Y)
    (A Y)
    (A X)
    (C Y)
    (A X)
    (A Y)
    (A Y)
    (B Y)
    (B Y)
    (C Y)
    (C Y)
    (A Y)
    (A Y)
    (A X)
    (B Z)
    (C Y)
    (A Y)
    (A X)
    (C Y)
    (B Y)
    (C Y)
    (A Y)
    (A X)
    (A X)
    (A X)
    (A Y)
    (A X)
    (B Y)
    (A Y)
    (C Y)
    (A X)
    (B Y)
    (C Y)
    (B Y)
    (A X)
    (B Y)
    (B Y)
    (C Z)
    (C Y)
    (C X)
    (A Y)
    (C X)
    (B Y)
    (B Y)
    (C Y)
    (A X)
    (A X)
    (C Z)
    (B Z)
    (C Y)
    (C Y)
    (A X)
    (A X)
    (B Y)
    (B Z)
    (C Y)
    (B Y)
    (B Y)
    (B Y)
    (A X)
    (A X)
    (B Y)
    (C Y)
    (A Y)
    (C Y)
    (C Y)
    (A X)
    (B Y)
    (A Y)
    (A X)
    (C Y)
    (C X)
    (A Y)
    (A Y)
    (C Y)
    (C X)
    (C Y)
    (B Y)
    (A Y)
    (A Y)
    (C Y)
    (B Y)
    (C X)
    (A Y)
    (C X)
    (A X)
    (B Y)
    (C X)
    (A Y)
    (A Y)
    (A Y)
    (C Y)
    (C Y)
    (A Y)
    (A X)
    (C Y)
    (C Z)
    (B X)
    (C Z)
    (B Z)
    (A Z)
    (A Y)
    (B Y)
    (A Y)
    (B Y)
    (B Y)
    (A Y)
    (A Y)
    (C X)
    (C Y)
    (A Y)
    (A Y)
    (B Y)
    (A Y)
    (B Y)
    (B Z)
    (A Y)
    (B Y)
    (C Y)
    (A Y)
    (C Y)
    (B X)
    (A Y)
    (A X)
    (A Y)
    (A X)
    (A Y)
    (C Y)
    (A X)
    (C Y)
    (C Y)
    (A X)
    (B Y)
    (A Y)
    (B Y)
    (A Y)
    (A Z)
    (C Y)
    (B Z)
    (C X)
    (C Y)
    (C X)
    (B Y)
    (C X)
    (B Y)
    (A Y)
    (C Y)
    (A Y)
    (A X)
    (C Y)
    (A X)
    (A X)
    (C X)
    (C X)
    (C Y)
    (C X)
    (C Y)
    (A X)
    (B Z)
    (A Y)
    (B X)
    (B Y)
    (C Z)
    (B Y)
    (B Y)
    (B Y)
    (C X)
    (B Y)
    (A Y)
    (B Y)
    (A Y)
    (C Y)
    (C Y)
    (B Z)
    (B Y)
    (A X)
    (A Y)
    (C Y)
    (B Y)
    (C Y)
    (B Y)
    (B Y)
    (A X)
    (B Y)
    (B Z)
    (C Y)
    (A Y)
    (A Y)
    (A Y)
    (A Y)
    (A Y)
    (A Y)
    (C X)
    (B Y)
    (B Y)
    (C Y)
    (B Y)
    (B Y)
    (A Z)
    (C Y)
    (A Y)
    (A Y)
    (C Y)
    (C X)
    (A Y)
    (B Y)
    (A Y)
    (C X)
    (B Y)
    (A Y)
    (C Y)
    (B Y)
    (B Z)
    (A Y)
    (B Y)
    (A Y)
    (C Y)
    (A Y)
    (B Y)
    (C Y)
    (B Y)
    (A X)
    (C Z)
    (C Y)
    (A Y)
    (C Y)
    (B Y)
    (C Z)
    (A Y)
    (A Y)
    (C Y)
    (C X)
    (B Y)
    (C Y)
    (C X)
    (B Y)
    (C X)
    (A Y)
    (C X)
    (C Y)
    (B Y)
    (B Y)
    (A Y)
    (B Y)
    (A X)
    (C X)
    (C Y)
    (B Y)
    (A Y)
    (A X)
    (C Y)
    (C Y)
    (C Y)
    (C X)
    (B Z)
    (B Y)
    (C Y)
    (A X)
    (C Y)
    (A X)
    (C Y)
    (B Y)
    (B Y)
    (B Y)
    (B Y)
    (C X)
    (C Y)
    (C Y)
    (A Z)
    (B Y)
    (B Y)
    (A Y)
    (A Y)
    (B X)
    (A Y)
    (B Y)
    (B Y)
    (B Z)
    (C Y)
    (A X)
    (A X)
    (A Y)
    (B Z)
    (B Y)
    (C Y)
    (B Y)
    (B Y)
    (B Y)
    (B Y)
    (A X)
    (B Y)
    (C Y)
    (A Y)
    (B Y)
    (A X)
    (C Y)
    (C Y)
    (B Y)
    (B Y)
    (C Y)
    (B Y)
    (A Y)
    (A Y)
    (A X)
    (A Y)
    (B X)
    (B Y)
    (C Y)
    (C Y)
    (A Y)
    (A Y)
    (B Y)
    (B Y)
    (A X)
    (B Y)
    (A X)
    (C Y)
    (C Y)
    (C Y)
    (B Y)
    (A Y)
    (B Y)
    (A Y)
    (A Y)
    (A X)
    (C Y)
    (A Y)
    (A Y)
    (B Z)
    (C Y)
    (A X)
    (C X)
    (B Y)
    (C X)
    (C Y)
    (C X)
    (C X)
    (C Z)
    (A Y)
    (A X)
    (A Y)
    (B Z)
    (B Y)
    (A X)
    (A Y)
    (A X)
    (B Y)
    (A Y)
    (C X)
    (B Y)
    (B Y)
    (A Y)
    (A Y)
    (C Y)
    (A Y)
    (A Z)
    (C Y)
    (C Y)
    (C Z)
    (A Y)
    (C Y)
    (C Y)
    (C Y)
    (C Y)
    (A Y)
    (B Y)
    (B Y)
    (C Y)
    (A Y)
    (C Y)
    (C Y)
    (A X)
    (B X)
    (B Y)
    (A Y)
    (B Y)
    (A Y)
    (C Y)
    (A Y)
    (C Y)
    (B Y)
    (B Y)
    (B Y)
    (A Y)
    (C Y)
    (C X)
    (C X)
    (C X)
    (C X)
    (C Y)
    (C Y)
    (C X)
    (B Y)
    (A Y)
    (A X)
    (B Y)
    (C X)
    (B X)
    (C Y)
    (B Y)
    (C Y)
    (C X)
    (C Y)
    (B Y)
    (A Y)
    (A Y)
    (A Y)
    (C Y)
    (C Y)
    (A X)
    (A X)
    (C X)
    (A Y)
    (A Y)
    (B Y)
    (B X)
    (B Z)
    (C Z)
    (B Y)
    (B Y)
    (B Y)
    (B Y)
    (B Y)
    (A Y)
    (A Y)
    (B Y)
    (A Y)
    (C Z)
    (A Y)
    (C X)
    (A Y)
    (A Y)
    (C Y)
    (C X)
    (B Y)
    (A Y)
    (B Y)
    (B Y)
    (C Z)
    (A Y)
    (A Y)
    (A X)
    (C Y)
    (C Z)
    (B Y)
    (A X)
    (C Y)
    (C X)
    (B Y)
    (C Z)
    (A X)
    (A X)
    (A Y)
    (C Y)
    (A Z)
    (B Y)
    (B Y)
    (B Y)
    (B Y)
    (C Y)
    (B Y)
    (C X)
    (A Y)
    (C Y)
    (B Y)
    (B Y)
    (A Y)
    (A X)
    (A Y)
    (C Y)
    (C Y)
    (A X)
    (C Y)
    (B Y)
    (B Y)
    (C Y)
    (C Y)
    (A Y)
    (B Y)
    (A Y)
    (B Y)
    (A Y)
    (B Y)
    (C X)
    (A X)
    (C X)
    (B Y)
    (B Y)
    (B Y)
    (C Y)
    (B Y)
    (C Y)
    (B X)
    (A Y)
    (A Y)
    (C Y)
    (C X)
    (C X)
    (C Y)
    (C X)
    (A X)
    (A Y)
    (A Z)
    (A Y)
    (A Y)
    (B Y)
    (B Y)
    (C X)
    (C Y)
    (A Y)
    (B Z)
    (A Y)
    (A X)
    (A Y)
    (C Y)
    (A Y)
    (A X)
    (C Y)
    (B Y)
    (B Y)
    (C X)
    (A X)
    (A Z)
    (C Y)
    (B Y)
    (A X)
    (A Y)
    (C Y)
    (C Y)
    (A X)
    (C Y)
    (B Y)
    (A Y)
    (A X)
    (C Y)
    (B Y)
    (A Y)
    (A Y)
    (C X)
    (B Y)
    (B Y)
    (B Y)
    (C Z)
    (C Y)
    (C X)
    (C X)
    (B Y)
    (A X)
    (A Y)
    (B X)
    (A Y)
    (C Y)
    (B Y)
    (B Y)
    (B Y)
    (C Y)
    (C Z)
    (A Y)
    (C X)
    (C Y)
    (A Y)
    (A Y)
    (B Y)
    (B X)
    (B X)
    (B Y)
    (A Y)
    (A Y)
    (A Y)
    (C Y)
    (C Z)
    (B X)
    (C Y)
    (A Z)
    (A X)
    (B Y)
    (C Y)
    (B Y)
    (B X)
    (A Y)
    (A Y)
    (C Y)
    (B Y)
    (A X)
    (C X)
    (B Y)
    (C Y)
    (B Y)
    (B Y)
    (A X)
    (A Y)
    (A X)
    (C Y)
    (A Y)
    (C Y)
    (B Y)
    (A Y)
    (C Y)
    (A X)
    (A Y)
    (C Y)
    (C X)
    (C Y)
    (A X)
    (B Y)
    (B Y)
    (B Z)
    (A Y)
    (C Y)
    (B Y)
    (B Y)
    (C Y)
    (A X)
    (C X)
    (C Y)
    (C Y)
    (B Y)
    (C Y)
    (B Y)
    (A Y)
    (B Y)
    (A Y)
    (B Y)
    (A Y)
    (C Y)
    (B Y)
    (C X)
    (B Y)
    (C X)
    (A Y)
    (C Y)
    (A X)
    (C X)
    (B Y)
    (A Y)
    (B Y)
    (C X)
    (B Y)
    (C Y)
    (C Z)
    (A Y)
    (C Z)
    (C X)
    (C X)
    (A Y)
    (A X)
    (A Y)
    (B Z)
    (C Y)
    (C Y)
    (B Y)
    (A X)
    (A Y)
    (B Y)
    (C Y)
    (A Y)
    (A X)
    (B Y)
    (C Y)
    (A Z)
    (C Y)
    (B Y)
    (B Y)
    (B Y)
    (C Y)
    (A Y)
    (B Y)
    (A Y)
    (C X)
    (A X)
    (A Y)
    (B Y)
    (C Y)
    (B X)
    (C Y)
    (A X)
    (A Y)
    (A Y)
    (C Y)
    (A Y)
    (C Y)
    (A Y)
    (A X)
    (B Y)
    (C Y)
    (B Y)
    (C Y)
    (B Y)
    (B Y)
    (A Y)
    (A X)
    (A Y)
    (C Y)
    (A X)
    (C Y)
    (B Y)
    (B X)
    (C Y)
    (B Z)
    (C Y)
    (B X)
    (B Y)
    (C X)
    (C Y)
    (A X)
    (C Y)
    (C Y)
    (A X)
    (C Z)
    (C X)
    (B Y)
    (A X)
    (A X)
    (C Y)
    (B Y)
    (A Y)
    (A Y)
    (B Y)
    (B Y)
    (A X)
    (C Y)
    (B Y)
    (A X)
    (A Y)
    (B Y)
    (C Y)
    (A X)
    (A X)
    (A Y)
    (A X)
    (A X)
    (A Y)
    (A Y)
    (A Y)
    (A X)
    (C Y)
    (A X)
    (A Y)
    (A Y)
    (A Y)
    (B Y)
    (C Y)
    (C Y)
    (B Y)
    (A Y)
    (B Y)
    (B Z)
    (B Y)
    (B Y)
    (C Y)
    (B Y)
    (C X)
    (A Y)
    (C Y)
    (A Y)
    (C Y)
    (B Y)
    (C X)
    (B Y)
    (B Y)
    (C Z)
    (C Y)
    (B Z)
    (B X)
    (B Y)
    (A Z)
    (A Y)
    (B Y)
    (A Y)
    (C Y)
    (A X)
    (B Y)
    (C Y)
    (A Z)
    (A Y)
    (C Y)
    (C Y)
    (B Y)
    (C Y)
    (C X)
    (B Y)
    (C Y)
    (C X)
    (B X)
    (C X)
    (A X)
    (A X)
    (A Y)
    (C Y)
    (B Y)
    (C X)
    (B X)
    (A X)
    (B Y)
    (C Y)
    (A Y)
    (C X)
    (C Y)
    (B Y)
    (C Y)
    (B Y)
    (A Y)
    (B Y)
    (A Y)
    (A Y)
    (A Y)
    (A Y)
    (B X)
    (C X)
    (A Y)
    (B Y)
    (A X)
    (B Y)
    (C Y)
    (B Y)
    (A X)
    (A Y)
    (C Y)
    (A X)
    (C Z)
    (C Y)
    (A X)
    (B Y)
    (A Y)
    (B X)
    (A Y)
    (A Y)
    (A Y)
    (A Y)
    (A Y)
    (B Y)
    (B X)
    (A Y)
    (B Y)
    (C X)
    (C Y)
    (B Y)
    (B Y)
    (B Y)
    (B Y)
    (B X)
    (A Y)
    (A X)
    (B Y)
    (A X)
    (A X)
    (A Y)
    (B Y)
    (A Y)
    (A Y)
    (C Y)
    (B X)
    (B Y)
    (B Y)
    (A Y)
    (A X)
    (C Y)
    (A Y)
    (C Y)
    (A X)
    (B Y)
    (B Y)
    (A X)
    (A X)
    (A X)
    (A Y)
    (A X)
    (C Y)
    (C Z)
    (B Y)
    (B Y)
    (A X)
    (C Y)
    (B X)
    (A Y)
    (B Y)
    (A Y)
    (B Z)
    (B Y)
    (B Y)
    (A Z)
    (B Y)
    (A Y)
    (B Y)
    (B Y)
    (C X)
    (B Y)
    (A X)
    (B Y)
    (B Y)
    (C Y)
    (B Y)
    (A X)
    (C Y)
    (C Y)
    (C X)
    (C Y)
    (B Y)
    (B Z)
    (A Y)
    (B Y)
    (C X)
    (A Y)
    (B Y)
    (B Y)
    (A X)
    (B Y)
    (A Z)
    (B Y)
    (A Y)
    (C X)
    (B Y)
    (A Y)
    (B X)
    (A X)
    (C X)
    (C Y)
    (B Y)
    (A X)
    (B Y)
    (B Y)
    (C Y)
    (B Y)
    (C Y)
    (A Y)
    (C X)
    (C Y)
    (A Y)
    (C Y)
    (A Y)
    (C Z)
    (A Y)
    (B Y)
    (A Y)
    (C Z)
    (A Y)
    (C Y)
    (A Y)
    (C X)
    (C Y)
    (C Y)
    (A Y)
    (B Y)
    (B Y)
    (B Y)
    (A Y)
    (A Y)
    (B Y)
    (B Y)
    (B Y)
    (B Y)
    (B Y)
    (C Z)
    (A X)
    (A Y)
    (B Y)
    (B Y)
    (A Y)
    (B Y)
    (B Z)
    (B X)
    (A Y)
    (A Y)
    (A Y)
    (C X)
    (B Y)
    (A Y)
    (C Y)
    (A Y)
    (A Z)
    (B Y)
    (B Y)
    (A X)
    (B Y)
    (B Y)
    (A Y)
    (C X)
    (A X)
    (A Y)
    (A Y)
    (C Y)
    (B X)
    (A Y)
    (C Y)
    (C X)
    (A Z)
    (B Y)
    (B Y)
    (A X)
    (A Y)
    (C X)
    (B Y)
    (C Y)
    (A Y)
    (C Y)
    (B Y)
    (A Y)
    (A Y)
    (B Y)
    (B Y)
    (B X)
    (C Y)
    (A X)
    (A Y)
    (C X)
    (A Y)
    (C Y)
    (A Y)
    (C X)
    (C Y)
    (B Y)
    (A Y)
    (A Y)
    (A Y)
    (B Y)
    (A Y)
    (A X)
    (B Y)
    (B Y)
    (B Y)
    (A Y)
    (B Y)
    (B Y)
    (B Y)
    (C X)
    (B Y)
    (A Y)
    (C X)
    (B Y)
    (B Y)
    (A Y)
    (C Y)
    (B Y)
    (A Y)
    (B Y)
    (C Y)
    (B Y)
    (B Y)
    (C Y)
    (C X)
    (C X)
    (C Y)
    (B Y)
    (B Y)
    (A Y)
    (A Y)
    (A Z)
    (B Y)
    (C X)
    (B Y)
    (B Y)
    (C Y)
    (C Y)
    (C Y)
    (A Y)
    (A X)
    (A Y)
    (A Y)
    (C X)
    (C X)
    (B Y)
    (C Z)
    (C X)
    (A Z)
    (B Y)
    (A Y)
    (B Y)
    (C X)
    (A Y)
    (B Z)
    (C Y)
    (A Y)
    (B Y)
    (C Y)
    (C Y)
    (C Y)
    (A Y)
    (B Y)
    (C Y)
    (A X)
    (B Y)
    (A Y)
    (C Y)
    (A Y)
    (C X)
    (B Z)
    (B X)
    (C Y)
    (C Y)
    (C Y)
    (B Y)
    (C X)
    (A Y)
    (A Y)
    (B Y)
    (C Y)
    (A Y)
    (A Y)
    (B Y)
    (B Y)
    (A X)
    (A Y)
    (A Y)
    (B Z)
    (A X)
    (A X)
    (C Y)
    (C Y)
    (C X)
    (B X)
    (A Z)
    (B Y)
    (A X)
    (A Y)
    (B Y)
    (A Y)
    (B X)
    (A Y)
    (A Z)
    (B Y)
    (A X)
    (B Y)
    (A Y)
    (A X)
    (A Y)
    (A Y)
    (B Y)
    (A Y)
    (A X)
    (A Y)
    (C Y)
    (B Y)
    (B Y)
    (A Y)
    (A Y)
    (A X)
    (C Y)
    (A Y)
    (B Z)
    (C Y)
    (B Y)
    (A Y)
    (C Z)
    (A Y)
    (B Y)
    (A Y)
    (C Y)
    (B Z)
    (B Y)
    (B Y)
    (C Y)
    (B Y)
    (C Y)
    (C Z)
    (B Y)
    (B Y)
    (A X)
    (B Y)
    (C Y)
    (A Y)
    (B Y)
    (A X)
    (B Y)
    (C Y)
    (C Y)
    (A Y)
    (C X)
    (C X)
    (B Y)
    (C Y)
    (B Y)
    (A Y)
    (A X)
    (B Z)
    (B Y)
    (C Y)
    (A Y)
    (C X)
    (B Y)
    (C Z)
    (B Y)
    (A Y)
    (B Y)
    (A X)
    (A X)
    (A Y)
    (B Y)
    (A Y)
    (A X)
    (C Y)
    (C Y)
    (C X)
    (A X)
    (A X)
    (A Z)
    (C Y)
    (A Y)
    (A X)
    (A Y)
    (A Y)
    (B Y)
    (B Y)
    (C X)
    (B Y)
    (C Y)
    (A Y)
    (A Y)
    (A Y)
    (A X)
    (C X)
    (A Y)
    (C X)
    (A Y)
    (B Y)
    (A Y)
    (B Y)
    (A X)
    (C Y)
    (A X)
    (B Y)
    (A X)
    (B X)
    (A X)
    (B X)
    (C Y)
    (B X)
    (B Y)
    (B Y)
    (A X)
    (A Z)
    (C Z)
    (B Y)
    (C X)
    (A Y)
    (C Y)
    (A Y)
    (B Y)
    (C X)
    (B Y)
    (C Y)
    (C Y)
    (B Y)
    (A X)
    (B X)
    (C Z)
    (C X)
    (A X)
    (C Y)
    (B Y)
    (A X)
    (C Y)
    (C Y)
    (A Y)
    (B Y)
    (C Y)
    (C Y)
    (B Y)
    (B Z)
    (A Y)
    (A X)
    (C X)
    (B Y)
    (B Y)
    (C X)
    (A Y)
    (A Y)
    (B Y)
    (A X)
    (B Y)
    (B Y)
    (C X)
    (A Y)
    (B Y)
    (B Z)
    (C Y)
    (A X)
    (B Y)
    (A Y)
    (A Y)
    (B Y)
    (C Y)
    (A Y)
    (A Y)
    (A Y)
    (B Y)
    (A Y)
    (B Y)
    (B Y)
    (B X)
    (A Z)
    (B Z)
    (A Z)
    (A Y)
    (B Y)
    (C Y)
    (B Y)
    (B Y)
    (B Y)
    (B Y)
    (A Y)
    (A Y)
    (B Y)
    (B Y)
    (A Y)
    (C Y)
    (A Y)
    (C X)
    (A Y)
    (C X)
    (B Y)
    (B X)
    (C Y)
    (C X)
    (B Y)
    (B Y)
    (B Y)
    (B Y)
    (C Y)
    (A Z)
    (A Y)
    (B Z)
    (C Y)
    (A Y)
    (A Y)
    (B Y)
    (C X)
    (A X)
    (A Y)
    (C Y)
    (A Y)
    (C Y)
    (A X)
    (A X)
    (C Y)
    (B Z)
    (A X)
    (A Y)
    (B Y)
    (B Y)
    (A Y)
    (A Y)
    (A Y)
    (B Y)
    (B X)
    (A Y)
    (C X)
    (B Y)
    (C X)
    (C Y)
    (C Z)
    (C Y)
    (A X)
    (C Y)
    (A Y)
    (C Y)
    (C X)
    (A Y)
    (A Z)
    (A X)
    (A X)
    (A X)
    (B Y)
    (A Y)
    (C Y)
    (C Y)
    (C X)
    (A Y)
    (B Y)
    (A Y)
    (A X)
    (C X)
    (A Y)
    (B Y)
    (A Y)
    (C Y)
    (C Y)
    (C Y)
    (C Y)
    (C X)
    (B Y)
    (C X)
    (B X)
    (A X)
    (C Y)
    (B X)
    (C Z)
    (B Z)
    (B Y)
    (A Y)
    (B Y)
    (B X)
    (C X)
    (C X)
    (B Y)
    (C Y)
    (C Y)
    (A X)
    (A X)
    (C Y)
    (A X)
    (A Y)
    (A Y)
    (A Y)
    (C Y)
    (A Z)
    (C X)
    (C X)
    (B Y)
    (B Y)
    (A Z)
    (B Y)
    (A Y)
    (A Y)
    (A Y)
    (C Y)
    (B Y)
    (C Y)
    (B Y)
    (B Y)
    (A Y)
    (B Y)
    (A Y)
    (B Y)
    (C Y)
    (C X)
    (A Y)
    (A Y)
    (C Y)
    (A X)
    (C Y)
    (B Y)
    (A Y)
    (C X)
    (C X)
    (B Y)
    (A Y)
    (A Y)
    (C Y)
    (C Z)
    (A X)
    (C Z)
    (B Y)
    (B Y)
    (B Y)
    (C X)
    (A Y)
    (B Y)
    (C Y)
    (B Y)
    (C Y)
    (B Y)
    (B Y)
    (A Y)
    (B Y)
    (A Z)
    (A Y)
    (A X)
    (A Y)
    (A X)
    (A Y)
    (A X)
    (A Y)
    (A Y)
    (C Y)
    (A Y)
    (A Y)
    (B Z)
    (A Y)
    (A Y)
    (C Y)
    (B X)
    (C Y)
    (A Y)
    (C X)
    (C Y)
    (A Y)
    (C Y)
    (B Y)
    (A X)
    (A Y)
    (A X)
    (A Y)
    (B Y)
    (C X)
    (C Y)
    (C X)
    (C X)
    (A Y)
    (C Y)
    (A Y)
    (B Y)
    (C X)
    (C X)
    (A Y)
    (C Y)
    (B X)
    (C X)
    (B Y)
    (C X)
    (A Y)
    (C Y)
    (A X)
    (B X)
    (C Y)
    (C Y)
    (C Y)
    (B Y)
    (A Y)
    (A X)
    (A Y)
    (A Y)
    (B Y)
    (B Y)
    (A Y)
    (C Y)
    (C Y)
    (B Y)
    (B Y)
    (B Y)
    (B Y)
    (B Y)
    (B Z)
    (A Y)
    (A X)
    (C Y)
    (B Y)
    (C Y)
    (C X)
    (B Y)
    (A Y)
    (B Y)
    (C Y)
    (B Z)
    (A X)
    (B X)
    (B Y)
    (A X)
    (B Z)
    (A X)
    (A X)
    (B Y)
    (B X)
    (C X)
    (A Y)
    (A Y)
    (B Y)
    (B Z)
    (A Y)
    (B X)
    (C Y)
    (C Y)
    (B Y)
    (A Y)
    (A Y)
    (B Y)
    (A Y)
    (B Z)
    (C Y)
    (B Y)
    (A X)
    (C X)
    (A Y)
    (B Y)
    (B Y)
    (A Y)
    (C X)
    (C Y)
    (C Z)
    (B Y)
    (B Y)
    (A Y)
    (C Y)
    (A X)
    (C X)
    (A X)
    (C X)
    (C Y)
    (B Y)
    (A Z)
    (A Y)
    (A X)
    (C Y)
    (A X)
    (C X)
    (A Y)
    (B Y)
    (C Z)
    (B Y)
    (C Z)
    (B Y)
    (B Y)
    (A Y)
    (A Z)
    (B Z)
    (A Y)
    (A Y)
    (B Y)
    (A Y)
    (A Z)
    (B Y)
    (B Z)
    (B Y)
    (B Z)
    (B Y)
    (A Y)
    (A Y)
    (B Y)
    (B Z)
    (B Y)
    (A Y)
    (B Y)
    (C X)
    (B Y)
    (C X)
    (A Y)
    (B Y)
    (C Y)
    (A Y)
    (A Y)
    (B Y)
    (B Z)
    (A Y)
    (A Y)
    (B Y)
    (C X)
    (C Y)
    (C Y)
    (A Y)
    (B Y)
    (A Y)
    (A X)
    (B Z)
    (A Y)
    (A X)
    (A X)
    (A Z)
    (A Z)
    (A Y)
    (C Y)
    (A Z)
    (A Y)
    (C Y)
    (A Y)
    (B Z)
    (C X)
    (C Y)
    (C Y)
    (B Y)
    (C X)
    (C X)
    (A Y)
    (C Y)
    (B X)
    (C Y)
    (B Y)
    (B Y)
    (B Y)
    (B X)
    (C Y)
    (C Y)
    (A X)
    (A Y)
    (B Z)
    (A Y)
    (B Y)
    (A Y)
    (B Y)
    (B Y)
    (A Y)
    (B Y)
    (B Z)
    (A X)
    (C Y)
    (A Y)
    (A X)
    (A Y)
    (B Y)
    (A Y)
    (B Y)
    (B Y)
    (C Y)
    (B Y)
    (B Y)
    (C Y)
    (B Y)
    (C X)
    (C Y)
    (A X)
    (B X)
    (C Y)
    (B Y)
    (C X)
    (A Y)
    (A Y)
    (B Y)
    (A Y)
    (B Y)
    (A Y)
    (B Z)
    (C Y)
    (B Y)
    (B Y)
    (C Y)
    (A Y)
    (A X)
    (B X)
    (A Y)
    (B Y)
    (B Y)
    (C Y)
    (A Y)
    (C Y)
    (A Y)
    (A Y)
    (B X)
    (C X)
    (A Y)
    (B Y)
    (B Y)
    (B Y)
    (B Y)
    (A Y)
    (C Y)
    (A X)
    (B Y)
    (A Y)
    (C Y)
    (A X)
    (A Y)
    (A Y)
    (C Y)
    (A Y)
    (B Y)
    (A X)
    (A Y)
    (B Y)
    (A X)
    (C X)
    (C Y)
    (A Y)
    (A Y)
    (B Y)
    (C Y)
    (C Y)
    (A X)
    (C Y)
    (C Y)
    (B Y)
    (A Y)
    (C X)
    (B Y)
    (A Y)
    (A Y)
    (A Y)
    (A Y)
    (B Y)
    (A Y)
    (B Z)
    (A Y)
    (B Z)
    (A X)
    (A Y)
    (A Y)
    (A X)
    (C Y)
    (A X)
    (A X)
    (C Y)
    (A X)
    (A X)
    (A Y)
    (C Y)
    (A Y)
    (B Y)
    (A X)
    (C Z)
    (A Y)
    (B X)
    (A Y)
    (B Y)
    (C Y)
    (A Y)
    (B Y)
    (A Y)
    (C Y)
    (B X)
    (C Y)
    (B Y)
    (A Y)
    (B Y)
    (C Y)
    (C Y)
    (A Y)
    (A Y)
    (C X)
    (A X)
    (A Y)
    (B Y)
    (C Y)
    (C Y)
    (B Z)
    (C Y)
    (C Y)
    (B Y)
    (B Z)
    (A Y)
    (A Y)
    (B Y)
    (B Y)
    (B Y)
    (A X)
    (B Y)
    (C Y)
    (B Y)
    (A Y)
    (C Y)
    (C Y)
    (C X)
    (B Y)
    (A Y)
    (B Y)
    (C Y)
    (B Y)
    (C Y)
    (A Y)
    (A Y)
    (C Z)
    (C X)
    (B Y)
    (A Y)
    (A X)
    (B Y)
    (C Y)
    (A Y)
    (C Y)
    (B Y)
    (C Y)
    (C X)
    (A X)
    (C Y)
    (A Y)
    (A X)
    (A X)
    (A X)
    (C Y)
    (C Y)
    (A X)
    (C Y)
    (C Y)
    (C Z)
    (A X)
    (C Y)
    (C X)
    (B Z)
    (C Y)
    (A Y)
    (A X)
    (C X)
    (A Y)
    (C Z)
    (A Y)
    (C X)
    (B Y)
    (A Y)
    (C X)
    (A X)
    (B Y)
    (B Y)
    (A X)
    (C Y)
    (B Y)
    (C X)
    (C X)
    (B Y)
    (A Y)
    (C X)
    (C Z)
    (C X)
    (C X)
    (A Y)
    (C Y)
    (C Y)
    (C Y)
    (A X)
    (B Y)
    (C Y)
    (A X)
    (C X)
    (C Y)
    (B X)
    (B Y)
    (C Y)
    (A Y)
    (A Y)
    (B Y)
    (B Y)
    (B Y)
    (B Y)
    (B Y)
    (B Y)
    (A Y)
    (A X)
    (B Y)
    (B Y)
    (B Y)
    (B Z)
    (B X)
    (C Y)
    (A X)
    (A Y)
    (A Y)
    (C Y)
    (A Y)
    (A X)
    (C X)
    (A Y)
    (A Y)
    (C X)
    (A Y)
    (B Y)
    (A Y)
    (B Y)
    (C Y)
    (C X)
    (A X)
    (C Y)
    (C Y)
    (B Z)
    (A Y)
    (C Z)
    (A X)
    (A X)
    (B Y)
    (A X)
    (A Y)
    (B Y)
    (C Y)
    (B Y)
    (C Y)
    (A Y)
    (A X)
    (C Y)
    (C Y)
    (C Y)
    (A X)
    (C Y)
    (A Y)
    (A Z)
    (A X)
    (C Z)
    (C Y)
    (C X)
    (B Y)
    (C Y)
    (B Y)
    (C Y)
    (A Y)
    (C X)
    (B Y)
    (C Y)
    (C X)
    (A Y)
    (B Y)
    (C X)
    (C Y)
    (C Y)
    (A Y)
    (A Y)
    (B Y)
    (A X)
    (B Y)
    (A Y)
    (C Y)
    (B X)
    (C Y)
    (B Y)
    (B Y)
    (B Y)
    (B Y)
    (A X)
    (C Y)
    (A X)
    (A Y)
    (A Y)
    (B Z)
    (C X)
    (B Y)
    (B Y)
    (A X)
    (C Y)
    (A Y)
    (A Y)
    (B Y)
    (A X)
    (B Z)
    (B Y)
    (B Y)
    (A Y)
    (A X)
    (A X)
    (A Y)
    (A Y)
    (C X)
    (C X)
    (A X)
    (B Y)
    (C X)
    (C Z)
    (C Y)
    (B Y)
    (C Z)
    (A Y)
    (A Y)
    (B Y)
    (B Y)
    (C X)
    (A Z)
    (A X)
    (A Y)
    (B Y)
    (B Y)
    (A Y)
    (A X)
    (A X)
    (A Y)
    (A Y)
    (C Y)
    (B Y)
    (B Y)
    (A X)
    (A X)
    (C Y)
    (C X)
    (C Z)
    (A Y)
    (A Y)
    (A Y)
    (C Y)
    (C X)
    (B Y)
    (C Y)
    (A X)
    (B Y)
    (C Y)
    (A Y)
    (A X)
    (B Y)
    (C Y)
    (A Y)
    (B Y)
    (C X)
    (A Y)
    (A Y)
    (B Y)
    (B Y)
    (C Y)
    (A Y)
    (A Y)
    (A X)
    (B Z)
    (A X)
    (A Y)
    (C Y)
    (C Y)
    (A Y)
    (A Y)
    (C Y)
    (C Y)
    (B Y)
    (A Y)
    (A X)
    (A Y)
    (C X)
    (C X)
    (B Y)
    (B Y)
    (A Y)
    (A X)
    (C X)
    (A Y)
    (B Y)
    (B Y)
    (A Y)
    (C Y)
    (B Y)
    (C X)
    (C Y)
    (A Y)
    (A Y)
    (C Y)
    (B Y)
    (B X)
    (C Y)
    (C Y)
    (B Y)
    (B X)
    (A Y)
    (B X)
    (A X)
    (B Y)
    (C X)
    (A X)
    (B Y)
    (A Y)
    (A Y)
    (A Y)
    (C Y)
    (B X)
    (A Y)
    (B Y)
    (A Y)
    (A X)
    (C Z)
    (B Y)
    (A Y)
    (B Y)
    (B Y)
    (A Y)
    (A Y)
    (B Y)
    (C Y)
    (A Y)
    (C Y)
    (B Y)
    (C Y)
    (A X)
    (A Y)
    (C Y)
    (A Y)
    (C Y)
    (B Y)
    (B Y)
    (C Y)
    (A Y)
    (A Y)
    (B Y)
    (A Y)
    (C Y)
    (A Y)
    (A Y)
    (B Y)
    (B Y)
    (B Y)
    (B Y)
    (A X)
    (A X)
    (C Y)
    (C Y)
    (B X)
    (C Z)
    (C Y)
    (A X)
    (A Y)
    (B Y)
    (A Y)
    (C Y)
    (C X)
    (B Y)
    (B Z)
    (C Y)
    (A X)
    (A Y)
    (A X)
    (B Y)
    (B X)
    (B Y)
    (B X)
    (C Y)
    (A Z)
    (B Y)
    (C Y)
    (A Y)
    (A X)
    (C Y)
    (C X)
    (B Y)
    (A Y)
    (A Y)
    (A X)
    (C X)
    (A Y)
    (C X)
    (A Y)
    (C Y)
    (C X)
    (A X)
    (C Z)
    (B Y)
    (A X)
    (B Y)
    (A Y)
    (A X)
    (A X)
    (A Y)
    (A Y)
    (A Y)
    (C X)
    (C X)
    (A Y)
    (A Y)
    (B Y)
    (C Y)
    (C Y)
    (B Y)
    (C Y)
    (A Y)
    (C Y)
    (A Y)
    (B Y)
    (B Y)
    (A Y)
    (B Y)
    (C X)
    (B X)
    (A X)
    (A X)
    (B Y)
    (B Y)
    (B Y)
    (C Z)
    (C Y)
    (B X)
    (A X)
    (B Y)
    (A X)
    (B Y)
    (A Y)
    (B Y)
    (C Y)
    (A Y)
    (A Y)
    (C Y)
    (B Y)
    (A Y)
    (A Y)
    (C Y)
    (C Y)
    (C Y)
    (B Y)
    (A Y)
    (C Y)
    (C Y)
    (B X)
    (B Y)
    (B Y)
    (A Y)
    (B X)
    (A Y)
    (B Y)
    (C Z)
    (B Z)
    (B Y)
    (C Y)
    (B Z)
    (C Y)
    (C Y)
    (B Z)
    (A Y)
    (A Y)
    (A Y)
    (C Z)
    (B Y)
    (A Y)
    (A Y)
    (C Z)
    (A Y)
    (A Y)
    (A Y)
    (B Y)
    (C X)
    (A Y)
    (A Y)
    (A Y)
    (A Y)
    (A Z)
    (C Y)
    (A X)
    (A Y)
    (A Y)
    (A Y)
    (B Y)
    (B Y)
    (A X)
    (B Y)
    (A Y)
    (A Z)
    (A Y)
    (A Y)
    (A Y)
    (A Y)
    (A Y)
    (C Y)
    (B Y)
    (C X)
    (B Y)
    (C Y)
    (C X)
    (B Y)
    (A Y)
    (B Y)
    (A Y)
    (B Y)
    (B Y)
    (A Y)
    (A X)
    (A Y)
    (B Y)
    (B Y)
    (B Y)
    (A X)
    (C Y)
    (A Y)
    (B Y)
    (B X)
    (A Y)
    (C X)
    (B Y)
    (A X)
    (A Y)
    (A Y)
    (C Z)
    (C Y)
    (B Y)
    (C Y)
    (B Z)
    (A X)
    (B Y)
    (C Y)
    (B Y)
    (A Y)
    (A Y)
    (C Y)
    (A Y)
    (C Y)
    (A Y)
    (C Y)
    (A X)
    (A X)
    (A Y)
    (B Y)
    (B Y)
    (B X)
    (A X)
    (A Y)
    (A Y)
    (A X)
    (A Y)
    (C Y)
    (C Y)
    (B Y)
    (A X)
    (C Y)
    (A Y)
    (C X)
    (B Y)
    (A Y)
    (C Y)
    (C X)
    (A Y)
    (C Y)
    (B Y)
    (B Y)
    (A X)
    (A Y)
    (C Y)
    (A X)
    (A X)
    (B Y)
    (B Y)
    (A Y)
    (A X)
    (A Y)
    (B Y)
    (A X)
    (B Y)
    (A X)
    (A X)
    (C Z)
    (C Z)
    (B Y)
    (A X)
    (B Y)
    (A Y)
    (B Y)
    (B Z)
    (A Y)
    (A X)
    (C Y)
    (C Y)
    (C Z)
    (C Y)
    (C Z)
    (A X)
    (A Y)
    (C Y)
    (C Y)
    (C Y)
    (A Y)
    (B Y)
    (A Y)
    (A Y)
    (B Y)
    (C Y)
    (B Z)
    (A Y)
    (C Y)))