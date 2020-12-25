(defun day-23-2020-1 ()
  (rest (iterate-it (input) 100)))

(defun day-23-2020-2 ()
  (let ((answer (iterate-it (part-2-input (input)) 10000000)))
    (* (second answer) (third answer))))

(defun iterate-it (input repeat)
  (let* ((max (length input))
         (hash (make-hash input))
         (answer (loop repeat repeat
                       for (i l) = (single-move-destructive input (last input) max hash)
                         then (single-move-destructive i l max hash)
                       finally (return (normalize i)))))
    answer))

(defun make-hash (input)
  (let ((hash (make-hash-table :test #'equal)))
    (loop for i on input
          do (setf (gethash (first i) hash) i)
          finally (return hash))))

(defun normalize (list)
  (when (equal (first list) 1) (return-from normalize list))
  (loop for i on list
        when (equal 1 (cadr i))
          return (let ((start (cdr i)))
                   (setf (cdr (last i)) list)
                   (setf (cdr i) nil)
                   start)))

(defun single-move-destructive (first last max hash)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((three-start (cdr first))
         (three-end (cdr (cdr three-start)))
         (first-con first)
         (after-three (cdr three-end)))
    (setf (cdr first-con) nil)
    (setf (cdr last) first-con)
    (setf (cdr three-end) nil)
    (let ((to-put-three (gethash (make-candidate (car first-con) max three-start) hash)))
      (setf (cdr three-end) (cdr to-put-three))
      (setf (cdr to-put-three) three-start))
    (list after-three first-con)))

(defun make-candidate (current max removed)
  (first (loop repeat 4 for i  = (1- current) then (1- i)
               when (and (not (member i removed)) (> i 0)) collect i
                 when (and (not (member (+ max i) removed)) (<= i 0)) collect (+ max i))))

(defun part-2-input (input) (part-2-input-max input 1000000))

(defun part-2-input-max (input max)
  (append input (loop repeat (- max (length input))
                      for i = (1+ (reduce #'max input)) then (1+ i)
                      collecting i)))

(defun input () (list 3 6 8 1 9 5 7 4 2))

(defun example () (list 3 8 9 1 2 5 4 6 7))
