(defun day-13-2020-1 ()
  (let* ((distances
           (mapcar #'(lambda (x) (list x (- (parse-integer x) (rem (first (input)) (parse-integer x)))))
                   (remove "" (uiop:split-string (remove #\x (second (input))) :separator ",") :test #'equal)))
         (found (rassoc (reduce #'min distances :key #'second) distances :test #'equal :key #'car)))
    (* (parse-integer (first found)) (second found))))

(defun day-13-2020-2 ()
  (find-it (normalize (second (input)))))

(defun find-it (input)
  (loop for (offset time) in input with timeline = 0 with multiplier = 1
        do (loop when (not (equal (mod timeline time) (- time (mod offset time))))
                   do (incf timeline multiplier)
                 else return 'done)
        (setf multiplier (* multiplier time))
        finally (return (1+ timeline))))

(defun normalize (input)
  (loop for i in (uiop:split-string input :separator ",") for counter = 1 then (1+ counter)
        when (not (equal i "x"))
          collect (list counter (parse-integer i)) into output
        finally (return (sort output #'> :key #'second))))

(defun input ()
  '(1001796
   "37,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,457,x,x,x,x,x,x,x,x,x,x,x,x,13,17,x,x,x,x,x,x,x,x,23,x,x,x,x,x,29,x,431,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,19"))

(defun example ()
  '(939
   "7,13,x,x,59,x,31,19"))
