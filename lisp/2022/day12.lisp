(defun new-hash ()
  (make-hash-table :test #'equal))

(defun set-hash (k v hash)
  (setf (gethash k hash) v)
  hash)

(defun delete-hash (k hash)
    (remhash k hash)
    hash)

(defun make-hash (input)
  (loop with hash = (new-hash)
        with start = '(0 0)
        with end = '(0 0)
        for i in input
        for y = 0 then (1+ y)
        do (loop for c across i
                 for x = 0 then (1+ x)
                 do (setf (gethash (list x y) hash) c)
                 when (equal #\S c)
                   do (setf start (list x y))
                      (setf (gethash (list x y) hash) #\a)
                 when (equal #\E c)
                   do (setf end (list x y))
                      (setf (gethash (list x y) hash) #\z))
        finally (return (list hash start end))))

(defun make-graph (hash)
  (loop with graph = (new-hash)
        for k being the hash-keys in hash using (hash-value v)
        do (setf (gethash k graph)
                 (loop for k-neighbour in (get-neighbours k)
                       when (can-move v (gethash k-neighbour hash))
                         collect k-neighbour))
        finally (return graph)))

(defun can-move (current next)
  (cond ((null next) nil)
        (t (<= (- (char-code next) (char-code current)) 1))))

(defun get-neighbours (pos)
  (let ((x (car pos))
        (y (cadr pos)))
    (list (list (1- x) y) (list (1+ x) y)
          (list x (1- y)) (list x (1+ y)))))

(defun dijkstra-step (evaluated removed map final)
  (let* ((smallest (find-smallest evaluated))
         (removed (set-hash (car smallest) (cadr smallest) removed))
         (evaluated (delete-hash (car smallest) evaluated))
         (next (next-coords (car smallest) removed map)))
    (cond
      ((equal final (car smallest)) (cadr smallest))
      ((null (car smallest)) 999999)
      (t
       (dijkstra-step (update-evaluated next (cadr smallest) evaluated)
                      removed map final)))))

(defun find-smallest (hash)
  (loop for v being the hash-value in hash using (hash-key k)
        for min-pos = k then (if (< v (gethash min-pos hash)) k min-pos)
        finally (return (list min-pos (gethash min-pos hash)))))

(defun next-coords (pos removed map)
  (loop for p in (gethash pos map) when (null (gethash p removed)) collect p))

(defun update-evaluated (list weight evaluated)
  (loop for pos in list
        for new-weight = (1+ weight)
        for next-weight = (gethash pos evaluated)
        when (or (null next-weight) (<= new-weight next-weight))
          do (set-hash pos new-weight evaluated)
        else
          do (set-hash pos next-weight evaluated)
        finally (return evaluated)))

(defun find-path-for (hash start end)
  (let ((graph (make-graph hash)))
    (dijkstra-step (set-hash start 0 (new-hash)) (new-hash) graph end)))

(defun find-all-starts (input)
  (loop for v being the hash-values in (car (make-hash input)) using (hash-key k)
        when (or (equal #\S v) (equal #\a v)) collect k))

(defun find-path (input)
  (destructuring-bind (hash start end) (make-hash input)
    (find-path-for hash start end)))

(defun find-all-paths (input)
  (let ((all-starts (find-all-starts input)))
    (destructuring-bind (hash start end) (make-hash input)
      (declare (ignore start))
      (loop for s in all-starts for i = 0 then (1+ i)
            collect (find-path-for hash s end)))))

(defun day12-part1 () (assert (equal 456 (find-path (input)))))

(defun day12-part2 () (assert (equal 454 (apply #'min (find-all-paths (input))))))

(defun input ()
  '(
    "abcccccccccccccccccccccccccccccccccccccaaaaaaacccccccaaaaaaaaaaaccccccccccccccccccccaaacaaaaaaaacccccccccccccccccccccccccccccccccccaaaaa"
    "abccccccccccccccccccaaccaacccccccccccccaaaaaaaccccccccaaaaaaaaaaacccccccaaaaccccccccaaaaaaaaaaaaacccccccccccccccccccccccccccccccccaaaaaa"
    "abccccccccccccccccccaaaaaaccccccccccaaaccaaaaaacccccccaaaaaaaaaaccccccccaaaaccccccaaaaaaaaaaaaaaacccccccccccccccccccaaacccccccccccaaaaaa"
    "abcccccccccccccccccccaaaaacccccccccccaaccaacaaaccccccaaaaaaaaaaaccccccccaaaacccccaaaaaaaaacaaaaaaacccccccccccccccccaaaacccccccccccaaacaa"
    "abccccccccccccccccccaaaaaaccccccccaacaaaaaacccccccccaaaaaaaaaaaaacaaaccccaaccccccaaaaaaaaacaacccccccccccccccccaaaccaaaacccccccccccccccaa"
    "abcccccccccccccccccaaaaaaaacccccccaaaaaaaaccccccaaaaaaaacaaaacaaaaaaacccccccccaaccccaaaaaacaaacccccccccccccccaaaakkkaaccccccccccccccccaa"
    "abcccccccccccccccccaaaaaaaaccccccccaaaaaccccaacccaaaaaaaaaaaacaaaaaaccccccccccaacccaaaaaaaaaaaacccccccccccccccakkkkkklcccccccccccccccccc"
    "abaaacccccccccccaaccccaaccccccccccccaaaaaccaaacccaaaaaaaaaaaaaaaaaaaaccccccaaaaaaaacaacccaaaaaaccccccccccccccckkkkkkkllcccccccaaaccccccc"
    "abaaaacccccccaacaaccccaacccccccccccaaacaaaaaaaccccaaaaaaaaaaaaaaaaaaaacccccaaaaaaaaaaaccccaaaaacccccccccccccckkkksssllllccccccaaaaaacccc"
    "abaaaacccccccaaaaacccccccccccaaaccccaacaaaaaaccccaaaaaacaaaaaaaaaaaaaacccccccaaaaccccccccaaaaacccccccccccccckkkksssssllllcccccaaaaaacccc"
    "abaaacccccccccaaaaaaccccccccaaaaccccccccaaaaaaaacaaaaaaaaaaaaacaaacaaacccccccaaaaacccccccaaaaacccccccccccccjkkkrssssssllllccccccaaaccccc"
    "abccccccccccaaaaaaaaccccccccaaaacccccccaaaaaaaaacaacaaaaaaaaaacaaaccccccccccaaacaaccccccccccccccccccccccccjjkkrrsuuussslllllcccccaaccccc"
    "abccaaacccccaaaaacccccccccccaaaaccccccaaaaaaaaaacccccaaaaaaaaaacaaccccccccccaacccacccccccccccccccccccccjjjjjjrrrsuuuussslllllmcccddacccc"
    "abcccaaaccaccacaaaccccccccccccccccccccaaaaaaaccccccccccaaaaaaaaccccccaacccccccccccaaaaacccccccccccccccjjjjjjrrrruuuuuusssllmmmmmddddcccc"
    "abccaaaaaaaacccaaaccccccccccccccccaaacccccaaaccccccccccccaaacccccccccaacccccccccccaaaaacccccccccccccjjjjjrrrrrruuuxuuussqqqqmmmmmdddcccc"
    "abcaaaaaaaacccaaaaaacaaaaaccccccaaaaaaccccaaacccaaccccccccaaccccccaaaaaaaaccaaacccaaaaaaccccccccccccjjjjrrrrrruuuxxxuuuqqqqqqqmmmdddcccc"
    "abaaaaaaaaaccccaaaaacaaaaaccccccaaaaaaaaccccccaaaaaaccccccccccccccaaaaaaaaccaaacaaaaaaaacccccccccccjjjjrrrtttuuuuxxxyvvvvvqqqqmmmdddcccc"
    "abaaaaaaaaaccaaaaaaacaaaaaaccccccaaaaaaaacccccaaaaaaccccccccccccccccaaaaccaaaaaaaaaaaaaacccccccccaaiijqqqrttttuuuxxyyvvvvvvvqqmmmdddcccc"
    "abcaaaaaaaaccaaaaaaaaaaaaaacccccaaaaaaaacccccccaaaacccccaaaaccccccccaaaaacaaaaaaaaccaaccccccccccaaaiiiqqqttttxxxxxxyyyyyyvvvqqmmmdddcccc"
    "abcccaaaaaaacaaaaaaaaaaaaaacccccaaaaaaaaaaaccccaaaaccccaaaaacccccccaaaaaacaaaaaaacccccccccccccccaaaiiiqqqtttxxxxxxxyyyyyyvvqqqmmmdddcccc"
    "SbcccaacccaccccaaacacccaaacccccccccaaaaaaaaacccaccaccccaaaaaaccccccaaccaacccaaaaaccccccccccccccccaaiiiiqqtttxxxxEzzzyyyyvvvqqqmmmddccccc"
    "abccaaaccccccccaaccccccccccccccccccaaaaaaaaccccccccccccaaaaaaccccccccccccccaaacaaaccaacccccccccccccciiiqqqttttxxxyyyyyvvvvqqqmmmdddccccc"
    "abccccccccccccccccccccccccccccccccaaaaaaaccccccccccccccaaaaaacccccccccccccccaacccccaaaaaaaccccccccccciiiqqqttttxxyyyyyvvvrrrnnneeecccccc"
    "abcaaaaccccccccccccccccccccccccccaaaaaaaaccccccccccccccccaacccccccccccccccccccccccccaaaaacccccccccccciiiqqqqttxxyyyyyyyvvrrnnnneeecccccc"
    "abcaaaaacccccccccccccccccccccccccaaaacaaacccaccaaacccccccccccccccccccccccccaaaccccaaaaaaaccccccccccccciiiqqqttwwyywwyyywwrrnnneeeccccccc"
    "abaaaaaacccaccaccccccccccccccccccaaaaccaacccaaaaaaccccccccccccccccaaaccccaaaaaacccaaaaaaaacccccccccccciiiqqqtswwwwwwwwwwwrrnnneeeccccccc"
    "abaaaaaacccaaaaccccccccaaaacccccccaaacccccccaaaaaacccccccccccccccaaaaaaccaaaaaacccaaaaaaaacaaccccccaaciiiqppsswwwwsswwwwwrrrnneeeccccccc"
    "abcaaaaacccaaaaacccccccaaaacccccccccccccccccaaaaaaaccccccccccccccaaaaaaccaaaaaacccccaaaaaaaaaccccccaaaahhpppssswwsssswwwwrrrnneeeacccccc"
    "abcaaaccccaaaaaacccccccaaaaccccccccccccccccaaaaaaaaccccccccccccccaaaaacccaaaaaccccccaacaaaaaaaaccaaaaaahhpppsssssssssrrrrrrnnneeeacccccc"
    "abccccccccaaaaaaccccccccaacccccccccccccccccaaaaaaaaccccaacccccccccaaaaaccaaaaacccccccccaaaaaaaaccaaaaachhpppssssssoosrrrrrrnnneeeaaacccc"
    "abccccccccccaaccccccccccccccccaaaaaccccccaacccaaacccaaaaacccccccccaacaacccccccccccccccccaaaaaaacccaaaaahhhppppssppooooorroonnffeaaaacccc"
    "abaaccccccccccccccccccccccccccaaaaaccccccaacccaaaccccaaaaacccccccccccccccccccccccccccaacaaaaacccccaacaahhhppppppppoooooooooonfffaaaacccc"
    "abaccccccccccccccccccccccccccaaaaaacccaaaaaaaacccccccaaaaaccccccccccccccccccccccccaaaaaaaaaaaccccccccccchhhpppppppgggoooooooffffaacccccc"
    "abaccccccccccccccccccccccccccaaaaaacccaaaaaaaaccccccaaaaaccccccacccaacccccccccccccaaaaaccccaaccccccccccchhhhhhggggggggfffffffffaaacccccc"
    "abaacccccccccccccccccccccccccaaaaaacccccaaaacccccccccaaaacccaacaacaaacccccccccccccaaaaaaacccccccccccccccchhhhgggggggggffffffffccaacccccc"
    "abcccccccaacccccccccccccccccccaaaccccccaaaaaccccccccaaaaccaaaacaaaaacccccccccccccaaaaaaaaccccccccccccccccchhhggggaaaagffffffcccccccccccc"
    "abcccccccaacccccccccccccaacccccccccccccaaaaaaccaaccccaaaaaaaaacaaaaaacccccccaaaacaaaaaaaacccccccccccaacccccccaaaacaaaacccccccccccccccccc"
    "abccccaaaaaaaacccccccaacaaaccccccccccccaaccaacaaaacccaaaaaaaacaaaaaaaaccccccaaaaccacaaaccaaaccccaaaaaacccccccaacccaaaacccccccccccccaaaaa"
    "abccccaaaaaaaacccccccaaaaaccccccccccccccccccccaaaaccccaaaaaaacaaaaaaaaccccccaaaaccccaaaccaaaaaccaaaaaaaacccccccccccaaaccccccccccccccaaaa"
    "abccccccaaaaccccccccccaaaaaaccccccccccccccccccaaaacccaaaaaaaaaaccaaccccccccccaacccccccccaaaaacccaaaaaaaacccccccccccaaaccccccccccccccaaaa"
    "abcccccaaaaaacccccccaaaaaaaacccccccccccccccccccccccaaaaaaaaaaaaaaaacccccccccccccccccccccaaaaaacccaaaaaaaccccccccccccccccccccccccccaaaaaa"))
