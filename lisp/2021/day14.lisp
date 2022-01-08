(defun new-hash ()
  (make-hash-table :test #'equal))

(defun to-hash (input)
  (let ((hash (new-hash)))
    (loop for (key nil value) in input
          do (setf (gethash key hash) value)
          finally (return hash))))

(defun make-a-step (instruction hash)
  (loop for (a b) on instruction by #'cdr
        when (not (null b))
          append (concatenate 'list (list a) (get-hash-list (list a b) hash))
        else append (list a)))

(defun get-hash-list (key hash)
  (let ((v (gethash key hash)))
    (if (not (null v))
        (list v)
        nil)))

(defun get-hash-value (key hash)
  (let ((v (gethash key hash)))
    (if (not (null v)) v 0)))

(defun n-steps (n instruction hash)
  (loop for x from 0 upto n
        for next-instr = instruction then (make-a-step next-instr hash)
        finally (return next-instr)))

(defun map-level (n-pair hash)
  (destructuring-bind (n (a b)) n-pair
    (loop with map = nil
          for x from n downto 1
          for prev = b then next
          for next = (gethash (list a b) hash) then (gethash (list a next) hash)
          do (push (list (1- x) (list next prev)) map)
          when (null next) return map
            finally (return map))))

(defun map-and-reduce (n-pair hash)
  (let ((counter (new-hash))
        (list (map-level n-pair hash)))
    (loop do (count-me (cadr (pop list)) counter)
          when (null list) return (inc-counter (car (cadr n-pair)) counter)
            do (let ((to-map (pop list)))
                 (setf list (append (map-level to-map hash) list))))))

(defun make-cache (depth hash)
  (let ((cache (new-hash)))
    (loop for x being the hash-key in hash
          do (setf (gethash x cache) (map-and-reduce (list depth x) hash))
          finally (return cache))))

(defun map-and-reduce-cached (n-pair cache hash)
  (destructuring-bind (n pair) n-pair
    (let ((counter (new-hash))
          (list (n-steps n pair hash)))
      (loop for (a b) on list by #'cdr
            for to-sum = (gethash (list a b) cache)
            when (null b) return counter
            do (sum-to-hash (dec-counter a counter) to-sum)
            finally (return counter)))))

(defun count-n-steps (n template hash)
  (let* ((cache-size (/ n 2))
         (cache (make-cache cache-size hash)))
    (inc-counter
     (car template)
     (sum-hashes (loop for (a b) on template by #'cdr
                       collect (map-and-reduce-cached
                                (list cache-size (list a b))
                                cache
                                hash))))))

(defun count-me (ab counter)
  (destructuring-bind (a b) ab
    (inc-counter a counter)
    (inc-counter b counter)))

(defun sum-to-hash (sum-to hash)
  (loop for k being the hash-key in hash using (hash-value v)
        for old-value = (get-hash-value k sum-to)
        do (setf (gethash k sum-to) (+ old-value v))))

(defun sum-hashes (list-of-hashes)
  (let ((sum-hash (new-hash)))
    (loop for h in list-of-hashes
          do (loop for k being the hash-key in h using (hash-value v)
                   for old-value = (get-hash-value k sum-hash)
                   do (setf (gethash k sum-hash) (+ old-value v)))
          finally (return sum-hash))))

(defun inc-counter (key hash)
  (update-counter-by 1 key hash))

(defun dec-counter (key hash)
  (update-counter-by -1 key hash))

(defun update-counter-by (number key hash)
   (let ((value (gethash key hash)))
     (if value (setf (gethash key hash) (+ number value))
         (setf (gethash key hash) number))
     hash))

(defun find-most-common (list)
  (let ((counter (new-hash)))
    (loop for i in list
          do (inc-counter i counter)
          finally (return counter))))

(defun day14-1 ()
  (assert (= 2010
             (loop for x being the hash-values
                     in (find-most-common (n-steps 10 (instruction) (to-hash (input))))
                   maximize x into mx
                   minimize x into mn
                   finally (return (- mx mn))))))

(defun day14-2 ()
  (assert (= 2437698971143
             (loop for x being the hash-values
                     in (count-n-steps 40 (instruction) (to-hash (input)))
                   maximize x into mx
                   minimize x into mn
                   finally (return (- mx mn))))))

(defun instruction () '(O O V S K S P K P P P N N F F B C N O V))

(defun input ()
  '(
    ((B C) -> C)
    ((P P) -> O)
    ((S K) -> K)
    ((K H) -> N)
    ((O K) -> S)
    ((P C) -> O)
    ((V P) -> K)
    ((C F) -> K)
    ((H C) -> H)
    ((F V) -> V)
    ((P B) -> P)
    ((N K) -> H)
    ((C K) -> F)
    ((F H) -> H)
    ((S V) -> B)
    ((N H) -> C)
    ((C P) -> S)
    ((H P) -> O)
    ((H S) -> O)
    ((B K) -> B)
    ((K C) -> P)
    ((V V) -> B)
    ((O F) -> O)
    ((K P) -> V)
    ((F O) -> V)
    ((F K) -> V)
    ((V H) -> K)
    ((K B) -> P)
    ((K F) -> H)
    ((S H) -> S)
    ((H F) -> O)
    ((B B) -> F)
    ((F C) -> O)
    ((S O) -> S)
    ((B S) -> O)
    ((H H) -> C)
    ((B O) -> S)
    ((C O) -> F)
    ((V C) -> V)
    ((K S) -> N)
    ((O C) -> N)
    ((F P) -> P)
    ((H N) -> B)
    ((H V) -> V)
    ((H O) -> P)
    ((K O) -> C)
    ((S F) -> H)
    ((N O) -> N)
    ((P S) -> C)
    ((B P) -> K)
    ((S C) -> C)
    ((N P) -> C)
    ((C H) -> V)
    ((K V) -> B)
    ((H K) -> V)
    ((O P) -> V)
    ((S P) -> V)
    ((N C) -> V)
    ((F F) -> B)
    ((C C) -> V)
    ((C S) -> F)
    ((S B) -> C)
    ((O S) -> C)
    ((F N) -> O)
    ((C V) -> P)
    ((O H) -> H)
    ((O O) -> P)
    ((P O) -> F)
    ((N S) -> H)
    ((V B) -> K)
    ((O V) -> K)
    ((P H) -> H)
    ((B H) -> V)
    ((S S) -> B)
    ((P K) -> F)
    ((V K) -> O)
    ((B N) -> V)
    ((V F) -> O)
    ((P F) -> H)
    ((V S) -> K)
    ((O N) -> V)
    ((B F) -> F)
    ((C N) -> F)
    ((V O) -> B)
    ((F S) -> K)
    ((O B) -> B)
    ((P N) -> H)
    ((N F) -> O)
    ((V N) -> P)
    ((B V) -> S)
    ((N V) -> V)
    ((F B) -> V)
    ((N B) -> P)
    ((C B) -> B)
    ((K K) -> S)
    ((N N) -> F)
    ((S N) -> B)
    ((H B) -> P)
    ((P V) -> S)
    ((K N) -> S)))
