(defun input () "#################################################################################
#.......#.A...........#.........#.......#.#.........#.............#...#..r......#
#.###.###.#####.#####.#.#######.#.#######.#.#.#####.#.#.#.#######.###.#.###.###.#
#...#.#...#.....#.#...#...#.....#.......#...#.#...#o#.#.#.#...#.#...#.#...#.#...#
#####.#.###.#####.#.###.#W###.#########.#####.#.#.###.#.#.#.#.#.###.#K###.#.#####
#.....#.#.#...#...#...#.#...#...........#...#...#.....#.#.#.#.#.#...#...#.#.....#
#M###.#.#.###.#.#####.#####.###########.#.#.###.#######.###.#.#.#.#####.#.#####.#
#.#...#...#...#.#...#.....#...#..z....#.#.#...#...#.....#...#.#.#.#...#.......#.#
#.###.###.#.###.#.#.#####.###.#.###.#.#.#.###.#####.#.###.###.#.#.#.#.#####.###.#
#...#...#.#.#....y#.....#.....#.#.#.#.#.#...#.#...#.#.#...#...#.#...#.#...#.#m..#
#.#.#####.#.###########.#######.#.#.#.#.###.#.#.#X#.#.#.###.###.#####.#.#.###.###
#.#.#...#.#s..#.......#.......#...#.#.#.#...#...#.#.#.#.#.#.#.......#...#...#...#
###.#.#.#.###.#.#####.###.###.#####.#.#.#.#######.#.###.#.#.#.#####.#######.###.#
#...#.#...#.#...#...#.#...#.......#.#.#.#.#.......#.....#.#.#.#.....#.....#.....#
#.###.#####.#####.#.#C#.###.#####.#.#.#.#.#####.###.#####.#.###.#####.#.#.#####.#
#.....#.....#.....#.#.#.#...#...#...#.#.#.#.F.#...#...#...#...#.#...#.#.#.....#.#
#.#######.#.###.###.#.#.#####.#.#######.#.#.#.###.###.###.###.#.#.#.#.#.#######.#
#.#.......#...#.#...#.#...#...#.#.......#...#.#.#...#.......#.#.#.#.#.#.........#
#.#.#########.#.#####.###N#.###.#.###########.#.###.#########.#.#.#.#.###########
#...#...#.......#.....#...#.#.#.#.......#...#.#...............#...#.#.#....q....#
#####.###.#######.#####.###.#.#.#######.#.#.#.#############.#####.#.#.#.#.#####.#
#g..#.....#.....#...#.#...#...#.......#.#.#.#..l..........#.#...#.#.#.#.#...#...#
###.#.#####.###.###.#T#.#.###.###.#####.###.#############.#####B#.#.#.#.###.#####
#...#.#...#...#.....#.#.#...#.#.#.#...#.#.......#.......#...#...#.#...#...#.....#
#.###.###.###.#####.#.###.###.#.#.#.#.#.#.#####.#.###.#####.#.###.#####.#######.#
#...#...#...#.#.#...#...#.I...#.#...#...#.....#.#...#.#...#...#.#...#.#.#.......#
###.###.#.###.#.#.#####.#####.#.#############.#####.#.#.#.#####.###.#.#.#.#######
#.....#.#...#.#.......#.#...#...........#.....#.....#...#.......#.#.#.#.#.......#
#.#####.###.#.#######.#.#.#.#####.#######.###.#.###########.###.#.#.#.#.#######.#
#...........#.#e....#.#...#...#...#.....#.#...#...#.........#.....#.#...#.......#
#.###########.#.###.#########.#####.###.#.#######.#.#####.#######.#.#####.#####.#
#.....#.#.....#.#.#.........#.......#...#.#.......#.#.....#.....#.#...#...#.#...#
#####.#.#.#####.#.#####.#.###########.###.#.#######.#####.#.###.#.###.#.###.#.###
#...#.#.#.......#.#.....#.#.........#.#.#...#.#.....#...#.#.#...#...#.#...#...#.#
#.###.#.#########.#.#####.#V#.#####.#.#.#.###.#.###.#.#.###.#.#######.###.#.###.#
#.#...#.....#.....#.#.....#.#...#...#.#.#.#...#.#.#.#.#.....#.........#...#.#...#
#.#.###.###.#.#####.#######.###.#.###.#.#.###.#.#.#.#.#################.###.#.#.#
#.#.....#.#.#.....#.......#.#.#.#...#.#.#.#...#.#...#.#.........#.......#.S.#.#.#
#.#######.#.#####.#######.#.#.#.#####.#.#.#.#.#.#####.#####.###.#.#######.###.#.#
#.......................#.....#.............#.#.............#.....#...........#.#
#######################################.@.#######################################
#...............#.....#.....#.............#.........................D.......#..u#
#.#############.#####.#.#.###.###.#####.#.#.#.#####.#######.###############.#.###
#.....#.....#...#...#...#.....#.#.#.....#...#....v#.#.....#.#.......#.....#.#.E.#
#####.#.#####.###.#.#.#########.#.#######.#######.###.###.###.#####.#.#####.#.#.#
#.....#.....#.....#.#.#...#.....#.......#.#.......#.....#.#...#...#.#....j#t#.#.#
#.#########.#######.#.#.#.#.###########.#.#.#######.#####.#.###.###.#####.#.#.#.#
#.......#...#...#...#...#...#.......#...#.#.#.....#.#.....#.#.....#.#...#.#.#.#.#
#######.#.#.###.#.#######.###.#####.#.#####.#.###.#.#.#####.#####.#.#.#.#.#.###.#
#.....#.#.#.....#...#.....#...#.....#...#...#.#.#...#.....#.......#...#.#.#.#...#
###.###.#.#####.###.#####.#.###.#####.#.#.#.#.#.#########.#######.#####.#.#.#.#.#
#...#...#...#...#.#.....#.#.#.#.#.....#.#.#d#.#.....#.......#.....#.....#.#...#.#
#.###.#####.#.###.#####.#.#.#.#.#######.#.###.###.#.#.#######.#####.#####.#####.#
#.....#...#.#.....#.....#.#.#.#...#.....#.....#...#.#.....#.....#...#...#.....#.#
#.#####.#.#.#.#####.#####.#.#.###.#.###.#######.#.#######.#.#####.#####.#.#.#.#.#
#.#.....#.#.#.#.....#.....#.#...#.#.#...#.......#.#.....#...#.#...#.....#.#.#.#.#
#.###.#.#.#.#.#.###########.###.#.#.#.###.#########.#.#######.#.#####.###.#.###.#
#.H.#.#.#.#.#.#.....#.....#.#...#...#...#.#.....#...#.........#.#.....#...#.....#
###.###.#.#.#.#####.#.###.#.#.#########.#.#.###.#.#######.#####.#.#.#.#.#########
#.#.#...#...#...#.#...#...#.#...#.......#...#.#...#.....#...#...#.#.#.#.........#
#.#.#.#.#######.#.#####.###.#.#.#.#######.###.#########.###.#.#####.#.#########.#
#...#.#.#...#.#.#...#...#...#.#.#...#...#...#...#.....#...#.#.#.....#.#.......#.#
#.###.###.#.#.#.###.#######.#.#.###.#.#.###.#.###.#.#.#.#.#.###.#####.###.###.#.#
#.#...#...#.#.#.#...........#.#...#.#.#.#.#.#.#...#.#...#.#.....#...#...#.#.#.#.#
#.###.#.###.#.#.#.###########.###.#.#.#.#.#.#.#.###.#####.#####.###.###.#.#.#.#.#
#...#...#.#.#.#...#...#.U...#.#.#...#.#.#...#.....#.#.....#...#...#...#...#...#.#
###.#.###.#.#.#####J#.#.#.###.#.#####.###.#######.#.#####.#.#####.#.#####.#####.#
#...#.Q.#.#.#.....#.#...#.....#.........#.......#.#f....#.L.#.....#h....#.#...#.#
#.#####.#.#.###.#.#.#################.#.#######.#.#####.###.#.#####.#.###.#.#.#.#
#.#...#...#...#.#.#.#...#.......#...#.#.#.#...G.#.....#.Z.#.#...#...#.....#.#...#
#.#.#####.###.###.#.#.#.#####.#.#.#.###.#.#.#############.#.###.#########.#.#####
#k#.#.....#.#...#.#..c#...#...#...#.#...#.#.#........n#...#...#.#.......#.#...#.#
#.#.#.#####.###.#.#######.#.#######.#.#.#.#.#.#######.#.###.###.#.#####.#####.#.#
#.#...#.....#...#.#.P...#.#.#.....#...#.#.#.#.#...#...#.#...#...#.....#.....#p..#
#.###.#.#.###.###.#.#.###.#.###.#.#####.#.#.#.#.###.###.###.#.#######.###.#####.#
#...#...#.#...#.#...#...#.#...#.#...#...#.O.#.#.....#.#...#b#.#.......#.#.#...#.#
###.#####.#.###.#.#####.#.###.#####.#.###.###.#.#####.#.#.###.#.#######.#.#.#.#.#
#.#.#.....#.#a..#.#...#.#.#...#.....#...#...#i#.#...#...#.Y.#.#.#....w..#.#.#...#
#.#R#######.#.#.#.###.#.#.#.###.#######.###.#.#.#.#.#######.#.#.#####.#.#.#.#####
#...........#.#.......#.....#...........#.....#x..#.........#.........#.#.......#
#################################################################################")

(defun newhash ()
  (make-hash-table :test #'equal))

(defun sethash (key value hash)
  (setf (gethash key hash) value)
  hash)

(defun sethash-if-new (key value hash)
  (if (null (gethash key hash))
      (sethash key value hash))
  hash)

(defun mergehash (to from)
  (maphash #'(lambda (k v)
               (sethash k v to)) from)
  to)

(defun copyhash (hash)
  (mergehash (newhash) hash))

(defun go-to-key (labirynth keys) (list labirynth keys))

(defun labirynth-to-hash (input)
  (let ((x 0)
        (y 0)
        (hash (newhash)))
    (loop for char across input do
         (cond
           ((= 10 (char-code char))
            (setf y (1+ y))
            (setf x 0))
           ((char= #\# char)
            (setf x (1+ x)))
           (t
            (sethash (list x y) char hash)
            (setf x (1+ x)))))
    hash))

(defun print-hash (hash)
  (format t "hash:~%")
  (maphash #'(lambda (k v) (format t "~a ~a~%" k v)) hash))

(defun entrancep (value)
  (char= #\@ value))

(defun doorp (value)
  (and (char>= value #\A) (char<= value #\Z)))

(defun keyp (value)
  (and (char>= value #\a) (char<= value #\z)))

(defun key-doorp (key door)
  (char= key (door-to-key door)))

(defun door-to-key (door)
  (char-downcase door))

(defun get-neighbours (location)
  (list (mapcar #'+ location '(1 0))
        (mapcar #'+ location '(-1 0))
        (mapcar #'+ location '(0 1))
        (mapcar #'+ location '(0 -1))))

(defun find-all-types-location (type-function labirynth)
  (loop
     for location being the hash-keys in labirynth
     when (funcall type-function (gethash location labirynth)) collect location))

(defun find-all-keys-location (labirynth)
  (find-all-types #'keyp labirynth))

(defun find-all-doors-location (labirynth)
  (find-all-types #'doorp labirynth))

(defun find-entrance-location (labirynth)
  (car (find-all-types #'entrancep labirynth)))

(defun cache-paths (hash)
  (let ((all-keys (find-all-keys-location hash)))
    (loop for start in all-keys
       for rest on (cdr all-keys)
       for paths =
         (progn
           (format t "start:~a~%" start)
           (loop for end in rest
              for path = (shortest-path start end hash) collecting (list start end path)))
       appending paths)))

(defun find-keys-and-doors-in-range (start owned-keys labirynth)
  (loop named outer
     with visited = (sethash start start (newhash))
     with keys = (newhash)
     with doors = (newhash)
     for step = 1 then (1+ step)
     for newly-visited = (newhash) then (newhash) do
       (loop
          for location being the hash-keys in visited do
            (dolist (n (get-neighbours location))
              (let ((type (gethash n labirynth)))
                (cond
                  ((not (null (gethash n visited))) nil)
                  ((null type) nil)
                  ((doorp type)
                   (sethash-if-new n (list step type) doors)
                   (if (can-pass type owned-keys)
                       (sethash n n newly-visited)))
                  (t
                   (sethash n n newly-visited)
                   (if (keyp type)
                       (sethash-if-new n (list step type) keys)))))))
       (mergehash visited newly-visited)
       (if (= 0 (hash-table-count newly-visited)) (return-from outer (list keys doors)))))

(defun go-from-point (start owned-keys labirynth)
  (loop named outer
     with visited = (sethash start start (newhash))
     with keys = (newhash)
     for step = 1 then (1+ step)
     for newly-visited = (newhash) then (newhash) do
       (loop
          for location being the hash-keys in visited do
            (dolist (n (get-neighbours location))
              (let ((type (gethash n labirynth)))
                (cond
                  ((not (null (gethash n visited))) nil)
                  ((null type) nil)
                  ((doorp type)
                   (if (can-pass type owned-keys)
                       (sethash n n newly-visited))
                   (sethash-if-new type (list step n) keys))
                  (t
                   (sethash n n newly-visited)
                   (if (keyp type)
                       (sethash-if-new type (list step n) keys)))))))
       (mergehash visited newly-visited)
       (if (= 0 (hash-table-count newly-visited))
           (return-from outer keys))))

(defun iterate-going (input)
  (let* ((labirynth (labirynth-to-hash input))
         (entrance (find-entrance-location labirynth)))
    (let ((new-keys (go-from-point entrance (newhash) labirynth)))
      (loop
         for key being the hash-keys in new-keys
         for loop-new-keys = (go-from-point
                              (location key new-keys)
                              (add-key key new-keys (newhash))
                              labirynth) collecting loop-new-keys do
           (format t "Looping k: ~a, nks: ~a~%" key loop-new-keys)))))

(defun gt (input)
  (let* ((lab (labirynth-to-hash input))
         (ak (length (find-all-keys-location lab)))
         (ad (length (find-all-doors-location lab)))
         (ent (find-entrance-location lab))
         (hashes ()))
    (loop for new-keys = (go-from-point ent (newhash) lab)
       then (go-from-point ent new-keys lab)
       collecting (copyhash new-keys) do
         (setf hashes (push (copyhash new-keys) hashes))
         (if (= (hash-table-count new-keys) (+ ad ak)) (return hashes)))))

(defun makediff (hash-list)
  (concatenate 'list
   (loop for x in hash-list
      for y in (cdr hash-list)
      collecting (diff-hash x y))
   (last hash-list)))

(defun diff-hash (big small)
  (let ((diff (newhash)))
    (loop for k being the hash-keys in big using (hash-value v) do
         (if (null (gethash k small))
             (sethash k v diff)))
    diff))

(defun go-through (input)
  (let* ((labirynth (labirynth-to-hash input))
         (all-keys (length (find-all-keys-location labirynth)))
         (entrance (find-entrance-location labirynth)))
    (let ((new-keys (go-from-point entrance (newhash) labirynth))
          (total 9999999999))
      (go-one-step nil new-keys (newhash) all-keys labirynth total))))

(defun go-one-step (steps-so-far new-keys owned-keys keys-total labirynth found-total)
  (cond
    ((not (new-location-p steps-so-far)) found-total)
    ((= (hash-table-count owned-keys) keys-total)
     (let ((new-total (count-steps steps-so-far)))
       (if (> new-total found-total)
           found-total
           new-total)))
    (t
     (loop for (steps location) being the hash-values in new-keys using (hash-key key)
        for new-keys = (sethash key (list steps location) (copyhash owned-keys))
        for next-keys = (go-from-point location new-keys labirynth)
        for new-steps = (push-to-copy (list key steps location) steps-so-far)
        for total = (go-one-step new-steps next-keys new-keys keys-total labirynth found-total)
        minimize total))))

(defun push-to-copy (element list)
  (let ((list (loop for e in list collecting e)))
    (setf list (push element list))
    list))

(defun new-location-p (steps)
  (= (length steps) (length (delete-duplicates (loop for (k nil nil) in steps collecting k)
                              :test #'equal))))

(defun count-steps (steps)
  (let ((total (loop for (nil step nil) in steps sum step)))
    total))

(defun location (key hash)
  (destructuring-bind (steps location) (gethash key hash)
    (declare (ignore steps))
    location))

(defun add-key (key from to)
  (sethash key (gethash key from) to))

(defun shortest-path (start end labirynth)
  (loop named outer
     with visited = (sethash start start (newhash))
     for step = 0 then (1+ step)
     for newly-visited = (newhash) then (newhash) do
       (loop
          for location being the hash-keys in visited do
            (dolist (n (get-neighbours location))
              (if (not (visited-or-wall-p n visited labirynth))
                  (sethash-if-new n n newly-visited))))
       (mergehash visited newly-visited)
       (cond
         ((not (null (gethash end visited))) (return-from outer step))
         ((= 0 (hash-table-count newly-visited)) (return-from outer -1))
         (t nil))))

(defun visited-or-wall-p (location visited labirynth)
  (or (not (null (gethash location visited)))
      (null (gethash location labirynth))))

(defun can-pass (door keys)
  (let ((to-lookup
         (if (hash-table-p keys)
             (loop for k being the hash-keys in keys collecting k)
             keys)))
    (not (null (member (door-to-key door) to-lookup)))))

(defun get-keys (input own-keys)
  (destructuring-bind (start keys-doors)
      (let* ((hash (labirynth-to-hash input))
             (start (find-entrance-location hash)))
        (list start (find-keys-and-doors-in-range start own-keys hash)))
    (destructuring-bind (keys doors) keys-doors
      (print-hash keys)
      (print-hash doors))
    start))

(defun sample1 () "#########
#b.A.@.a#
#########")

(defun sample2 () "########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################")

(defun sample3 () "########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################")

(defun sample4 () "#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################")

(defun sample5 () "########################
#@..............ac.GI.b#
###d#e#f################
###A#B#C################
###g#h#i################
########################")
