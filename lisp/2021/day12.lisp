(defun new-hash ()
  (make-hash-table :test #'equal))

(defun clone (hash)
  (let ((cloned (new-hash)))
    (loop for k being the hash-keys in hash using (hash-value v)
          do (setf (gethash k cloned) v)
          finally (return cloned))))

(defun remove-from-hash (key hash)
  (loop for k being the hash-keys in hash using (hash-value v)
        do (setf (gethash k hash) (remove key v :test #'equal :count 1))
        finally (return (remove-if-last key hash))))

(defun remove-if-last (key hash)
  (loop for v being the hash-values in hash
        when (member key v :test #'equal)
          return hash
        finally (progn (remhash key hash)
                       (return hash))))

(defun clone-and-remove-if-small (key hash)
  (let ((cloned (clone hash)))
    (if (smallp key)
        (remove-from-hash key cloned)
        cloned)))

(defun smallp (key)
  (lower-case-p (char key 0)))

(defun update-hash (k v hash)
  (let ((value (gethash k hash)))
    (if (null value)
        (setf (gethash k hash) (list v))
        (setf (gethash k hash) (append value (list v))))
    hash))

(defun to-list (string)
  (loop for l across string
        with collect-first = t
        when (char-equal #\- l)
          do (setf collect-first nil)
        else
          when collect-first collect l into first
            else collect l into second
        finally (return (list (list-to-string first) (list-to-string second)))))

(defun list-to-string (list)
  (apply #'concatenate 'string (mapcar #'string list)))

(defun to-hash (input)
  (let ((pairs (loop for i in input collect (to-list i)))
        (hash (new-hash)))
    (loop for (a b) in pairs
          do (update-hash b a (update-hash a b hash))
          finally (return hash))))

(defun to-hash-all-small (key input)
  (let ((pairs (loop for i in input collect (to-list i)))
        (hash (new-hash)))
    (loop for (a b) in pairs
          do (update-twice-small key b a hash)
             (update-twice-small key a b hash)
          finally (return hash))))

(defun get-all-small (input)
  (let* ((hash (to-hash input))
         (all-small (loop for k being the hash-key in hash
                          when (and (not (terminalp k)) (smallp k))
                            collect k)))
    all-small))

(defun get-all-paths (input)
  (let ((all-small (get-all-small input)))
    (loop for s in all-small
          for h = (to-hash-all-small s input)
          for l = (cons nil nil)
          do (recursive-step "start" h nil l)
          append (car l))))

(defun update-twice-small (twice-key key value hash)
  (update-hash key value hash)
  (when (equal twice-key value)
    (update-hash key value hash))
  hash)

(defun terminalp (value)
  (member value '("start" "end") :test #'equal))

(defun recursive-step (key hash visited full-paths)
  (when (equal key "end")
    (progn
      (let ((new-path (append visited (list key))))
        (when (not (member new-path (car full-paths) :test #'equal))
          (setf (car full-paths) (append (car full-paths) (list new-path)))))
      (return-from recursive-step full-paths)))
  (loop for n in (gethash key hash)
        do (recursive-step
            n
            (clone-and-remove-if-small key hash)
            (append visited (list key)) full-paths)))

(defun day12-1 ()
  (assert (= 3410 (length (car (let ((l (cons nil nil)))
                                 (recursive-step "start" (to-hash (input)) nil l) l))))))
(defun day12-2 ()
  (assert (= 98796 (length (remove-duplicates (get-all-paths (input)) :test #'equal)))))

(defun input ()
  '("hl-WP"
    "vl-fo"
    "vl-WW"
    "WP-start"
    "vl-QW"
    "fo-wy"
    "WW-dz"
    "dz-hl"
    "fo-end"
    "VH-fo"
    "ps-vl"
    "FN-dz"
    "WP-ps"
    "ps-start"
    "WW-hl"
    "end-QW"
    "start-vl"
    "WP-fo"
    "end-FN"
    "hl-QW"
    "WP-dz"
    "QW-fo"
    "QW-dz"
    "ps-dz"))
