(defun new-hash ()
  (make-hash-table :test #'equal))

(defun make-hash (input)
  (loop for shape in input with hash = (new-hash)
        do (loop for (xs ys arrow xe ye arrow-two) on shape by #'cdddr
                 when (not (null xe))
                   do (cond
                        ((= xs xe)
                         (loop for y from (min ys ye) to (max ys ye)
                               do (setf (gethash (list xs y) hash) 'r)))
                        (t
                         (loop for x from (min xs xe) to (max xs xe)
                               do (setf (gethash (list x ys) hash) 'r)))))
        finally (return hash)))

(defun find-boundaries (hash)
  (loop for (x y) being the hash-keys in hash
        minimize x into min-x
        maximize x into max-x
        minimize y into min-y
        maximize y into max-y
        finally (return (list min-x max-x min-y max-y))))

(defun is-stable-spot (pos hash stable-y limit)
  (destructuring-bind (x y) pos
    (cond
     ((and (equal 'limit limit) (equal y stable-y) t))
     (t
      (and (null (gethash (list x y) hash))
           (gethash (list x (1+ y)) hash)
           (gethash (list (1+ x) (1+ y)) hash)
           (gethash (list (1- x) (1+ y)) hash))))))

(defun fall (pos hash)
  (destructuring-bind (x y) pos
   (cond
     ((null (gethash (list x (1+ y)) hash))
      (list x (1+ y)))
     (t
      (cond
        ((null (gethash (list (1- x) (1+ y)) hash))
         (list (1- x) (1+ y)))
        ((null (gethash (list (1+ x) (1+ y)) hash))
         (list (1+ x) (1+ y))))))))

(defun drop-a-sand (hash limit-y limit)
  (loop with sand-start = '(500 0)
        for sand = sand-start then (fall sand hash)
        when (and (equal 'limit limit)
                  (gethash sand-start hash))
          do (return (list 'max-reached hash))
        when (is-stable-spot sand hash limit-y limit)
          do (setf (gethash sand hash) 's)
             (return hash)
        when (and (equal limit 'no-limit)
                  (> (cadr sand) limit-y))
          do (return (list 'max-reached hash))))

(defun drop-some-sand (input limit)
  (loop with hash = (make-hash input)
        with boundaries = (find-boundaries hash)
        for dropped = (drop-a-sand hash (1+ (cadddr boundaries)) limit)
        when (listp dropped)
          do (return (count-sand hash))))

(defun count-sand (hash)
  (loop for v being the hash-values in hash
        when (equal v 's) count v))

(defun day14-part1 ()
  (assert (equal 817 (drop-some-sand (input) 'no-limit))))

(defun day14-part2 ()
  (assert (equal 23416 (drop-some-sand (input) 'limit))))

(defun input ()
  '(
    (498 13 -> 498 17 -> 491 17 -> 491 21 -> 507 21 -> 507 17 -> 502 17 -> 502 13)
    (506 61 -> 506 57 -> 506 61 -> 508 61 -> 508 56 -> 508 61 -> 510 61 -> 510 55 -> 510 61 -> 512 61 -> 512 56 -> 512 61 -> 514 61 -> 514 56 -> 514 61 -> 516 61 -> 516 60 -> 516 61 -> 518 61 -> 518 55 -> 518 61)
    (498 13 -> 498 17 -> 491 17 -> 491 21 -> 507 21 -> 507 17 -> 502 17 -> 502 13)
    (506 61 -> 506 57 -> 506 61 -> 508 61 -> 508 56 -> 508 61 -> 510 61 -> 510 55 -> 510 61 -> 512 61 -> 512 56 -> 512 61 -> 514 61 -> 514 56 -> 514 61 -> 516 61 -> 516 60 -> 516 61 -> 518 61 -> 518 55 -> 518 61)
    (468 113 -> 472 113)
    (502 34 -> 502 33 -> 502 34 -> 504 34 -> 504 31 -> 504 34 -> 506 34 -> 506 27 -> 506 34 -> 508 34 -> 508 25 -> 508 34 -> 510 34 -> 510 32 -> 510 34 -> 512 34 -> 512 33 -> 512 34)
    (502 34 -> 502 33 -> 502 34 -> 504 34 -> 504 31 -> 504 34 -> 506 34 -> 506 27 -> 506 34 -> 508 34 -> 508 25 -> 508 34 -> 510 34 -> 510 32 -> 510 34 -> 512 34 -> 512 33 -> 512 34)
    (506 61 -> 506 57 -> 506 61 -> 508 61 -> 508 56 -> 508 61 -> 510 61 -> 510 55 -> 510 61 -> 512 61 -> 512 56 -> 512 61 -> 514 61 -> 514 56 -> 514 61 -> 516 61 -> 516 60 -> 516 61 -> 518 61 -> 518 55 -> 518 61)
    (506 61 -> 506 57 -> 506 61 -> 508 61 -> 508 56 -> 508 61 -> 510 61 -> 510 55 -> 510 61 -> 512 61 -> 512 56 -> 512 61 -> 514 61 -> 514 56 -> 514 61 -> 516 61 -> 516 60 -> 516 61 -> 518 61 -> 518 55 -> 518 61)
    (502 34 -> 502 33 -> 502 34 -> 504 34 -> 504 31 -> 504 34 -> 506 34 -> 506 27 -> 506 34 -> 508 34 -> 508 25 -> 508 34 -> 510 34 -> 510 32 -> 510 34 -> 512 34 -> 512 33 -> 512 34)
    (438 152 -> 442 152)
    (474 110 -> 474 103 -> 474 110 -> 476 110 -> 476 103 -> 476 110 -> 478 110 -> 478 108 -> 478 110 -> 480 110 -> 480 100 -> 480 110 -> 482 110 -> 482 103 -> 482 110 -> 484 110 -> 484 104 -> 484 110 -> 486 110 -> 486 103 -> 486 110 -> 488 110 -> 488 101 -> 488 110)
    (474 117 -> 478 117)
    (495 39 -> 511 39 -> 511 38)
    (474 110 -> 474 103 -> 474 110 -> 476 110 -> 476 103 -> 476 110 -> 478 110 -> 478 108 -> 478 110 -> 480 110 -> 480 100 -> 480 110 -> 482 110 -> 482 103 -> 482 110 -> 484 110 -> 484 104 -> 484 110 -> 486 110 -> 486 103 -> 486 110 -> 488 110 -> 488 101 -> 488 110)
    (444 147 -> 444 142 -> 444 147 -> 446 147 -> 446 142 -> 446 147 -> 448 147 -> 448 140 -> 448 147 -> 450 147 -> 450 137 -> 450 147 -> 452 147 -> 452 138 -> 452 147 -> 454 147 -> 454 142 -> 454 147 -> 456 147 -> 456 145 -> 456 147 -> 458 147 -> 458 140 -> 458 147)
    (474 110 -> 474 103 -> 474 110 -> 476 110 -> 476 103 -> 476 110 -> 478 110 -> 478 108 -> 478 110 -> 480 110 -> 480 100 -> 480 110 -> 482 110 -> 482 103 -> 482 110 -> 484 110 -> 484 104 -> 484 110 -> 486 110 -> 486 103 -> 486 110 -> 488 110 -> 488 101 -> 488 110)
    (471 93 -> 483 93 -> 483 92)
    (502 34 -> 502 33 -> 502 34 -> 504 34 -> 504 31 -> 504 34 -> 506 34 -> 506 27 -> 506 34 -> 508 34 -> 508 25 -> 508 34 -> 510 34 -> 510 32 -> 510 34 -> 512 34 -> 512 33 -> 512 34)
    (502 34 -> 502 33 -> 502 34 -> 504 34 -> 504 31 -> 504 34 -> 506 34 -> 506 27 -> 506 34 -> 508 34 -> 508 25 -> 508 34 -> 510 34 -> 510 32 -> 510 34 -> 512 34 -> 512 33 -> 512 34)
    (498 13 -> 498 17 -> 491 17 -> 491 21 -> 507 21 -> 507 17 -> 502 17 -> 502 13)
    (441 150 -> 445 150)
    (444 147 -> 444 142 -> 444 147 -> 446 147 -> 446 142 -> 446 147 -> 448 147 -> 448 140 -> 448 147 -> 450 147 -> 450 137 -> 450 147 -> 452 147 -> 452 138 -> 452 147 -> 454 147 -> 454 142 -> 454 147 -> 456 147 -> 456 145 -> 456 147 -> 458 147 -> 458 140 -> 458 147)
    (506 61 -> 506 57 -> 506 61 -> 508 61 -> 508 56 -> 508 61 -> 510 61 -> 510 55 -> 510 61 -> 512 61 -> 512 56 -> 512 61 -> 514 61 -> 514 56 -> 514 61 -> 516 61 -> 516 60 -> 516 61 -> 518 61 -> 518 55 -> 518 61)
    (444 147 -> 444 142 -> 444 147 -> 446 147 -> 446 142 -> 446 147 -> 448 147 -> 448 140 -> 448 147 -> 450 147 -> 450 137 -> 450 147 -> 452 147 -> 452 138 -> 452 147 -> 454 147 -> 454 142 -> 454 147 -> 456 147 -> 456 145 -> 456 147 -> 458 147 -> 458 140 -> 458 147)
    (456 125 -> 456 128 -> 453 128 -> 453 134 -> 462 134 -> 462 128 -> 460 128 -> 460 125)
    (444 147 -> 444 142 -> 444 147 -> 446 147 -> 446 142 -> 446 147 -> 448 147 -> 448 140 -> 448 147 -> 450 147 -> 450 137 -> 450 147 -> 452 147 -> 452 138 -> 452 147 -> 454 147 -> 454 142 -> 454 147 -> 456 147 -> 456 145 -> 456 147 -> 458 147 -> 458 140 -> 458 147)
    (471 93 -> 483 93 -> 483 92)
    (502 34 -> 502 33 -> 502 34 -> 504 34 -> 504 31 -> 504 34 -> 506 34 -> 506 27 -> 506 34 -> 508 34 -> 508 25 -> 508 34 -> 510 34 -> 510 32 -> 510 34 -> 512 34 -> 512 33 -> 512 34)
    (444 147 -> 444 142 -> 444 147 -> 446 147 -> 446 142 -> 446 147 -> 448 147 -> 448 140 -> 448 147 -> 450 147 -> 450 137 -> 450 147 -> 452 147 -> 452 138 -> 452 147 -> 454 147 -> 454 142 -> 454 147 -> 456 147 -> 456 145 -> 456 147 -> 458 147 -> 458 140 -> 458 147)
    (502 34 -> 502 33 -> 502 34 -> 504 34 -> 504 31 -> 504 34 -> 506 34 -> 506 27 -> 506 34 -> 508 34 -> 508 25 -> 508 34 -> 510 34 -> 510 32 -> 510 34 -> 512 34 -> 512 33 -> 512 34)
    (456 125 -> 456 128 -> 453 128 -> 453 134 -> 462 134 -> 462 128 -> 460 128 -> 460 125)
    (503 64 -> 503 67 -> 496 67 -> 496 71 -> 512 71 -> 512 67 -> 508 67 -> 508 64)
    (506 61 -> 506 57 -> 506 61 -> 508 61 -> 508 56 -> 508 61 -> 510 61 -> 510 55 -> 510 61 -> 512 61 -> 512 56 -> 512 61 -> 514 61 -> 514 56 -> 514 61 -> 516 61 -> 516 60 -> 516 61 -> 518 61 -> 518 55 -> 518 61)
    (444 147 -> 444 142 -> 444 147 -> 446 147 -> 446 142 -> 446 147 -> 448 147 -> 448 140 -> 448 147 -> 450 147 -> 450 137 -> 450 147 -> 452 147 -> 452 138 -> 452 147 -> 454 147 -> 454 142 -> 454 147 -> 456 147 -> 456 145 -> 456 147 -> 458 147 -> 458 140 -> 458 147)
    (502 34 -> 502 33 -> 502 34 -> 504 34 -> 504 31 -> 504 34 -> 506 34 -> 506 27 -> 506 34 -> 508 34 -> 508 25 -> 508 34 -> 510 34 -> 510 32 -> 510 34 -> 512 34 -> 512 33 -> 512 34)
    (474 110 -> 474 103 -> 474 110 -> 476 110 -> 476 103 -> 476 110 -> 478 110 -> 478 108 -> 478 110 -> 480 110 -> 480 100 -> 480 110 -> 482 110 -> 482 103 -> 482 110 -> 484 110 -> 484 104 -> 484 110 -> 486 110 -> 486 103 -> 486 110 -> 488 110 -> 488 101 -> 488 110)
    (502 34 -> 502 33 -> 502 34 -> 504 34 -> 504 31 -> 504 34 -> 506 34 -> 506 27 -> 506 34 -> 508 34 -> 508 25 -> 508 34 -> 510 34 -> 510 32 -> 510 34 -> 512 34 -> 512 33 -> 512 34)
    (506 61 -> 506 57 -> 506 61 -> 508 61 -> 508 56 -> 508 61 -> 510 61 -> 510 55 -> 510 61 -> 512 61 -> 512 56 -> 512 61 -> 514 61 -> 514 56 -> 514 61 -> 516 61 -> 516 60 -> 516 61 -> 518 61 -> 518 55 -> 518 61)
    (456 125 -> 456 128 -> 453 128 -> 453 134 -> 462 134 -> 462 128 -> 460 128 -> 460 125)
    (493 78 -> 497 78)
    (444 147 -> 444 142 -> 444 147 -> 446 147 -> 446 142 -> 446 147 -> 448 147 -> 448 140 -> 448 147 -> 450 147 -> 450 137 -> 450 147 -> 452 147 -> 452 138 -> 452 147 -> 454 147 -> 454 142 -> 454 147 -> 456 147 -> 456 145 -> 456 147 -> 458 147 -> 458 140 -> 458 147)
    (474 110 -> 474 103 -> 474 110 -> 476 110 -> 476 103 -> 476 110 -> 478 110 -> 478 108 -> 478 110 -> 480 110 -> 480 100 -> 480 110 -> 482 110 -> 482 103 -> 482 110 -> 484 110 -> 484 104 -> 484 110 -> 486 110 -> 486 103 -> 486 110 -> 488 110 -> 488 101 -> 488 110)
    (506 61 -> 506 57 -> 506 61 -> 508 61 -> 508 56 -> 508 61 -> 510 61 -> 510 55 -> 510 61 -> 512 61 -> 512 56 -> 512 61 -> 514 61 -> 514 56 -> 514 61 -> 516 61 -> 516 60 -> 516 61 -> 518 61 -> 518 55 -> 518 61)
    (483 48 -> 487 48)
    (484 81 -> 484 84 -> 481 84 -> 481 88 -> 492 88 -> 492 84 -> 488 84 -> 488 81)
    (498 13 -> 498 17 -> 491 17 -> 491 21 -> 507 21 -> 507 17 -> 502 17 -> 502 13)
    (465 115 -> 469 115)
    (444 147 -> 444 142 -> 444 147 -> 446 147 -> 446 142 -> 446 147 -> 448 147 -> 448 140 -> 448 147 -> 450 147 -> 450 137 -> 450 147 -> 452 147 -> 452 138 -> 452 147 -> 454 147 -> 454 142 -> 454 147 -> 456 147 -> 456 145 -> 456 147 -> 458 147 -> 458 140 -> 458 147)
    (474 110 -> 474 103 -> 474 110 -> 476 110 -> 476 103 -> 476 110 -> 478 110 -> 478 108 -> 478 110 -> 480 110 -> 480 100 -> 480 110 -> 482 110 -> 482 103 -> 482 110 -> 484 110 -> 484 104 -> 484 110 -> 486 110 -> 486 103 -> 486 110 -> 488 110 -> 488 101 -> 488 110)
    (444 147 -> 444 142 -> 444 147 -> 446 147 -> 446 142 -> 446 147 -> 448 147 -> 448 140 -> 448 147 -> 450 147 -> 450 137 -> 450 147 -> 452 147 -> 452 138 -> 452 147 -> 454 147 -> 454 142 -> 454 147 -> 456 147 -> 456 145 -> 456 147 -> 458 147 -> 458 140 -> 458 147)
    (474 110 -> 474 103 -> 474 110 -> 476 110 -> 476 103 -> 476 110 -> 478 110 -> 478 108 -> 478 110 -> 480 110 -> 480 100 -> 480 110 -> 482 110 -> 482 103 -> 482 110 -> 484 110 -> 484 104 -> 484 110 -> 486 110 -> 486 103 -> 486 110 -> 488 110 -> 488 101 -> 488 110)
    (474 110 -> 474 103 -> 474 110 -> 476 110 -> 476 103 -> 476 110 -> 478 110 -> 478 108 -> 478 110 -> 480 110 -> 480 100 -> 480 110 -> 482 110 -> 482 103 -> 482 110 -> 484 110 -> 484 104 -> 484 110 -> 486 110 -> 486 103 -> 486 110 -> 488 110 -> 488 101 -> 488 110)
    (462 117 -> 466 117)
    (471 115 -> 475 115)
    (444 147 -> 444 142 -> 444 147 -> 446 147 -> 446 142 -> 446 147 -> 448 147 -> 448 140 -> 448 147 -> 450 147 -> 450 137 -> 450 147 -> 452 147 -> 452 138 -> 452 147 -> 454 147 -> 454 142 -> 454 147 -> 456 147 -> 456 145 -> 456 147 -> 458 147 -> 458 140 -> 458 147)
    (456 125 -> 456 128 -> 453 128 -> 453 134 -> 462 134 -> 462 128 -> 460 128 -> 460 125)
    (498 13 -> 498 17 -> 491 17 -> 491 21 -> 507 21 -> 507 17 -> 502 17 -> 502 13)
    (499 78 -> 503 78)
    (474 110 -> 474 103 -> 474 110 -> 476 110 -> 476 103 -> 476 110 -> 478 110 -> 478 108 -> 478 110 -> 480 110 -> 480 100 -> 480 110 -> 482 110 -> 482 103 -> 482 110 -> 484 110 -> 484 104 -> 484 110 -> 486 110 -> 486 103 -> 486 110 -> 488 110 -> 488 101 -> 488 110)
    (444 147 -> 444 142 -> 444 147 -> 446 147 -> 446 142 -> 446 147 -> 448 147 -> 448 140 -> 448 147 -> 450 147 -> 450 137 -> 450 147 -> 452 147 -> 452 138 -> 452 147 -> 454 147 -> 454 142 -> 454 147 -> 456 147 -> 456 145 -> 456 147 -> 458 147 -> 458 140 -> 458 147)
    (506 61 -> 506 57 -> 506 61 -> 508 61 -> 508 56 -> 508 61 -> 510 61 -> 510 55 -> 510 61 -> 512 61 -> 512 56 -> 512 61 -> 514 61 -> 514 56 -> 514 61 -> 516 61 -> 516 60 -> 516 61 -> 518 61 -> 518 55 -> 518 61)
    (503 64 -> 503 67 -> 496 67 -> 496 71 -> 512 71 -> 512 67 -> 508 67 -> 508 64)
    (450 156 -> 454 156)
    (506 61 -> 506 57 -> 506 61 -> 508 61 -> 508 56 -> 508 61 -> 510 61 -> 510 55 -> 510 61 -> 512 61 -> 512 56 -> 512 61 -> 514 61 -> 514 56 -> 514 61 -> 516 61 -> 516 60 -> 516 61 -> 518 61 -> 518 55 -> 518 61)
    (498 13 -> 498 17 -> 491 17 -> 491 21 -> 507 21 -> 507 17 -> 502 17 -> 502 13)
    (474 110 -> 474 103 -> 474 110 -> 476 110 -> 476 103 -> 476 110 -> 478 110 -> 478 108 -> 478 110 -> 480 110 -> 480 100 -> 480 110 -> 482 110 -> 482 103 -> 482 110 -> 484 110 -> 484 104 -> 484 110 -> 486 110 -> 486 103 -> 486 110 -> 488 110 -> 488 101 -> 488 110)
    (495 48 -> 499 48)
    (474 110 -> 474 103 -> 474 110 -> 476 110 -> 476 103 -> 476 110 -> 478 110 -> 478 108 -> 478 110 -> 480 110 -> 480 100 -> 480 110 -> 482 110 -> 482 103 -> 482 110 -> 484 110 -> 484 104 -> 484 110 -> 486 110 -> 486 103 -> 486 110 -> 488 110 -> 488 101 -> 488 110)
    (498 46 -> 502 46)
    (474 110 -> 474 103 -> 474 110 -> 476 110 -> 476 103 -> 476 110 -> 478 110 -> 478 108 -> 478 110 -> 480 110 -> 480 100 -> 480 110 -> 482 110 -> 482 103 -> 482 110 -> 484 110 -> 484 104 -> 484 110 -> 486 110 -> 486 103 -> 486 110 -> 488 110 -> 488 101 -> 488 110)
    (487 78 -> 491 78)
    (506 61 -> 506 57 -> 506 61 -> 508 61 -> 508 56 -> 508 61 -> 510 61 -> 510 55 -> 510 61 -> 512 61 -> 512 56 -> 512 61 -> 514 61 -> 514 56 -> 514 61 -> 516 61 -> 516 60 -> 516 61 -> 518 61 -> 518 55 -> 518 61)
    (495 44 -> 499 44)
    (474 110 -> 474 103 -> 474 110 -> 476 110 -> 476 103 -> 476 110 -> 478 110 -> 478 108 -> 478 110 -> 480 110 -> 480 100 -> 480 110 -> 482 110 -> 482 103 -> 482 110 -> 484 110 -> 484 104 -> 484 110 -> 486 110 -> 486 103 -> 486 110 -> 488 110 -> 488 101 -> 488 110)
    (486 46 -> 490 46)
    (483 97 -> 492 97 -> 492 96)
    (502 34 -> 502 33 -> 502 34 -> 504 34 -> 504 31 -> 504 34 -> 506 34 -> 506 27 -> 506 34 -> 508 34 -> 508 25 -> 508 34 -> 510 34 -> 510 32 -> 510 34 -> 512 34 -> 512 33 -> 512 34)
    (506 61 -> 506 57 -> 506 61 -> 508 61 -> 508 56 -> 508 61 -> 510 61 -> 510 55 -> 510 61 -> 512 61 -> 512 56 -> 512 61 -> 514 61 -> 514 56 -> 514 61 -> 516 61 -> 516 60 -> 516 61 -> 518 61 -> 518 55 -> 518 61)
    (468 117 -> 472 117)
    (444 147 -> 444 142 -> 444 147 -> 446 147 -> 446 142 -> 446 147 -> 448 147 -> 448 140 -> 448 147 -> 450 147 -> 450 137 -> 450 147 -> 452 147 -> 452 138 -> 452 147 -> 454 147 -> 454 142 -> 454 147 -> 456 147 -> 456 145 -> 456 147 -> 458 147 -> 458 140 -> 458 147)
    (474 110 -> 474 103 -> 474 110 -> 476 110 -> 476 103 -> 476 110 -> 478 110 -> 478 108 -> 478 110 -> 480 110 -> 480 100 -> 480 110 -> 482 110 -> 482 103 -> 482 110 -> 484 110 -> 484 104 -> 484 110 -> 486 110 -> 486 103 -> 486 110 -> 488 110 -> 488 101 -> 488 110)
    (484 81 -> 484 84 -> 481 84 -> 481 88 -> 492 88 -> 492 84 -> 488 84 -> 488 81)
    (444 152 -> 448 152)
    (489 48 -> 493 48)
    (492 42 -> 496 42)
    (444 147 -> 444 142 -> 444 147 -> 446 147 -> 446 142 -> 446 147 -> 448 147 -> 448 140 -> 448 147 -> 450 147 -> 450 137 -> 450 147 -> 452 147 -> 452 138 -> 452 147 -> 454 147 -> 454 142 -> 454 147 -> 456 147 -> 456 145 -> 456 147 -> 458 147 -> 458 140 -> 458 147)
    (474 110 -> 474 103 -> 474 110 -> 476 110 -> 476 103 -> 476 110 -> 478 110 -> 478 108 -> 478 110 -> 480 110 -> 480 100 -> 480 110 -> 482 110 -> 482 103 -> 482 110 -> 484 110 -> 484 104 -> 484 110 -> 486 110 -> 486 103 -> 486 110 -> 488 110 -> 488 101 -> 488 110)
    (484 81 -> 484 84 -> 481 84 -> 481 88 -> 492 88 -> 492 84 -> 488 84 -> 488 81)
    (496 76 -> 500 76)
    (503 64 -> 503 67 -> 496 67 -> 496 71 -> 512 71 -> 512 67 -> 508 67 -> 508 64)
    (503 64 -> 503 67 -> 496 67 -> 496 71 -> 512 71 -> 512 67 -> 508 67 -> 508 64)
    (444 147 -> 444 142 -> 444 147 -> 446 147 -> 446 142 -> 446 147 -> 448 147 -> 448 140 -> 448 147 -> 450 147 -> 450 137 -> 450 147 -> 452 147 -> 452 138 -> 452 147 -> 454 147 -> 454 142 -> 454 147 -> 456 147 -> 456 145 -> 456 147 -> 458 147 -> 458 140 -> 458 147)
    (444 147 -> 444 142 -> 444 147 -> 446 147 -> 446 142 -> 446 147 -> 448 147 -> 448 140 -> 448 147 -> 450 147 -> 450 137 -> 450 147 -> 452 147 -> 452 138 -> 452 147 -> 454 147 -> 454 142 -> 454 147 -> 456 147 -> 456 145 -> 456 147 -> 458 147 -> 458 140 -> 458 147)
    (506 61 -> 506 57 -> 506 61 -> 508 61 -> 508 56 -> 508 61 -> 510 61 -> 510 55 -> 510 61 -> 512 61 -> 512 56 -> 512 61 -> 514 61 -> 514 56 -> 514 61 -> 516 61 -> 516 60 -> 516 61 -> 518 61 -> 518 55 -> 518 61)
    (456 125 -> 456 128 -> 453 128 -> 453 134 -> 462 134 -> 462 128 -> 460 128 -> 460 125)
    (435 154 -> 439 154)
    (503 64 -> 503 67 -> 496 67 -> 496 71 -> 512 71 -> 512 67 -> 508 67 -> 508 64)
    (441 154 -> 445 154)
    (474 110 -> 474 103 -> 474 110 -> 476 110 -> 476 103 -> 476 110 -> 478 110 -> 478 108 -> 478 110 -> 480 110 -> 480 100 -> 480 110 -> 482 110 -> 482 103 -> 482 110 -> 484 110 -> 484 104 -> 484 110 -> 486 110 -> 486 103 -> 486 110 -> 488 110 -> 488 101 -> 488 110)
    (447 154 -> 451 154)
    (459 121 -> 459 122 -> 470 122 -> 470 121)
    (444 147 -> 444 142 -> 444 147 -> 446 147 -> 446 142 -> 446 147 -> 448 147 -> 448 140 -> 448 147 -> 450 147 -> 450 137 -> 450 147 -> 452 147 -> 452 138 -> 452 147 -> 454 147 -> 454 142 -> 454 147 -> 456 147 -> 456 145 -> 456 147 -> 458 147 -> 458 140 -> 458 147)
    (502 34 -> 502 33 -> 502 34 -> 504 34 -> 504 31 -> 504 34 -> 506 34 -> 506 27 -> 506 34 -> 508 34 -> 508 25 -> 508 34 -> 510 34 -> 510 32 -> 510 34 -> 512 34 -> 512 33 -> 512 34)
    (474 110 -> 474 103 -> 474 110 -> 476 110 -> 476 103 -> 476 110 -> 478 110 -> 478 108 -> 478 110 -> 480 110 -> 480 100 -> 480 110 -> 482 110 -> 482 103 -> 482 110 -> 484 110 -> 484 104 -> 484 110 -> 486 110 -> 486 103 -> 486 110 -> 488 110 -> 488 101 -> 488 110)
    (474 110 -> 474 103 -> 474 110 -> 476 110 -> 476 103 -> 476 110 -> 478 110 -> 478 108 -> 478 110 -> 480 110 -> 480 100 -> 480 110 -> 482 110 -> 482 103 -> 482 110 -> 484 110 -> 484 104 -> 484 110 -> 486 110 -> 486 103 -> 486 110 -> 488 110 -> 488 101 -> 488 110)
    (506 61 -> 506 57 -> 506 61 -> 508 61 -> 508 56 -> 508 61 -> 510 61 -> 510 55 -> 510 61 -> 512 61 -> 512 56 -> 512 61 -> 514 61 -> 514 56 -> 514 61 -> 516 61 -> 516 60 -> 516 61 -> 518 61 -> 518 55 -> 518 61)
    (502 34 -> 502 33 -> 502 34 -> 504 34 -> 504 31 -> 504 34 -> 506 34 -> 506 27 -> 506 34 -> 508 34 -> 508 25 -> 508 34 -> 510 34 -> 510 32 -> 510 34 -> 512 34 -> 512 33 -> 512 34)
    (444 147 -> 444 142 -> 444 147 -> 446 147 -> 446 142 -> 446 147 -> 448 147 -> 448 140 -> 448 147 -> 450 147 -> 450 137 -> 450 147 -> 452 147 -> 452 138 -> 452 147 -> 454 147 -> 454 142 -> 454 147 -> 456 147 -> 456 145 -> 456 147 -> 458 147 -> 458 140 -> 458 147)
    (506 61 -> 506 57 -> 506 61 -> 508 61 -> 508 56 -> 508 61 -> 510 61 -> 510 55 -> 510 61 -> 512 61 -> 512 56 -> 512 61 -> 514 61 -> 514 56 -> 514 61 -> 516 61 -> 516 60 -> 516 61 -> 518 61 -> 518 55 -> 518 61)
    (474 110 -> 474 103 -> 474 110 -> 476 110 -> 476 103 -> 476 110 -> 478 110 -> 478 108 -> 478 110 -> 480 110 -> 480 100 -> 480 110 -> 482 110 -> 482 103 -> 482 110 -> 484 110 -> 484 104 -> 484 110 -> 486 110 -> 486 103 -> 486 110 -> 488 110 -> 488 101 -> 488 110)
    (495 39 -> 511 39 -> 511 38)
    (506 61 -> 506 57 -> 506 61 -> 508 61 -> 508 56 -> 508 61 -> 510 61 -> 510 55 -> 510 61 -> 512 61 -> 512 56 -> 512 61 -> 514 61 -> 514 56 -> 514 61 -> 516 61 -> 516 60 -> 516 61 -> 518 61 -> 518 55 -> 518 61)
    (493 74 -> 497 74)
    (444 147 -> 444 142 -> 444 147 -> 446 147 -> 446 142 -> 446 147 -> 448 147 -> 448 140 -> 448 147 -> 450 147 -> 450 137 -> 450 147 -> 452 147 -> 452 138 -> 452 147 -> 454 147 -> 454 142 -> 454 147 -> 456 147 -> 456 145 -> 456 147 -> 458 147 -> 458 140 -> 458 147)
    (474 110 -> 474 103 -> 474 110 -> 476 110 -> 476 103 -> 476 110 -> 478 110 -> 478 108 -> 478 110 -> 480 110 -> 480 100 -> 480 110 -> 482 110 -> 482 103 -> 482 110 -> 484 110 -> 484 104 -> 484 110 -> 486 110 -> 486 103 -> 486 110 -> 488 110 -> 488 101 -> 488 110)
    (502 34 -> 502 33 -> 502 34 -> 504 34 -> 504 31 -> 504 34 -> 506 34 -> 506 27 -> 506 34 -> 508 34 -> 508 25 -> 508 34 -> 510 34 -> 510 32 -> 510 34 -> 512 34 -> 512 33 -> 512 34)
    (502 34 -> 502 33 -> 502 34 -> 504 34 -> 504 31 -> 504 34 -> 506 34 -> 506 27 -> 506 34 -> 508 34 -> 508 25 -> 508 34 -> 510 34 -> 510 32 -> 510 34 -> 512 34 -> 512 33 -> 512 34)
    (484 81 -> 484 84 -> 481 84 -> 481 88 -> 492 88 -> 492 84 -> 488 84 -> 488 81)
    (492 46 -> 496 46)
    (506 61 -> 506 57 -> 506 61 -> 508 61 -> 508 56 -> 508 61 -> 510 61 -> 510 55 -> 510 61 -> 512 61 -> 512 56 -> 512 61 -> 514 61 -> 514 56 -> 514 61 -> 516 61 -> 516 60 -> 516 61 -> 518 61 -> 518 55 -> 518 61)
    (506 61 -> 506 57 -> 506 61 -> 508 61 -> 508 56 -> 508 61 -> 510 61 -> 510 55 -> 510 61 -> 512 61 -> 512 56 -> 512 61 -> 514 61 -> 514 56 -> 514 61 -> 516 61 -> 516 60 -> 516 61 -> 518 61 -> 518 55 -> 518 61)
    (444 147 -> 444 142 -> 444 147 -> 446 147 -> 446 142 -> 446 147 -> 448 147 -> 448 140 -> 448 147 -> 450 147 -> 450 137 -> 450 147 -> 452 147 -> 452 138 -> 452 147 -> 454 147 -> 454 142 -> 454 147 -> 456 147 -> 456 145 -> 456 147 -> 458 147 -> 458 140 -> 458 147)
    (502 34 -> 502 33 -> 502 34 -> 504 34 -> 504 31 -> 504 34 -> 506 34 -> 506 27 -> 506 34 -> 508 34 -> 508 25 -> 508 34 -> 510 34 -> 510 32 -> 510 34 -> 512 34 -> 512 33 -> 512 34)
    (474 110 -> 474 103 -> 474 110 -> 476 110 -> 476 103 -> 476 110 -> 478 110 -> 478 108 -> 478 110 -> 480 110 -> 480 100 -> 480 110 -> 482 110 -> 482 103 -> 482 110 -> 484 110 -> 484 104 -> 484 110 -> 486 110 -> 486 103 -> 486 110 -> 488 110 -> 488 101 -> 488 110)
    (444 156 -> 448 156)
    (459 121 -> 459 122 -> 470 122 -> 470 121)
    (459 121 -> 459 122 -> 470 122 -> 470 121)
    (438 156 -> 442 156)
    (490 76 -> 494 76)
    (444 147 -> 444 142 -> 444 147 -> 446 147 -> 446 142 -> 446 147 -> 448 147 -> 448 140 -> 448 147 -> 450 147 -> 450 137 -> 450 147 -> 452 147 -> 452 138 -> 452 147 -> 454 147 -> 454 142 -> 454 147 -> 456 147 -> 456 145 -> 456 147 -> 458 147 -> 458 140 -> 458 147)
    (502 34 -> 502 33 -> 502 34 -> 504 34 -> 504 31 -> 504 34 -> 506 34 -> 506 27 -> 506 34 -> 508 34 -> 508 25 -> 508 34 -> 510 34 -> 510 32 -> 510 34 -> 512 34 -> 512 33 -> 512 34)
    (444 147 -> 444 142 -> 444 147 -> 446 147 -> 446 142 -> 446 147 -> 448 147 -> 448 140 -> 448 147 -> 450 147 -> 450 137 -> 450 147 -> 452 147 -> 452 138 -> 452 147 -> 454 147 -> 454 142 -> 454 147 -> 456 147 -> 456 145 -> 456 147 -> 458 147 -> 458 140 -> 458 147)
    (432 156 -> 436 156)
    (506 61 -> 506 57 -> 506 61 -> 508 61 -> 508 56 -> 508 61 -> 510 61 -> 510 55 -> 510 61 -> 512 61 -> 512 56 -> 512 61 -> 514 61 -> 514 56 -> 514 61 -> 516 61 -> 516 60 -> 516 61 -> 518 61 -> 518 55 -> 518 61)
    (456 125 -> 456 128 -> 453 128 -> 453 134 -> 462 134 -> 462 128 -> 460 128 -> 460 125)
    (489 44 -> 493 44)
    (483 97 -> 492 97 -> 492 96)
    (503 64 -> 503 67 -> 496 67 -> 496 71 -> 512 71 -> 512 67 -> 508 67 -> 508 64)
    (484 81 -> 484 84 -> 481 84 -> 481 88 -> 492 88 -> 492 84 -> 488 84 -> 488 81)
    (484 81 -> 484 84 -> 481 84 -> 481 88 -> 492 88 -> 492 84 -> 488 84 -> 488 81)
    (474 110 -> 474 103 -> 474 110 -> 476 110 -> 476 103 -> 476 110 -> 478 110 -> 478 108 -> 478 110 -> 480 110 -> 480 100 -> 480 110 -> 482 110 -> 482 103 -> 482 110 -> 484 110 -> 484 104 -> 484 110 -> 486 110 -> 486 103 -> 486 110 -> 488 110 -> 488 101 -> 488 110)
    (484 81 -> 484 84 -> 481 84 -> 481 88 -> 492 88 -> 492 84 -> 488 84 -> 488 81)
    (456 125 -> 456 128 -> 453 128 -> 453 134 -> 462 134 -> 462 128 -> 460 128 -> 460 125)
    (506 61 -> 506 57 -> 506 61 -> 508 61 -> 508 56 -> 508 61 -> 510 61 -> 510 55 -> 510 61 -> 512 61 -> 512 56 -> 512 61 -> 514 61 -> 514 56 -> 514 61 -> 516 61 -> 516 60 -> 516 61 -> 518 61 -> 518 55 -> 518 61)
    (503 64 -> 503 67 -> 496 67 -> 496 71 -> 512 71 -> 512 67 -> 508 67 -> 508 64)
    (502 34 -> 502 33 -> 502 34 -> 504 34 -> 504 31 -> 504 34 -> 506 34 -> 506 27 -> 506 34 -> 508 34 -> 508 25 -> 508 34 -> 510 34 -> 510 32 -> 510 34 -> 512 34 -> 512 33 -> 512 34)
    (444 147 -> 444 142 -> 444 147 -> 446 147 -> 446 142 -> 446 147 -> 448 147 -> 448 140 -> 448 147 -> 450 147 -> 450 137 -> 450 147 -> 452 147 -> 452 138 -> 452 147 -> 454 147 -> 454 142 -> 454 147 -> 456 147 -> 456 145 -> 456 147 -> 458 147 -> 458 140 -> 458 147)
    (474 110 -> 474 103 -> 474 110 -> 476 110 -> 476 103 -> 476 110 -> 478 110 -> 478 108 -> 478 110 -> 480 110 -> 480 100 -> 480 110 -> 482 110 -> 482 103 -> 482 110 -> 484 110 -> 484 104 -> 484 110 -> 486 110 -> 486 103 -> 486 110 -> 488 110 -> 488 101 -> 488 110)
    (498 13 -> 498 17 -> 491 17 -> 491 21 -> 507 21 -> 507 17 -> 502 17 -> 502 13)
    (501 48 -> 505 48)
    (444 147 -> 444 142 -> 444 147 -> 446 147 -> 446 142 -> 446 147 -> 448 147 -> 448 140 -> 448 147 -> 450 147 -> 450 137 -> 450 147 -> 452 147 -> 452 138 -> 452 147 -> 454 147 -> 454 142 -> 454 147 -> 456 147 -> 456 145 -> 456 147 -> 458 147 -> 458 140 -> 458 147)))
