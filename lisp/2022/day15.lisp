(defun get-data (input)
  (loop for (sensor at-1 x x-val y y-val closest beacon is at-2 bx bx-val by by-val) in input
        for distance = (get-manhattan-distances x-val y-val bx-val by-val)
        collect (acons 'dist distance (acons 'data (list x-val y-val bx-val by-val) nil))
          into output
        minimize x-val into x-min
        minimize bx-val into bx-min
        maximize x-val into x-max
        maximize bx-val into bx-max
        minimize y-val into y-min
        minimize by-val into by-min
        maximize y-val into y-max
        maximize by-val into by-max
        minimize distance into dist-min
        maximize distance into dist-max
        finally
           (return
             (acons
              'output output
              (acons
               'x-min x-min
               (acons
                'x-max x-max
                (acons
                 'bx-min bx-min
                 (acons
                  'bx-max bx-max
                  (acons
                   'y-min y-min
                   (acons
                    'y-max y-max
                    (acons
                     'by-min by-min
                     (acons
                      'by-max by-max
                      (acons
                       'dist-min dist-min
                       (acons 'dist-max dist-max nil))))))))))))))

(defun can-be-beacon (x y data)
  (loop for (dist (data-key xc yc xb yb)) in data
        for m-dist = (get-manhattan-distances x y xc yc)
        when (and (= x xb) (= y yb))
          do (return (list t 'beacon))
        when (>= (cdr dist) m-dist)
          do (return (list nil m-dist (cdr dist) xc yc x y))
        finally (return (list t))))

(defun get-manhattan-distances (xa ya xb yb)
  (+ (abs (- xb xa)) (abs (- yb ya))))

(defun find-all-shorter (input y-pos)
  (loop with data = (get-data input)
        with dist-max = (cdr (assoc 'dist-max data))
        with output = (cdr (assoc 'output data))
        for x from (- (cdr (assoc 'x-min data)) dist-max) to (+ (cdr (assoc 'x-max data)) dist-max)
         for verdict = (can-be-beacon x y-pos output)
          count (not (car verdict))))

(defun range-at (x y dist at-y)
  (let ((x-at-distance (abs (- y at-y))))
    (if (> x-at-distance dist) nil
        (list (- x (- dist x-at-distance)) (+ x (- dist x-at-distance))))))

(defun sort-ranges (list)
  (sort (loop for i in list
              unless (null i)
                collect i) #'(lambda (x y) (< (car x) (car y)))))

(defun find-range-gap (list)
  (loop for ((x1 x2) (x3 x4)) on (sort-ranges list)
        for bigest = x2 then (if (> x2 bigest) x2 bigest)
        when (and (not (null x3))
                  (> (- x3 bigest) 1))
          do (return (+ bigest (- x3 bigest 1)))))

(defun traverse-for-gaps (input x-max)
  (loop with data = (get-data input)
        with output = (cdr (assoc 'output data))
        for x from 1 to x-max
        for is-gap = (find-range-gap (make-ranges-at x output))
        when is-gap
          do (return (+ x (* 4000000 is-gap)))))

(defun make-ranges-at (at output)
  (loop for (dist (data x y &optional rest)) in output
        collect (range-at x y (cdr dist) at)))

(defun day15-part1 ()
  (assert (equal 5125700 (find-all-shorter (input) 2000000))))

(defun day15-part2 ()
  (assert (equal 11379394658764 (traverse-for-gaps (input) 4000000))))

(defun input ()
  '(
    (Sensor at x 98246 y 1908027 closest beacon is at x 1076513 y 2000000)
    (Sensor at x 1339369 y 2083853 closest beacon is at x 1076513 y 2000000)
    (Sensor at x 679177 y 3007305 closest beacon is at x 1076513 y 2000000)
    (Sensor at x 20262 y 3978297 closest beacon is at x 13166 y 4136840)
    (Sensor at x 3260165 y 2268955 closest beacon is at x 4044141 y 2290104)
    (Sensor at x 2577675 y 3062584 closest beacon is at x 2141091 y 2828176)
    (Sensor at x 3683313 y 2729137 closest beacon is at x 4044141 y 2290104)
    (Sensor at x 1056412 y 370641 closest beacon is at x 1076513 y 2000000)
    (Sensor at x 2827280 y 1827095 closest beacon is at x 2757345 y 1800840)
    (Sensor at x 1640458 y 3954524 closest beacon is at x 2141091 y 2828176)
    (Sensor at x 2139884 y 1162189 closest beacon is at x 2757345 y 1800840)
    (Sensor at x 3777450 y 3714504 closest beacon is at x 3355953 y 3271922)
    (Sensor at x 1108884 y 2426713 closest beacon is at x 1076513 y 2000000)
    (Sensor at x 2364307 y 20668 closest beacon is at x 2972273 y -494417)
    (Sensor at x 3226902 y 2838842 closest beacon is at x 3355953 y 3271922)
    (Sensor at x 22804 y 3803886 closest beacon is at x 13166 y 4136840)
    (Sensor at x 2216477 y 2547945 closest beacon is at x 2141091 y 2828176)
    (Sensor at x 1690953 y 2203555 closest beacon is at x 1076513 y 2000000)
    (Sensor at x 3055156 y 3386812 closest beacon is at x 3355953 y 3271922)
    (Sensor at x 3538996 y 719130 closest beacon is at x 2972273 y -494417)
    (Sensor at x 2108918 y 2669413 closest beacon is at x 2141091 y 2828176)
    (Sensor at x 3999776 y 2044283 closest beacon is at x 4044141 y 2290104)
    (Sensor at x 2184714 y 2763072 closest beacon is at x 2141091 y 2828176)
    (Sensor at x 2615462 y 2273553 closest beacon is at x 2757345 y 1800840)))
