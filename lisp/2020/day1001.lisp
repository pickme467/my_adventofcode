(defun day-10-2020-1 ()
  (jolt-difference (input)))

(defun day-10-2020-2 ()
  (reduce #'* (mapcar #'count-to-combinations-of-three (count-it (jolt-difference-list (input))))))

(defun jolt-difference (input)
  (loop for (a b) on (sort (copy-list input) #'<)
        unless (null b)
          count (= 3 (- b a)) into threes
          and count (= 1 (- b a)) into ones
        finally (return (* (1+ threes) (1+ ones)))))

(defun jolt-difference-list (input)
  (let* ((to-loop (concatenate 'list '(0) (sort (copy-list input) #'<))))
    (loop for (a b) on to-loop
          unless (null b)
            collecting (- b a) into list
          finally (return list))))

(defun count-it (list)
  (loop for i in list
        count (= i 1) into ones
        when (= i 3)
          when (> ones 0)
            collect ones into ones-count and do (setf ones 0)
        finally
           (if (> ones 0) (return (append ones-count (list ones)))) (return ones-count)))

(defun count-to-combinations-of-three (x)
  (if (> x 3) (1- (expt 2 (1- x))) (expt 2 (1- x))))

(defun input ()
  '(26
    97
    31
    7
    2
    10
    46
    38
    112
    54
    30
    93
    18
    111
    29
    75
    139
    23
    132
    85
    78
    99
    8
    113
    87
    57
    133
    41
    104
    98
    58
    90
    13
    91
    20
    68
    103
    127
    105
    114
    138
    126
    67
    32
    145
    115
    16
    141
    1
    73
    45
    119
    51
    40
    35
    150
    118
    53
    80
    79
    65
    135
    74
    47
    128
    64
    17
    4
    84
    83
    147
    142
    146
    9
    125
    94
    140
    131
    134
    92
    66
    122
    19
    86
    50
    52
    108
    100
    71
    61
    44
    39
    3
    72))

(defun example ()
  '(28
    33
    18
    42
    31
    14
    46
    20
    48
    47
    24
    23
    49
    45
    19
    38
    39
    11
    1
    32
    25
    35
    8
    17
    7
    9
    4
    2
    34
    10
    3))

(defun example-1 ()
  '(16
    10
    15
    5
    1
    11
    7
    19
    6
    12
    4))
