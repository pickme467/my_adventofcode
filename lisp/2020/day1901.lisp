(defun day-19-2020-1 ()
  (words-check (input)))

(defun day-19-2020-2 ()
  (words-check (input-2)))

(defun words-check (input)
  (let ((hash (make-hash input)))
    (loop for i in (second (parse-input input)) count (equal 'correct (run-for-word i '(0) hash)))))

(defun run-for-word (word rule hash)
  (loop for character across word
        for letter = (string character)
        for next-rule = (run-steps letter rule hash) then (run-steps letter (second next-rule) hash)
        when (equal next-rule 'incorrect)
          return (list word 'incorrect)
        finally (return (values (assess-correctness next-rule) word))))

(defun assess-correctness (rule)
  (if (equal rule '(correct nil)) (return-from assess-correctness 'correct))
  (cond ((and (is-or (second rule))
              (or (null (second (second rule)))
                  (null (third (second rule)))))
         'correct)
        ((is-or (second rule))
               (cond ((or (equal 'correct (assess-correctness (list 'correct (second (second rule)))))
                          (equal 'correct (assess-correctness (list 'correct (third (second rule))))))
                      'correct)
                     (t 'incorrect)))
        (t 'incorrect)))

(defun run-steps (letter rule hash)
  (loop for r = rule then (one-step letter r hash)
        when (or (equal 'incorrect r)
                 (equal (first r) 'correct))
          return r))

(defun one-step (letter rule hash)
  (let ((step (first rule)))
    (cond ((equal step letter) (list 'correct (rest rule)))
          ((integerp step)
           (let ((next (gethash step hash)))
             (cond ((is-or next)
                    (list 'or
                          (append (second next) (rest rule))
                          (append (third next) (rest rule))))
                   (t (append (gethash step hash) (rest rule))))))
          ((is-or rule)
           (cond
             ((equal 'incorrect (second rule))
              (third rule))
             ((equal 'incorrect (third rule))
              (second rule))
             ((and (equal 'correct (first (second rule)))
                   (equal 'correct (first (third rule))))
              (list 'correct (append '(or) (append (rest (second rule)) (rest (third rule))))))
             ((equal 'correct (first (second rule)))
              (list 'or (second rule) (one-step letter (third rule) hash)))
             ((equal 'correct (first (third rule)))
              (list  'or (third rule) (one-step letter (second rule) hash)))
             (t (list 'or
                      (one-step letter (second rule) hash)
                      (one-step letter (third rule) hash)))))
          (t 'incorrect))))

(defun make-hash (input)
  (let ((hash (make-hash-table :test #'equal)))
    (loop for i in (first input)
          do (destructuring-bind (rule arrow &rest rest) i
               (declare (ignore arrow)) (setf (gethash rule hash) (make-rule rest ())))
          finally (return hash))))

(defun make-rule (list part)
  (unless list (return-from make-rule (reverse part)))
  (if (equal (first list) 'or) (return-from make-rule (list 'or (reverse part) (rest list))))
  (make-rule (rest list) (push (first list) part)))

(defun is-or (list)
  (and (listp list) (equal 'or (first list))))

(defun parse-input (input)
  (list (first input) (mapcar (lambda (s) (string-trim '(#\ ) s))
                              (uiop:split-string (second input) :separator (string #\linefeed)))))

(defun input ()
  '((
     (51 -> 52 129 or 86 30)
     (82 -> 52 97 or 86 16)
     (72 -> 75 52 or 85 86)
     (118 -> 28 86 or 62 52)
     (104 -> 23 52 or 9 86)
     (36 -> 52 23 or 86 62)
     (34 -> 86 113)
     (39 -> 52 56)
     (58 -> 116 86 or 28 52)
     (79 -> 9 86 or 54 52)
     (33 -> 28 115)
     (105 -> 75 52 or 83 86)
     (30 -> 27 52 or 46 86)
     (107 -> 51 52 or 112 86)
     (10 -> 86 102 or 52 36)
     (112 -> 86 87 or 52 103)
     (24 -> 49 86 or 37 52)
     (135 -> 15 86 or 28 52)
     (84 -> 3 52 or 132 86)
     (15 -> 52 52 or 86 52)
     (95 -> 86 18 or 52 94)
     (48 -> 40 86 or 63 52)
     (116 -> 115 115)
     (28 -> 86 52 or 52 86)
     (61 -> 22 52 or 41 86)
     (26 -> 116 86 or 75 52)
     (49 -> 78 52 or 65 86)
     (20 -> 86 55 or 52 34)
     (41 -> 86 116 or 52 15)
     (87 -> 39 86 or 47 52)
     (100 -> 23 52 or 116 86)
     (129 -> 86 58 or 52 81)
     (4 -> 86 86 or 86 52)
     (8 -> 42)
     (121 -> 116 86 or 9 52)
     (119 -> 86 56 or 52 4)
     (133 -> 52 73 or 86 44)
     (35 -> 52 96 or 86 116)
     (32 -> 86 92 or 52 1)
     (113 -> 86 86)
     (97 -> 83 86 or 113 52)
     (89 -> 96 52 or 56 86)
     (3 -> 86 5 or 52 118)
     (2 -> 86 122 or 52 80)
     (120 -> 23 52 or 96 86)
     (56 -> 52 86 or 86 86)
     (52 -> "b")
     (19 -> 86 83 or 52 15)
     (22 -> 52 116 or 86 4)
     (71 -> 52 85 or 86 54)
     (74 -> 86 56)
     (130 -> 52 75 or 86 23)
     (80 -> 52 72 or 86 7)
     (40 -> 98 86 or 38 52)
     (50 -> 52 124 or 86 60)
     (108 -> 52 75 or 86 85)
     (64 -> 125 86 or 26 52)
     (78 -> 86 56 or 52 28)
     (43 -> 52 81 or 86 46)
     (85 -> 52 52 or 52 86)
     (128 -> 90 52 or 121 86)
     (110 -> 85 86 or 68 52)
     (14 -> 52 117 or 86 69)
     (101 -> 4 52 or 113 86)
     (117 -> 52 50 or 86 99)
     (45 -> 52 95 or 86 114)
     (29 -> 52 116 or 86 56)
     (73 -> 86 110 or 52 17)
     (102 -> 52 62 or 86 116)
     (134 -> 52 127 or 86 77)
     (131 -> 86 54 or 52 4)
     (123 -> 61 86 or 10 52)
     (54 -> 86 115 or 52 52)
     (109 -> 52 76 or 86 14)
     (44 -> 47 86 or 36 52)
     (124 -> 52 56 or 86 113)
     (38 -> 52 130 or 86 105)
     (21 -> 28 86 or 116 52)
     (65 -> 86 62 or 52 9)
     (37 -> 35 52 or 81 86)
     (115 -> 86 or 52)
     (103 -> 86 22 or 52 111)
     (53 -> 86 6 or 52 134)
     (59 -> 52 91 or 86 19)
     (88 -> 86 84 or 52 12)
     (12 -> 86 43 or 52 32)
     (6 -> 86 82 or 52 59)
     (125 -> 86 62 or 52 75)
     (27 -> 28 52 or 23 86)
     (96 -> 52 86)
     (47 -> 62 52 or 62 86)
     (9 -> 52 115 or 86 52)
     (0 -> 8 11)
     (63 -> 128 52 or 20 86)
     (68 -> 86 115 or 52 86)
     (13 -> 86 4 or 52 75)
     (18 -> 110 86 or 104 52)
     (126 -> 88 86 or 107 52)
     (111 -> 115 4)
     (122 -> 29 52 or 71 86)
     (16 -> 56 52 or 75 86)
     (91 -> 23 86 or 23 52)
     (69 -> 67 86 or 64 52)
     (70 -> 53 86 or 48 52)
     (7 -> 52 113 or 86 15)
     (127 -> 135 52 or 131 86)
     (57 -> 17 86 or 34 52)
     (86 -> "a")
     (106 -> 52 62 or 86 56)
     (11 -> 42 31)
     (17 -> 52 56 or 86 23)
     (98 -> 52 13 or 86 119)
     (92 -> 86 15 or 52 23)
     (94 -> 33 86 or 100 52)
     (55 -> 86 54 or 52 9)
     (132 -> 101 52 or 108 86)
     (75 -> 86 86 or 52 52)
     (5 -> 52 83 or 86 113)
     (60 -> 116 86 or 96 52)
     (23 -> 52 52)
     (77 -> 86 130 or 52 74)
     (99 -> 124 52 or 89 86)
     (83 -> 115 86 or 52 52)
     (90 -> 62 52 or 9 86)
     (42 -> 109 86 or 66 52)
     (114 -> 52 57 or 86 93)
     (25 -> 52 133 or 86 123)
     (62 -> 86 52)
     (93 -> 52 79 or 86 120)
     (76 -> 86 24 or 52 2)
     (66 -> 86 25 or 52 45)
     (46 -> 9 115)
     (67 -> 86 106 or 52 21)
     (31 -> 52 70 or 86 126)
     (81 -> 85 86 or 15 52)
     (1 -> 116 52 or 62 86))
    "bbbababaabaabaabababbbabaababababbabbbbabbabaabababbbaabbbbaaababbbbbaabbaaaaaab
     ababbbbbaaabbbababaabaababaabaaabbbaaabaaaaaabbbabbbaabababbabab
     aaabbabbababaaababbaaabaaabbaaabbababbbb
     aaaababbbbbbaabbbbabaabaaaaaabbaabbbbbabaaabbbaabaabbbaaabaabbbbaaabaaaa
     aaababaabababbabababbaaa
     abababaaabbabbaaaaaabaaaaaabaaba
     bbabbbbbaaaabbbbaabbabbaabbbbbabaababbbababaaaab
     babaaabaaabaababbbaaaabb
     bbaababbbbabababbbabaabbbaaaaaabbabbabaaaaaaaabababbbaab
     ababbbbbaaaabaabaaaabbbabaabbaabbaaababbbaaaababaaababbababaabbbabaaabbabaaaaabbaaabbaaa
     baaabaabaaaaabbbbaaaaaba
     bbbbbbbbbabbbabaaaabbabb
     bbbbbbaabbbbbabbbabbbbab
     aaaaabbbbbbbbbaaaababbaaabaaaaaabbabaaaa
     aabbbaababbaaaabbabababaaaabbaaa
     bbabababbbbababbbaaababb
     babbbbbbbbbbaabaabbaabbbbaaabbabbaabbbbbabababbb
     aabbaaaaaaaaabbaababaaab
     babbabbaabbbbbabbaaabbaa
     aaabaaabaabaabbaaabababa
     abaabaabaaababbbbbbbbbbabbbaabbbabaaabba
     bbbbaabbbbabbbbbabbbbaba
     abaabbabbaaaabbaabaabbbb
     aaaabbbaabababaaaaabaaaa
     aaaaabaababaaaaabbababbaabaababaabbabaabababbbaabbbaabbbaaaabaaabbbaaaaaaabbbaaa
     aabbabbaaababaaababaaaaa
     ababbbaabbbbabbbaabaaaabaababbbaaaabbbaababbbbba
     abbbbbabbbaaaabaaaabbbaa
     bbbbaabaabbababaabababab
     baabaabbbbbbbbbbbbababaa
     bbabbbbaaaaaaaaaaabaabaaababbaaa
     abbaabbbabaaaaaabbabbababbbbaaababaaaaba
     abbbbbbababaaababbbbaaabaabbaabaabbaabbaaabbaaabbaaaabbb
     abbaaaaaabbaabbbaabaaaab
     abaababbbbbbbbaabaaaaaabaabbaaaabbbbbbbb
     aabbbbbbbaabbbababbbabab
     bbbbaabaaabaabbbaaabbabb
     aaaabbbbbbabaabaaabaaababbabbbaababaababbabbabbbbaaabbab
     babbabbabbaaaaaaababaaaabaaaabaabababbbabababbbb
     bbabaabbaabbaaaabababaab
     baaabaaabbabbbbbabbabaabbabbbbba
     aabaababbabbaabababaaabbaaabaabbbabbbbba
     aababaaaaaaabababbababaa
     abbaaaaabbbbbabbaabaaabb
     baabbbabbaaabbbbbaaaaaaa
     bbbbaabbbbabbababbbabbba
     aaaababaaabbbbbabbabbbbbbabbbabaabaababb
     abbababaabbaaaaabbaabbabbabababb
     babaaabbaabaabbbabbabbabbaaaabaabbbbaababbbbabbbaabaaaaabaabaaba
     bbaabababbaaabaaabbbabba
     bbbaaabbbabbaaaaaabbabaabbaabbbbaaabbbab
     baabbbababbaabbbbbbabbbb
     aabbbbaaabbaabaaabaabaaa
     aabbabbbaababaababaabaaabbabbaabbabaabbaaaababbbaabbaaaababababbbabbabbbaabbaabb
     bbaaabababbabbabbbbabbba
     bbbabaaaaaaababbaaabbbaa
     aababaababbabaababbabbba
     abbbbaaaaaabaaabaabbbaabbbabbbbbabaaabab
     baabbaababbbbbabbaababbaababbbba
     aabbbbbbbbbaaabaabbbaaab
     bbbbbbbabaababaaabaaabab
     baabbabbaaabababababbaab
     ababaabaaaaababbabbaabba
     aabbabbababaaabbbbbaaaaa
     abaaabbbbbaabbbbbabbbbababbaabba
     bbabbabbbbaaaababaaaabaaabaababbbaabbabb
     aababaabbabbabbaaabaaabababbaabb
     baaabbbbaaaabbbbabbaabba
     abaababababbbbabbaababababababaababbbbaaabbabbbaabbbbbbbbbbbaaba
     bbabbabbbabbaababbabbbbbabbaabaabbbbabaa
     bbaaaababbbbaabaaaababab
     aaaabbabaaaaabbbaabaabbaababaaab
     aaababbbaaaaaabbaabbabbb
     babbaaababaaaabaababbbbbbabbbaaaaaabaabaabaabbba
     babbabaaabaabaabbabbaabaabbaabbbbbabbbabbabbabbbbbbaabab
     bbbabbabbbbaabbaabbaaaab
     babbaaabbaabbbbbabbaabaababbbabb
     abbaabaaaababaaabbbababbabbbaaabababaabb
     aababaabbabbbaabbbbbaabaaaababababbaaaba
     baabbbbaaababbabaabbbbaaaabbabaaabbbaaab
     baabababbababbabaaaaaaba
     aabbaaaaabbababaaaaaaaba
     aabbabaaababaaaaaabbaaaabbbaaabbbababbbb
     baabababaabaaaabbaabaaaababbbabb
     babbaabaaabaaabaabaabbba
     aaaaaaababaabbabbabbbbab
     aabaabbbaaabbbbabababbba
     bbbbaaabbbaaabaaaabbbabb
     abbbbaaabaabbbbaaabaabaababbaabababbbaaaabababba
     abababbbabbabbbabaabbababbbaaaabbbaaaaabbbabbbbbbbabaabbaabbabbaaababbab
     bbabbbaaabbabaaabbaaabba
     bababbabababaabaaabbaaba
     baababbaaabbbabaaabaabba
     aaaaaaabbbabaabbbbbbbaaaabbbabbaaaabbbbb
     bbbabaaaabbabbabbabbabaabbabbbab
     bbbbbbbabaabbabbaabbbaab
     abbbbaaabbbababbbaababbaaaaabaabaaaaaaaaabbabbba
     abbaaabbbbabbaabababbbbbbabaabba
     aababbaabbaabbaabababbbb
     bbaaabaaaabbbbabbbbaabaa
     aabbabaaabbaabaababaabaa
     abbbabbbaaaabbaababaabba
     bbbababbbbabbabbabaaaababaaabaaaaaaabaaababaabbb
     aaaabbbbbbabbabaabbaabbbbbbaabbb
     bbaaaababaababbbbaabaaaa
     abbaabbbbbaabbaaabbaabab
     abbbbbabbbbaaabbaaaaabbbabbaabaaaabbbaaa
     abbaabbbaabbbababbaaaabb
     baababaaaaaaaaabbbbbbaba
     baabbbbababbbaabbbabbbab
     babbabaabbabbbbaabaaaabaabbbbaabbaabbaaaaaabbaba
     baaababaaaabbbbaaabbaababbaaabab
     aabbaabbbbaaaaaabaabbaaaabbbaaaaabbbbaaa
     aabaababaabbbaabbbbaaababbbbabaa
     abbababbaaaabbbaabbbbbbb
     abbabaaaaaaabaaaabaabbaa
     baabbababbabaabaaaabbbab
     abababbabbaabbbaababbbaaaaababbbbbabbbbbbaaabbbbaaabbaaabaaaabaa
     aaababaabbabbaaabbbbbaab
     bbbabbabaababbaababbbaabbabaabbb
     bababaaaaaaaabbabbabababaaaabaaaaabaaabbbbbabbaababbabab
     aababaaababbbaabaabaaabababaabaababbabbbbabaaaaabaaaaabbbaaaaaba
     aabbbbaabbaaaabababbaaabbaaaaabb
     abbababaabbaaabbbabbabaa
     aabbbbbabbbbbbbbaabbabbabbbbaaaabbbaabab
     aabbbbbbbbbbaabababbbbbabbbaababbababaab
     abaaabbaabaabbababbbbaabaaaabbabbaabbbaababaaabbbabaabba
     bbaaaabaaababbaaababbababbbaabba
     bbbbaababbaabbaabaabbabb
     abaabaaabbbabaabbbabbbbbbbbaababbaaaaaabbaabbaababaabbba
     bbabaabbababaabababaabab
     aabbbbbbabbabbaaabbbabaa
     bbabbaabaaaabbbaaaaaaaaabbbbabbabaabaaab
     abbabbabbababaaabaaaabab
     bbaaabaaabbababaaabbbaaa
     bbabaabaabbbbbaabaaaaabb
     aabbabbaabbbabbbaaabbabb
     bababaaababbaaabbbbbbbbaababbaab
     babbabbbbaaabbbbabbbabaabbbabbbaababbbabbaababbaaaabaabbaaababababaaabaaaaabbaaaabaaaaaa
     bbbbababbbabaabbaabbbaabaaababbbababaaaababbbbaaabbbbabb
     bbabaaabaababbbaabbbbbbabaaaabbbabbabaabaaababbbbabbbaaabbbabbba
     aaaabbbbaababaabbabaaabaaabaabbaaababbbb
     aabaabbbbababaaabbabbbba
     bababbaabbbbbbbabbbababa
     abbaabbbbbabbababbbbababaababbbb
     aaaaabbbabbbbaaababbbaaaabbbbaba
     aabaababbabaaabbbbaaabaaaaaaabbabaaaaababbbbaaaaaaabbbab
     bbaababaabbaabbbabaaaabb
     abbbbbabaababaabbaabbbaa
     aaaabbabbbabbaabbbbababbbaabbbba
     bbaababbbabbaaaabbaabaaa
     babbaababaaaabaaaaaaabaa
     abbaaaaabababbaaabbaaaba
     babbababbaaaabbbbabababbaabbbaaabbbabbbaabababba
     aaaabbabaababaabbabbabbaaaabaaaaaaabbbbb
     aabbbbabaaabbaabbbaabbbb
     aababaabaaaabbbaabaaaaab
     bbbbaaabaabbabbaabbaabbabaabbabb
     ababaaaabbaaabaaabbbbaba
     abbabababbabaaabbabaaaab
     babbbaabbaababbaaababbabbbaababbabbabbabababaababaaababbabbaaaab
     bbbbbaaabbbbbbbaabbbbbbb
     bbbababbaaaaaaabbbbbbaba
     bbaaaababbbabaaabaabbaaa
     bbabbbbaabaabbaaabbaabbababababb
     abbaaaaabaaabbabbabaabbaaabbaabb
     aaababaabaaaabbaaabbaaba
     aaaaaaabbaabbbbbbbbbabba
     aabbaabbbbaaaabbabbbbbbbabbababb
     bbabaabbaabaaaababababbababbbbbababbabababbaaabaabababab
     aaabaaabbbbaaababbbbaaaa
     babbaabaabbbabbbabbaaaab
     abbbbbaabbabababbbbbabaa
     bbaabbaabaabbbabaabababa
     aabaabbbaabaababbabbabbb
     aabbbabaaabbbbbbabaaaabaaabaaaaa
     aabbbbabaabbbbaaaaabaabb
     aaabbbbaaabbaabaabaaaabb
     aaaababbbaababbbbbbbabba
     aaaaababbaabbbababbaaaba
     baabababaabbababbbaaabba
     abbaabbbabbabbaaabbaabab
     bbabaababbbaaaaaabaaaababaaabbaababbbaaaababaaaabaabbaaaababbabb
     aaababbbbbabbaaabbbbbbbbbabbaaaa
     bbabbbaaaabbbbaaaaaaabbbabbabbabbbbaabaa
     aabbaabaaaabbaaabbbaabbbbabaaaababbbbbaaabaababbbaaababbaaaaabba
     bbabbabaabbbbaaaabababab
     aaaaabbabbbbbbbaaababbabbbbbabaa
     ababaababaabbbababbbbaababbbabababbbbbbb
     baabbbbaabbabaaaaaaaaaaaabaaaaaabaaabaaabaaaababaaabababababbbba
     aabaababbbaabbbaaaaabbaabbabbabaabaabbaaabbbaaaaabaabaabababaaaa
     bbaaaaaabbabaaababbaaaba
     baaabaabbbbababbbaabaaaa
     bbbaababbbbabaaaaaabbbbbbabbaaabaaaaabba
     ababbabaabbbabbbaabbbbabbaaaaaaa
     baaabbbbbbbababbabaaaabb
     bbabbabbabbbbaaaaaaabaaabaaaaaabbbbababa
     babbaaabaababaabbabbbaaabaaabaaaaabaaaababaaabba
     abbbabbbbaaaabbabbbababa
     aabbabababbabbaababbabab
     abaabbabaabbbbbbabbbabbbaaaabbbbaaaabbababaabbaa
     aabbbabbabababbbaabaababbabbbaabbababbba
     baababaaaababaaaabbbabab
     bbbaaabaaaaabbbbabaabbaa
     baaabbbabababbabbbbabbba
     baaababaabaaabaabbabbabbabbabababaabbbababbabaaabbaaaaabbaabbbbaaaaaababbaaabbbb
     baabbababbabbbbbbbbbabbb
     bbaaababbaaabbbbabaabbaa
     baaabaababaaaabaaababbababbababaaaababbbbaabaaba
     baaabaaabababbabaabababa
     bababbabaaabaaabaabbbbbabaaaaaaaabbaabab
     aabbabbabbabbabaabbbbaba
     aababbaabababaaabbbbaaaa
     bbbbabababaaaabaabbaabab
     bbbaabbabbaabbabaaabbbbbaaabbaaa
     aabaabaaababbaaabbaaabbbabbaaabaaaabbbabababbbaa
     aabaababbabbabbbbbbaabaaabaaabbaabbaaaab
     bababbaaaaaabaaaaaabaaba
     bbbbbabbabbbbaaababbabab
     abababaaaaaaabbaaaaaabaa
     babbaaabbabbaaaaababbaaaaaaaaaabbbbbbbaababbbbbabaaaaabbababbababbaabaab
     baababbbabbababaaabbababaabaaabaabbaaababbbabbbbaababbbb
     bbbbbaaaaaaaabbbaaaabbbabbaabbababaaaaab
     aabbbbaabababbabbaaaabaabaaabbaabbaabaababaababa
     aaaabbbbabababaaabababababbaababaabaabababbbaabbaaababbaabababaa
     aababaaababbbaabaaabbaba
     aaaaaaaabaaaaababbbaabaaabababab
     abaabbabaabbbbaababaaaab
     abababaabbaaaabaaabaabbbbaabbaaa
     abbaabbbaabaababaaaaaabbbabababb
     abbabababbabaabbbbbaabaa
     bbbbbabbabbababbbbaabbbb
     abbbbaabaabaabbbbaaaaaaabbababbb
     abaabbabababbbbbabbabbaababaaaba
     aaaaababaabaabbaabbabbaaabaabbbb
     babbaaabbababbabababbbaa
     abbabaabbbabaabaabbbbbba
     bbabaabbaababbbbbabbabababbbbbababbabbbb
     aabbabbaabaaaaaaabababbb
     abaaaaaaabbbabbbbbaabbbb
     abaabaabbbbbbaaabbaaababaabaabbababaaabbaaabaaabaabababa
     bbbbbabbbaabbabaabaaabba
     baababaaaaaababbabbbabba
     bbabaabbbabbabaabbaaabba
     aababaabbababbabaababbabbbbaabbaaabbbbbaaababbbbabababab
     babbabaabaababbbbaaababb
     aaaabbbbbababbaaaaaaabaa
     bbabbabaaabbabaaabaaaabb
     bbaabbaabbbababbaababbbb
     bbabbbaabbbbabaabbabbbabaabbaabbaabbaabb
     aaaabbbabababaaababaabbb
     bbbaaababbbbbabbbbbabaabaababaabbbbbabbb
     bbaaabaabbaaababbbbbaabbaaabbbaababaabba
     babbabaabbaabbabbbbbababbbaaabbb
     ababbbbbaaaaabababaabbbb
     aababbabaabbbbbbabbbbabb
     aaaabbaaaaabbaabbbbbaabaaabbbbbababaaabb
     aabbababaaaabbbabaabaaba
     aaaaaaababbababbbaabaaba
     bbbbbbbaabbaabaaaaabaaba
     babbabaaaababaaabbaaabbb
     bbabbbbaabbbbbabbbaaabbb
     aabbaaaababbabbaaaabbbaa
     aabbabaaabababaababbabab
     aabaabbbaaaaaabbaabbaaab
     bbaabbabbabbabbabbaabbbb
     bbbabaaabbbbbabbbaabbaaa
     abbabbaaabbaaaaabbabbbbabaaaaaaa
     abaaaaaaabbbabbbbbbbabbb
     bbbabaaaabbabaabbaaabbbbbbaababbbbabaabb
     bbbabaabaaabaabaabbbababaabbaabbababaabbabbbbabb
     baabbbbbbabbabaabbbababbbaaaabaa
     aaaaabbabbaababbbabbbbbb
     ababbbaaaaabbababbbbbaaaaaaabbabaababaab
     aaaaabbaaababaaaabbbabab
     bbabbbbaabbbbaabbbbbbaab
     baababababaaaabababbabbb
     aabaaabababbabaaabaabbbb
     aabbbabaabbabbaaaabbbabb
     baaabbbbbbbababbaaaaaabbaabbaaab
     baabbbbaabbaabbbbbaaaabb
     aaaabbaabaabbbabbaabbbaabaaaaaaabbaaabbabbababbabbbaabbabbabbbab
     ababaaaaababbbbbaaaaaaba
     babaaababbbbbbbabbbbbbbaaaababbbabaaaabb
     baababbbabbaabaabbbababbbabaababbbbbbaba
     abbbabbbbbbabbabbabbbabb
     abaabaabbabbaaaabaaaaaaa
     babbaaaabaaaabbaababbabaaaababbababaabaa
     babbbaababbabaaaababbaab
     baaabaaaabbbbaabaaaabaaaaaaaaaabaaaabbbbabaabaaabbabaaaaabbbaabbbabbbbab
     bbaababbbababaaaababbbab
     ababbbabbaabbbabaaababbabbbabaaabaaababbbbbbaaabbabaaababbbbbbababbbbabbabbbaaab
     aaabbaabbbaaabaaaaabbbab
     baabbbbabbabaabababaabba
     bbaaabaabbababbaaaaabbaa
     aaababaabbababbaaabbabaabbabbaaaaaabbaabbababbba
     babbbaaabbabbaaaaabaaaaa
     bbaaabaabaabaabbabaababb
     babbabaaabbaaabbabbaabab
     bbabaaabaabbababababaaab
     babbbababaabababbaaabbab
     baabbbababaabbababaabaaaabbaaabaaaabaaba
     ababaaaabbabababbbaaaaaabbbbbaba
     bbabbaaaabaaaaaabbbbaaaa
     aaaaaabbbaaabbbabbbabbaa
     abaabbabababaaaaaaababaababaabbbabaabbaa
     aabbbbabbbbabbbbabaabbbaabbaabbababbababbbbaaaaabaabbabababaaaaa
     abbbbaaaaabbbbbabbaaababaaabbbbababbbbbb
     bababbaabbbbbbbaaababaaaabbbbbaaabbaabbabbbababaaaababab
     ababbbbabbabbaaabbabbbaaaaabaaabaabbbaab
     bbbbaaabbbbbbbbabababaaabaaaaabb
     bbaababbbabbaaaabbaaabba
     aabaabaaaaababbaabaaaaaaaababbbb
     bbababbababbaaaaaababbba
     aabaabbbabbbbbaabbaabaaa
     bbbbbabbbaaabaaabaaaaaba
     aaababbababaaabbbabababaababaaaaaaababbbbbaaabab
     abbababbaaababaaabaababa
     aabaabaababbaabaababbbab
     babaaaaaabaabaabaaaabbbabaababbabbaaabbb
     bbbababbbbabbabbabbababaaaaabbbbbbbbaabaabbbababbaaaabab
     aaaababbbbabababbbbabaab
     aabbbbaabababbababababbb
     aaaaabbaaabbabababaaaabb
     aabbabaaaabbabaaabbbbbabbbbbbbbbbbbabbbb
     aaaaaabbaabbababababaaab
     aabbabbabbbabbabbaabbabb
     aabbbbbbbbabbabbabbbabab
     baababbabaabbbbbbbabaaabbbbbbabbabbbaababaaabbabaabbbaaa
     ababbabbabbbabbaaaaababa
     bbbaaabaabbabbaaabaaabaabaabaaba
     babbaababbaaaabaababbbaa
     abbaabaabbbbabaaaabbababaaababaabbbabaaaabaaabbabbaabbba
     baaabaaaabbbbbabababbbba
     bbbabbababbabaaababbaaabaabaaababbababaabbaaaabbabbabbbb
     aabaabbaaabbbbbaabaabbabaaaababbabababbaabababbb
     baabbbbbaababaabbabbaabb
     bbaaabaabaabbbbabaaabbbbbabababaaababababbaabaaabababbbb
     aaaaabbaaababaaabbaaaaaaabaababb
     bbaababaabbbaaaaabaababa
     abbabaaaaaabbbbaaabbbaababbbbababbaababbabbabbaa
     baaabaabbbbaabbaaaaaabaa
     aaaababbbabbbaabaababaaabaabbaabaabbaaab
     bbbaabaabbbbaaaaaababaababaaaabb
     baabbbbbbbaabbaabbbbbbbbbabbbaabaaaaaabb
     baababaababbbabaabaabaabaaabaaababaaabbaaabaaabb
     bbaabbaababaaabbabababab
     aaabbbbbbbabbbaaaaabaaaabaabbbaaabababba
     aaabbaabaabbaaaabbbbabbb
     baababaabaaaabbaabbaabbbabbbaaba
     baaabaababaabbbbbabbaaabbbabbaabbbbababbabbbababbbaabababaababab
     abbaabbaabbababbbabababbaababaabbbabbabbaabbbabbabbabbaaabbabbabaaabbaba
     bbabaabababbaabaababbabb
     ababaabbbaaababbbaaaababbbabbbab
     bbaababababbbaabbbaaaaab
     abbbbaaaaaababbaababaaaabbaaabba
     abbbaaaabbaaaaaaaabbbabb
     babbbbbbbbaaababbbbbbaaaaaaaababbaabbbbbbbbaaaababbabaaa
     babbaaabbbbbbaaabbababbb
     bbabbaaaabbaabaaababbbba
     aaaaaaabaabaabbbaaabbbaa
     aabbabaaaaaabbbbaabbaaaaaaabaaaa
     bbaaaaaababbaaaabbbaabaa
     baababbbabbbbaaabbaaabaabaaabaabaabaaaab
     abaaaabaaaaaababaabbbbaaaabbbbaabbaaaabaabaaaaab
     abaabbabbbabbabaabababaabbbbababbaaaaaab
     abaaabaabbabaabbaabbabbaaababbbbaaaaaaabaaababbaaabbaaabaaaabbaaababbaab
     aaaaabbbbbabbaaaabbaaaab
     babbaabaaaabbaababababbb
     bababaaabaabbaabababbabababaabbb
     aaaaabbbbbaaabaaabababbb
     bbabbaabbaaabaaabaabbaaa
     bbbbabbbaaaabbbaaabbaabbbbabaabaaababababbababbbaababbbb
     abbaaabbabbbabbbbbabbaaaabaabbabbbbaaaaa
     bbbbababbaaaaabaaaaaabaaabbbbabaabbbaabaaaaaabbabbbbbababaabaabbabbbbbabaaaabbab
     aaabbaababababaababbabbaaaabaabb
     abbbbabbbabaabbbabbbbbbaaabababbbabbabab
     aaaabbabaabbaaaaabababba
     abbababbbababbaabbbbbbab
     aaabaaabbbbabaababbaabaaaaaaaabbbaabbbbbbaaabbab
     baaabaaabbaaababbabbbbbababbbbbabababaabbabbbbab
     babbaabaaababaaaaabaaaaa
     bbbaaabaaaababbbbabbabab
     abaaabbbbbbaabbaabaabbbb
     aaaaabababaaaabaabaaabaa
     babbbaababbaaaaababaaaaa
     bbaababbbaabbbabbaaaaaab
     ababaaaaaabbbababbbaabab
     aaaaaaaababaaabaabbabbaabbbbbaba
     bbabbbbbaabbbbababaabaaa
     abbbbaaaaaaabbbaabbaabab
     abbbabbbaabaabbaaabbbaababbabaabbaabaabbbbbbbbbaabababba
     baabaabbbbabbbbbbbbbbaab
     bbaabbabbbababbaaababbbb
     aababaaaaabaaabababbbbba
     aaaabaaababaaabaaabbaaaabababbabababbaaa
     bbbbaaabaaaaaabbabaabbaa
     bbaabbaaaaaaaaaaabaaaaab
     aaababbbaabbababaaababbaabaaaaaabbbbbbab
     aaabbaaaabbbaabbaaabbbaaabbaabbbaabababaabababbbaabaaabaababbbbb
     baaabaaaaababaaabaabbaaa
     abbaabbbbbbaaabaaabbbaaa
     bababbababbaababbaabbabb
     abbababbaabbbabaabbbbbba
     babbaaaaabbbbaaabbbababbbaaabaaaabbababaababaaaaabaabaaaabaabbba
     baaabbbababbabaaaabbbabaabaabaababaaaaabbabbbbbbaabaaaab
     aabbaaaabaabbababaaaaabb
     babbaaabbaaaabbaabaaabab
     abaabbabbbaabbabbaabbabb
     babbbaabaaaaabbaabbababaabbaaabababbbbabaaaabbaaababaaabbbbaaaaa
     aabaaababbbabbaaaabaaabaaaabbaaabaaabaaabbaabbabaabaabababaaabbababbbaaaabbaababbaaababaaabbbbbb
     baababbaabbaabbbbabbabab
     aaaaaaaaaaaabbbbbbbaabbabbaaaabaaababbaaaabbaaababababbabaabaaba
     aabaaababbabbbaabbbaaaab
     abbbbbaaaaababbbbbabbababbbaabbaaabbaabaabaaabaa
     aabaabaaabababaabaabbababaaabaabaaaabababaabaabaabaaabaa
     bbbabbabbababbabaaabbaaa
     bbabbaaaaaababbaabbbbbaabababbbb
     ababbbbbbbabbabbabababba
     aabbbbabbbbbbbbbbbbaaaabbaababaabaababababbbabbbbaaaaaabbbabbabbabbabbba
     aaaaaabbbbbbaabbabaaabba
     aabbbbaabaaabbbbaabaaaaa
     aaabbbababaabababbaabbaaababbbbaaababaaababaabbbabbaabbb
     bbabbabbabbabaaaaaabbbba
     bbaabbabbababaaaabbbbaaaababbbababaaabab
     bbbbbbaaaaaabbbaabbaabba
     bbbaaababbaababaabbabaaababbbbabaabbaabbbabbbbaabaabaaaa
     aaabbaabbaabbbababaaaaab
     aaaabbbbabaabbabbabaabab
     aabaaabaabbabbabbabbaabaabaaabba
     aabbabaababaaabbabababbb
     aaaabaabbbbbbbbabbbbababaabaaabb
     aababbabaababaabaaabbbbb
     bbabaaabbaabbbbaaaaababaabbbbabbbbbaaaab
     abbaaaaabaaabbbababababa
     aababaaaaaaababaaabaabbbaabbabbb
     bbbabbabbbabbaaabbababbabbbbbaabbabbbbaa
     abbaaabbbbaababbaaabaaba
     aabaabaaaaaabababbbaaabbababaaaa
     bbbbbbbaaaaaabbabbabbbab
     bbbaaabaabababaabbbabbaa
     baaaabaababbbaaaabaabaaa
     bbbabaabaaababaaaababbba
     bbaaaababbabababaaabbbab
     baababaaaabbbbbabbaaaaab
     babbaaaaabaaaabbbbbaaaaa
     babaaabaaaaaababbbaaabbb
     aaaabbbbabaaaaaabbabbaabbbbaabaabaaabbbbaabaaababbabbbbb
     abbbbaabbbaabbaababbabaaaabbabbabababaab
     aababaabababbabababbabbb
     aababaaaaaaaaabbbbbaabbabababaaabbbbababbabaabbaaabbbabbbbbaaaaa
     bbbbaaabbbabaabaaaababbb
     aaaabbabbabbbaaababbaabb
     bbbaabbababbabbabaabbbabaaaabbaabbbaabbb
     bbbaaabbaabaaabaaaaabbbbaabbbbbbbbababababaaaabb
     bbababababbabaabbabbbbba
     bbbbbbbbbbaaaaaaaaaabaaaaaabbbbb
     babbabaabbabbaaabbaaabaabaaaaaab
     aababbabbabbbababbaaaababbaabababbaabbba
     aaabbaabbabaaabbabbabaaabbaaaaaababbabab
     ababaababbaaaabaabbbabab
     aababbaabbaababbbbabaabaaaabaaba
     aaaaaabbababbababababaabababbabbababbbbababbababaabbbaaa
     bbabababbaabbabaabbbaaab
     bababbabbaaabbbbaabbaaba
     baabbbaababaaaaaaabbabaabbabababaabaaabaabbbaabbabbaaaaaaababbbaaaaabaaabbababaaaaabbaba
     aabbbbbabaabaabbabbbbbabaabbbabb
     bbaaabbbbbbaababaabababaaabaaaab
     bbabbaaaabbbbaaabbababaa
     babaaabaababaababbbaaabbbaaabbaa
     babbabaabbbababbbbabbbab
     aaaabaaaabbabbaaaaabaabb
     aababaabbaabaabbbaabaaab
     babbbaabbaababbababbbabb
     aababbaabbabbbaaabaaaabb
     baababbaaaaaababababbbab
     aabbabbabbabababbababaab
     ababbbabbabbaaaaababbbbbbbbababa
     baaaabaabbabbbaaababbbaa
     bbabbbaaabaaabbbbabababa
     bbbbbbbabbbabaababaababb
     bbbaabbabaabababababbababbaababbababbaaaaabbbaaa
     abbaabaabbabbbaaaaaaabbbabbbbbabbbabaabbabbaabbaaaaabbaa
     aaabbbbaaabababbababaaabbbbbbaaabbbababbbbabbabaaabbabaaabbaaaaa
     baababaabbbabaabaaabbabb
     aaaabbabaabaaababaabbbbbbaabbbbabaaababbabbaabbababbbbab
     baabaabbaabbbbaaaabaabbabbbaabbbbaaababa
     aaabababbaababbabaabbbaaabbaaaaaabbbbbbbbabbbababbbababbaababababaaabbabbababbaa
     aabababbabaabaabbababbabaababbbababaabbbaababaabaaaabbbabbbabbaaabbabbaa
"))

(defun input-2 ()
  '((
     (51 -> 52 129 or 86 30)
     (82 -> 52 97 or 86 16)
     (72 -> 75 52 or 85 86)
     (118 -> 28 86 or 62 52)
     (104 -> 23 52 or 9 86)
     (36 -> 52 23 or 86 62)
     (34 -> 86 113)
     (39 -> 52 56)
     (58 -> 116 86 or 28 52)
     (79 -> 9 86 or 54 52)
     (33 -> 28 115)
     (105 -> 75 52 or 83 86)
     (30 -> 27 52 or 46 86)
     (107 -> 51 52 or 112 86)
     (10 -> 86 102 or 52 36)
     (112 -> 86 87 or 52 103)
     (24 -> 49 86 or 37 52)
     (135 -> 15 86 or 28 52)
     (84 -> 3 52 or 132 86)
     (15 -> 52 52 or 86 52)
     (95 -> 86 18 or 52 94)
     (48 -> 40 86 or 63 52)
     (116 -> 115 115)
     (28 -> 86 52 or 52 86)
     (61 -> 22 52 or 41 86)
     (26 -> 116 86 or 75 52)
     (49 -> 78 52 or 65 86)
     (20 -> 86 55 or 52 34)
     (41 -> 86 116 or 52 15)
     (87 -> 39 86 or 47 52)
     (100 -> 23 52 or 116 86)
     (129 -> 86 58 or 52 81)
     (4 -> 86 86 or 86 52)
     (8 -> 42 or 42 8)
     (121 -> 116 86 or 9 52)
     (119 -> 86 56 or 52 4)
     (133 -> 52 73 or 86 44)
     (35 -> 52 96 or 86 116)
     (32 -> 86 92 or 52 1)
     (113 -> 86 86)
     (97 -> 83 86 or 113 52)
     (89 -> 96 52 or 56 86)
     (3 -> 86 5 or 52 118)
     (2 -> 86 122 or 52 80)
     (120 -> 23 52 or 96 86)
     (56 -> 52 86 or 86 86)
     (52 -> "b")
     (19 -> 86 83 or 52 15)
     (22 -> 52 116 or 86 4)
     (71 -> 52 85 or 86 54)
     (74 -> 86 56)
     (130 -> 52 75 or 86 23)
     (80 -> 52 72 or 86 7)
     (40 -> 98 86 or 38 52)
     (50 -> 52 124 or 86 60)
     (108 -> 52 75 or 86 85)
     (64 -> 125 86 or 26 52)
     (78 -> 86 56 or 52 28)
     (43 -> 52 81 or 86 46)
     (85 -> 52 52 or 52 86)
     (128 -> 90 52 or 121 86)
     (110 -> 85 86 or 68 52)
     (14 -> 52 117 or 86 69)
     (101 -> 4 52 or 113 86)
     (117 -> 52 50 or 86 99)
     (45 -> 52 95 or 86 114)
     (29 -> 52 116 or 86 56)
     (73 -> 86 110 or 52 17)
     (102 -> 52 62 or 86 116)
     (134 -> 52 127 or 86 77)
     (131 -> 86 54 or 52 4)
     (123 -> 61 86 or 10 52)
     (54 -> 86 115 or 52 52)
     (109 -> 52 76 or 86 14)
     (44 -> 47 86 or 36 52)
     (124 -> 52 56 or 86 113)
     (38 -> 52 130 or 86 105)
     (21 -> 28 86 or 116 52)
     (65 -> 86 62 or 52 9)
     (37 -> 35 52 or 81 86)
     (115 -> 86 or 52)
     (103 -> 86 22 or 52 111)
     (53 -> 86 6 or 52 134)
     (59 -> 52 91 or 86 19)
     (88 -> 86 84 or 52 12)
     (12 -> 86 43 or 52 32)
     (6 -> 86 82 or 52 59)
     (125 -> 86 62 or 52 75)
     (27 -> 28 52 or 23 86)
     (96 -> 52 86)
     (47 -> 62 52 or 62 86)
     (9 -> 52 115 or 86 52)
     (0 -> 8 11)
     (63 -> 128 52 or 20 86)
     (68 -> 86 115 or 52 86)
     (13 -> 86 4 or 52 75)
     (18 -> 110 86 or 104 52)
     (126 -> 88 86 or 107 52)
     (111 -> 115 4)
     (122 -> 29 52 or 71 86)
     (16 -> 56 52 or 75 86)
     (91 -> 23 86 or 23 52)
     (69 -> 67 86 or 64 52)
     (70 -> 53 86 or 48 52)
     (7 -> 52 113 or 86 15)
     (127 -> 135 52 or 131 86)
     (57 -> 17 86 or 34 52)
     (86 -> "a")
     (106 -> 52 62 or 86 56)
     (11 -> 42 31 or 42 11 31)
     (17 -> 52 56 or 86 23)
     (98 -> 52 13 or 86 119)
     (92 -> 86 15 or 52 23)
     (94 -> 33 86 or 100 52)
     (55 -> 86 54 or 52 9)
     (132 -> 101 52 or 108 86)
     (75 -> 86 86 or 52 52)
     (5 -> 52 83 or 86 113)
     (60 -> 116 86 or 96 52)
     (23 -> 52 52)
     (77 -> 86 130 or 52 74)
     (99 -> 124 52 or 89 86)
     (83 -> 115 86 or 52 52)
     (90 -> 62 52 or 9 86)
     (42 -> 109 86 or 66 52)
     (114 -> 52 57 or 86 93)
     (25 -> 52 133 or 86 123)
     (62 -> 86 52)
     (93 -> 52 79 or 86 120)
     (76 -> 86 24 or 52 2)
     (66 -> 86 25 or 52 45)
     (46 -> 9 115)
     (67 -> 86 106 or 52 21)
     (31 -> 52 70 or 86 126)
     (81 -> 85 86 or 15 52)
     (1 -> 116 52 or 62 86))
    "bbbababaabaabaabababbbabaababababbabbbbabbabaabababbbaabbbbaaababbbbbaabbaaaaaab
     ababbbbbaaabbbababaabaababaabaaabbbaaabaaaaaabbbabbbaabababbabab
     aaabbabbababaaababbaaabaaabbaaabbababbbb
     aaaababbbbbbaabbbbabaabaaaaaabbaabbbbbabaaabbbaabaabbbaaabaabbbbaaabaaaa
     aaababaabababbabababbaaa
     abababaaabbabbaaaaaabaaaaaabaaba
     bbabbbbbaaaabbbbaabbabbaabbbbbabaababbbababaaaab
     babaaabaaabaababbbaaaabb
     bbaababbbbabababbbabaabbbaaaaaabbabbabaaaaaaaabababbbaab
     ababbbbbaaaabaabaaaabbbabaabbaabbaaababbbaaaababaaababbababaabbbabaaabbabaaaaabbaaabbaaa
     baaabaabaaaaabbbbaaaaaba
     bbbbbbbbbabbbabaaaabbabb
     bbbbbbaabbbbbabbbabbbbab
     aaaaabbbbbbbbbaaaababbaaabaaaaaabbabaaaa
     aabbbaababbaaaabbabababaaaabbaaa
     bbabababbbbababbbaaababb
     babbbbbbbbbbaabaabbaabbbbaaabbabbaabbbbbabababbb
     aabbaaaaaaaaabbaababaaab
     babbabbaabbbbbabbaaabbaa
     aaabaaabaabaabbaaabababa
     abaabaabaaababbbbbbbbbbabbbaabbbabaaabba
     bbbbaabbbbabbbbbabbbbaba
     abaabbabbaaaabbaabaabbbb
     aaaabbbaabababaaaaabaaaa
     aaaaabaababaaaaabbababbaabaababaabbabaabababbbaabbbaabbbaaaabaaabbbaaaaaaabbbaaa
     aabbabbaaababaaababaaaaa
     ababbbaabbbbabbbaabaaaabaababbbaaaabbbaababbbbba
     abbbbbabbbaaaabaaaabbbaa
     bbbbaabaabbababaabababab
     baabaabbbbbbbbbbbbababaa
     bbabbbbaaaaaaaaaaabaabaaababbaaa
     abbaabbbabaaaaaabbabbababbbbaaababaaaaba
     abbbbbbababaaababbbbaaabaabbaabaabbaabbaaabbaaabbaaaabbb
     abbaaaaaabbaabbbaabaaaab
     abaababbbbbbbbaabaaaaaabaabbaaaabbbbbbbb
     aabbbbbbbaabbbababbbabab
     bbbbaabaaabaabbbaaabbabb
     aaaabbbbbbabaabaaabaaababbabbbaababaababbabbabbbbaaabbab
     babbabbabbaaaaaaababaaaabaaaabaabababbbabababbbb
     bbabaabbaabbaaaabababaab
     baaabaaabbabbbbbabbabaabbabbbbba
     aabaababbabbaabababaaabbaaabaabbbabbbbba
     aababaaaaaaabababbababaa
     abbaaaaabbbbbabbaabaaabb
     baabbbabbaaabbbbbaaaaaaa
     bbbbaabbbbabbababbbabbba
     aaaababaaabbbbbabbabbbbbbabbbabaabaababb
     abbababaabbaaaaabbaabbabbabababb
     babaaabbaabaabbbabbabbabbaaaabaabbbbaababbbbabbbaabaaaaabaabaaba
     bbaabababbaaabaaabbbabba
     bbbaaabbbabbaaaaaabbabaabbaabbbbaaabbbab
     baabbbababbaabbbbbbabbbb
     aabbbbaaabbaabaaabaabaaa
     aabbabbbaababaababaabaaabbabbaabbabaabbaaaababbbaabbaaaababababbbabbabbbaabbaabb
     bbaaabababbabbabbbbabbba
     bbbabaaaaaaababbaaabbbaa
     aababaababbabaababbabbba
     abbbbaaaaaabaaabaabbbaabbbabbbbbabaaabab
     baabbaababbbbbabbaababbaababbbba
     aabbbbbbbbbaaabaabbbaaab
     bbbbbbbabaababaaabaaabab
     baabbabbaaabababababbaab
     ababaabaaaaababbabbaabba
     aabbabbababaaabbbbbaaaaa
     abaaabbbbbaabbbbbabbbbababbaabba
     bbabbabbbbaaaababaaaabaaabaababbbaabbabb
     aababaabbabbabbaaabaaabababbaabb
     baaabbbbaaaabbbbabbaabba
     abaababababbbbabbaababababababaababbbbaaabbabbbaabbbbbbbbbbbaaba
     bbabbabbbabbaababbabbbbbabbaabaabbbbabaa
     bbaaaababbbbaabaaaababab
     aaaabbabaaaaabbbaabaabbaababaaab
     aaababbbaaaaaabbaabbabbb
     babbaaababaaaabaababbbbbbabbbaaaaaabaabaabaabbba
     babbabaaabaabaabbabbaabaabbaabbbbbabbbabbabbabbbbbbaabab
     bbbabbabbbbaabbaabbaaaab
     babbaaabbaabbbbbabbaabaababbbabb
     abbaabaaaababaaabbbababbabbbaaabababaabb
     aababaabbabbbaabbbbbaabaaaababababbaaaba
     baabbbbaaababbabaabbbbaaaabbabaaabbbaaab
     baabababbababbabaaaaaaba
     aabbaaaaabbababaaaaaaaba
     aabbabaaababaaaaaabbaaaabbbaaabbbababbbb
     baabababaabaaaabbaabaaaababbbabb
     babbaabaaabaaabaabaabbba
     aaaaaaababaabbabbabbbbab
     aabaabbbaaabbbbabababbba
     bbbbaaabbbaaabaaaabbbabb
     abbbbaaabaabbbbaaabaabaababbaabababbbaaaabababba
     abababbbabbabbbabaabbababbbaaaabbbaaaaabbbabbbbbbbabaabbaabbabbaaababbab
     bbabbbaaabbabaaabbaaabba
     bababbabababaabaaabbaaba
     baababbaaabbbabaaabaabba
     aaaaaaabbbabaabbbbbbbaaaabbbabbaaaabbbbb
     bbbabaaaabbabbabbabbabaabbabbbab
     bbbbbbbabaabbabbaabbbaab
     abbbbaaabbbababbbaababbaaaaabaabaaaaaaaaabbabbba
     abbaaabbbbabbaabababbbbbbabaabba
     aababbaabbaabbaabababbbb
     bbaaabaaaabbbbabbbbaabaa
     aabbabaaabbaabaababaabaa
     abbbabbbaaaabbaababaabba
     bbbababbbbabbabbabaaaababaaabaaaaaaabaaababaabbb
     aaaabbbbbbabbabaabbaabbbbbbaabbb
     bbaaaababaababbbbaabaaaa
     abbaabbbbbaabbaaabbaabab
     abbbbbabbbbaaabbaaaaabbbabbaabaaaabbbaaa
     abbaabbbaabbbababbaaaabb
     baababaaaaaaaaabbbbbbaba
     baabbbbababbbaabbbabbbab
     babbabaabbabbbbaabaaaabaabbbbaabbaabbaaaaaabbaba
     baaababaaaabbbbaaabbaababbaaabab
     aabbaabbbbaaaaaabaabbaaaabbbaaaaabbbbaaa
     aabaababaabbbaabbbbaaababbbbabaa
     abbababbaaaabbbaabbbbbbb
     abbabaaaaaaabaaaabaabbaa
     baabbababbabaabaaaabbbab
     abababbabbaabbbaababbbaaaaababbbbbabbbbbbaaabbbbaaabbaaabaaaabaa
     aaababaabbabbaaabbbbbaab
     bbbabbabaababbaababbbaabbabaabbb
     bababaaaaaaaabbabbabababaaaabaaaaabaaabbbbbabbaababbabab
     aababaaababbbaabaabaaabababaabaababbabbbbabaaaaabaaaaabbbaaaaaba
     aabbbbaabbaaaabababbaaabbaaaaabb
     abbababaabbaaabbbabbabaa
     aabbbbbabbbbbbbbaabbabbabbbbaaaabbbaabab
     aabbbbbbbbbbaabababbbbbabbbaababbababaab
     abaaabbaabaabbababbbbaabaaaabbabbaabbbaababaaabbbabaabba
     bbaaaabaaababbaaababbababbbaabba
     bbbbaababbaabbaabaabbabb
     abaabaaabbbabaabbbabbbbbbbbaababbaaaaaabbaabbaababaabbba
     bbabaabbababaabababaabab
     aabbbbbbabbabbaaabbbabaa
     bbabbaabaaaabbbaaaaaaaaabbbbabbabaabaaab
     abbabbabbababaaabaaaabab
     bbaaabaaabbababaaabbbaaa
     bbabaabaabbbbbaabaaaaabb
     aabbabbaabbbabbbaaabbabb
     bababaaababbaaabbbbbbbbaababbaab
     babbabbbbaaabbbbabbbabaabbbabbbaababbbabbaababbaaaabaabbaaababababaaabaaaaabbaaaabaaaaaa
     bbbbababbbabaabbaabbbaabaaababbbababaaaababbbbaaabbbbabb
     bbabaaabaababbbaabbbbbbabaaaabbbabbabaabaaababbbbabbbaaabbbabbba
     aaaabbbbaababaabbabaaabaaabaabbaaababbbb
     aabaabbbbababaaabbabbbba
     bababbaabbbbbbbabbbababa
     abbaabbbbbabbababbbbababaababbbb
     aaaaabbbabbbbaaababbbaaaabbbbaba
     aabaababbabaaabbbbaaabaaaaaaabbabaaaaababbbbaaaaaaabbbab
     bbaababaabbaabbbabaaaabb
     abbbbbabaababaabbaabbbaa
     aaaabbabbbabbaabbbbababbbaabbbba
     bbaababbbabbaaaabbaabaaa
     babbaababaaaabaaaaaaabaa
     abbaaaaabababbaaabbaaaba
     babbababbaaaabbbbabababbaabbbaaabbbabbbaabababba
     aaaabbabaababaabbabbabbaaaabaaaaaaabbbbb
     aabbbbabaaabbaabbbaabbbb
     aababaabaaaabbbaabaaaaab
     bbbbaaabaabbabbaabbaabbabaabbabb
     ababaaaabbaaabaaabbbbaba
     abbabababbabaaabbabaaaab
     babbbaabbaababbaaababbabbbaababbabbabbabababaababaaababbabbaaaab
     bbbbbaaabbbbbbbaabbbbbbb
     bbbababbaaaaaaabbbbbbaba
     bbaaaababbbabaaabaabbaaa
     bbabbbbaabaabbaaabbaabbababababb
     abbaaaaabaaabbabbabaabbaaabbaabb
     aaababaabaaaabbaaabbaaba
     aaaaaaabbaabbbbbbbbbabba
     aabbaabbbbaaaabbabbbbbbbabbababb
     bbabaabbaabaaaababababbababbbbbababbabababbaaabaabababab
     aaabaaabbbbaaababbbbaaaa
     babbaabaabbbabbbabbaaaab
     abbbbbaabbabababbbbbabaa
     bbaabbaabaabbbabaabababa
     aabaabbbaabaababbabbabbb
     aabbbabaaabbbbbbabaaaabaaabaaaaa
     aabbbbabaabbbbaaaaabaabb
     aaabbbbaaabbaabaabaaaabb
     aaaababbbaababbbbbbbabba
     aaaaababbaabbbababbaaaba
     baabababaabbababbbaaabba
     abbaabbbabbabbaaabbaabab
     bbabaababbbaaaaaabaaaababaaabbaababbbaaaababaaaabaabbaaaababbabb
     aaababbbbbabbaaabbbbbbbbbabbaaaa
     bbabbbaaaabbbbaaaaaaabbbabbabbabbbbaabaa
     aabbaabaaaabbaaabbbaabbbbabaaaababbbbbaaabaababbbaaababbaaaaabba
     bbabbabaabbbbaaaabababab
     aaaaabbabbbbbbbaaababbabbbbbabaa
     ababaababaabbbababbbbaababbbabababbbbbbb
     baabbbbaabbabaaaaaaaaaaaabaaaaaabaaabaaabaaaababaaabababababbbba
     aabaababbbaabbbaaaaabbaabbabbabaabaabbaaabbbaaaaabaabaabababaaaa
     bbaaaaaabbabaaababbaaaba
     baaabaabbbbababbbaabaaaa
     bbbaababbbbabaaaaaabbbbbbabbaaabaaaaabba
     ababbabaabbbabbbaabbbbabbaaaaaaa
     baaabbbbbbbababbabaaaabb
     bbabbabbabbbbaaaaaaabaaabaaaaaabbbbababa
     babbaaabaababaabbabbbaaabaaabaaaaabaaaababaaabba
     abbbabbbbaaaabbabbbababa
     aabbabababbabbaababbabab
     abaabbabaabbbbbbabbbabbbaaaabbbbaaaabbababaabbaa
     aabbbabbabababbbaabaababbabbbaabbababbba
     baababaaaababaaaabbbabab
     bbbaaabaaaaabbbbabaabbaa
     baaabbbabababbabbbbabbba
     baaababaabaaabaabbabbabbabbabababaabbbababbabaaabbaaaaabbaabbbbaaaaaababbaaabbbb
     baabbababbabbbbbbbbbabbb
     bbaaababbaaabbbbabaabbaa
     baaabaababaaaabaaababbababbababaaaababbbbaabaaba
     baaabaaabababbabaabababa
     bababbabaaabaaabaabbbbbabaaaaaaaabbaabab
     aabbabbabbabbabaabbbbaba
     aababbaabababaaabbbbaaaa
     bbbbabababaaaabaabbaabab
     bbbaabbabbaabbabaaabbbbbaaabbaaa
     aabaabaaababbaaabbaaabbbabbaaabaaaabbbabababbbaa
     aabaababbabbabbbbbbaabaaabaaabbaabbaaaab
     bababbaaaaaabaaaaaabaaba
     bbbbbabbabbbbaaababbabab
     abababaaaaaaabbaaaaaabaa
     babbaaabbabbaaaaababbaaaaaaaaaabbbbbbbaababbbbbabaaaaabbababbababbaabaab
     baababbbabbababaaabbababaabaaabaabbaaababbbabbbbaababbbb
     bbbbbaaaaaaaabbbaaaabbbabbaabbababaaaaab
     aabbbbaabababbabbaaaabaabaaabbaabbaabaababaababa
     aaaabbbbabababaaabababababbaababaabaabababbbaabbaaababbaabababaa
     aababaaababbbaabaaabbaba
     aaaaaaaabaaaaababbbaabaaabababab
     abaabbabaabbbbaababaaaab
     abababaabbaaaabaaabaabbbbaabbaaa
     abbaabbbaabaababaaaaaabbbabababb
     abbabababbabaabbbbbaabaa
     bbbbbabbabbababbbbaabbbb
     abbbbaabaabaabbbbaaaaaaabbababbb
     abaabbabababbbbbabbabbaababaaaba
     aaaaababaabaabbaabbabbaaabaabbbb
     babbaaabbababbabababbbaa
     abbabaabbbabaabaabbbbbba
     bbabaabbaababbbbbabbabababbbbbababbabbbb
     aabbabbaabaaaaaaabababbb
     abaaaaaaabbbabbbbbaabbbb
     abaabaabbbbbbaaabbaaababaabaabbababaaabbaaabaaabaabababa
     bbbbbabbbaabbabaabaaabba
     baababaaaaaababbabbbabba
     bbabaabbbabbabaabbaaabba
     aababaabbababbabaababbabbbbaabbaaabbbbbaaababbbbabababab
     babbabaabaababbbbaaababb
     aaaabbbbbababbaaaaaaabaa
     bbabbabaaabbabaaabaaaabb
     bbaabbaabbbababbaababbbb
     bbabbbaabbbbabaabbabbbabaabbaabbaabbaabb
     aaaabbbabababaaababaabbb
     bbbaaababbbbbabbbbbabaabaababaabbbbbabbb
     bbaaabaabbaaababbbbbaabbaaabbbaababaabba
     babbabaabbaabbabbbbbababbbaaabbb
     ababbbbbaaaaabababaabbbb
     aababbabaabbbbbbabbbbabb
     aaaabbaaaaabbaabbbbbaabaaabbbbbababaaabb
     aabbababaaaabbbabaabaaba
     aaaaaaababbababbbaabaaba
     bbbbbbbaabbaabaaaaabaaba
     babbabaaaababaaabbaaabbb
     bbabbbbaabbbbbabbbaaabbb
     aabbaaaababbabbaaaabbbaa
     aabbabaaabababaababbabab
     aabaabbbaaaaaabbaabbaaab
     bbaabbabbabbabbabbaabbbb
     bbbabaaabbbbbabbbaabbaaa
     abbabbaaabbaaaaabbabbbbabaaaaaaa
     abaaaaaaabbbabbbbbbbabbb
     bbbabaaaabbabaabbaaabbbbbbaababbbbabaabb
     bbbabaabaaabaabaabbbababaabbaabbababaabbabbbbabb
     baabbbbbbabbabaabbbababbbaaaabaa
     aaaaabbabbaababbbabbbbbb
     ababbbaaaaabbababbbbbaaaaaaabbabaababaab
     aaaaabbaaababaaaabbbabab
     bbabbbbaabbbbaabbbbbbaab
     baababababaaaabababbabbb
     aabaaabababbabaaabaabbbb
     aabbbabaabbabbaaaabbbabb
     baaabbbbbbbababbaaaaaabbaabbaaab
     baabbbbaabbaabbbbbaaaabb
     aaaabbaabaabbbabbaabbbaabaaaaaaabbaaabbabbababbabbbaabbabbabbbab
     ababaaaaababbbbbaaaaaaba
     babaaababbbbbbbabbbbbbbaaaababbbabaaaabb
     baababbbabbaabaabbbababbbabaababbbbbbaba
     abbbabbbbbbabbabbabbbabb
     abaabaabbabbaaaabaaaaaaa
     babbaaaabaaaabbaababbabaaaababbababaabaa
     babbbaababbabaaaababbaab
     baaabaaaabbbbaabaaaabaaaaaaaaaabaaaabbbbabaabaaabbabaaaaabbbaabbbabbbbab
     bbaababbbababaaaababbbab
     ababbbabbaabbbabaaababbabbbabaaabaaababbbbbbaaabbabaaababbbbbbababbbbabbabbbaaab
     aaabbaabbbaaabaaaaabbbab
     baabbbbabbabaabababaabba
     bbaaabaabbababbaaaaabbaa
     aaababaabbababbaaabbabaabbabbaaaaaabbaabbababbba
     babbbaaabbabbaaaaabaaaaa
     bbaaabaabaabaabbabaababb
     babbabaaabbaaabbabbaabab
     bbabaaabaabbababababaaab
     babbbababaabababbaaabbab
     baabbbababaabbababaabaaaabbaaabaaaabaaba
     ababaaaabbabababbbaaaaaabbbbbaba
     bbabbaaaabaaaaaabbbbaaaa
     aaaaaabbbaaabbbabbbabbaa
     abaabbabababaaaaaaababaababaabbbabaabbaa
     aabbbbabbbbabbbbabaabbbaabbaabbababbababbbbaaaaabaabbabababaaaaa
     abbbbaaaaabbbbbabbaaababaaabbbbababbbbbb
     bababbaabbbbbbbaaababaaaabbbbbaaabbaabbabbbababaaaababab
     ababbbbabbabbaaabbabbbaaaaabaaabaabbbaab
     bbbbaaabbbbbbbbabababaaabaaaaabb
     bbaababbbabbaaaabbaaabba
     aabaabaaaaababbaabaaaaaaaababbbb
     bbababbababbaaaaaababbba
     aabaabbbabbbbbaabbaabaaa
     bbbbbabbbaaabaaabaaaaaba
     aaababbababaaabbbabababaababaaaaaaababbbbbaaabab
     abbababbaaababaaabaababa
     aabaabaababbaabaababbbab
     babaaaaaabaabaabaaaabbbabaababbabbaaabbb
     bbbababbbbabbabbabbababaaaaabbbbbbbbaabaabbbababbaaaabab
     aaaababbbbabababbbbabaab
     aabbbbaabababbababababbb
     aaaaabbaaabbabababaaaabb
     aabbabaaaabbabaaabbbbbabbbbbbbbbbbbabbbb
     aaaaaabbaabbababababaaab
     aabbabbabbbabbabbaabbabb
     aabbbbbbbbabbabbabbbabab
     baababbabaabbbbbbbabaaabbbbbbabbabbbaababaaabbabaabbbaaa
     ababbabbabbbabbaaaaababa
     bbbaaabaabbabbaaabaaabaabaabaaba
     babbaababbaaaabaababbbaa
     abbaabaabbbbabaaaabbababaaababaabbbabaaaabaaabbabbaabbba
     baaabaaaabbbbbabababbbba
     bbbabbababbabaaababbaaabaabaaababbababaabbaaaabbabbabbbb
     aabaabbaaabbbbbaabaabbabaaaababbabababbaabababbb
     baabbbbbaababaabbabbaabb
     bbaaabaabaabbbbabaaabbbbbabababaaababababbaabaaabababbbb
     aaaaabbaaababaaabbaaaaaaabaababb
     bbaababaabbbaaaaabaababa
     abbabaaaaaabbbbaaabbbaababbbbababbaababbabbabbaa
     baaabaabbbbaabbaaaaaabaa
     aaaababbbabbbaabaababaaabaabbaabaabbaaab
     bbbaabaabbbbaaaaaababaababaaaabb
     baabbbbbbbaabbaabbbbbbbbbabbbaabaaaaaabb
     baababaababbbabaabaabaabaaabaaababaaabbaaabaaabb
     bbaabbaababaaabbabababab
     aaabbbbbbbabbbaaaaabaaaabaabbbaaabababba
     aaabbaabaabbaaaabbbbabbb
     baababaabaaaabbaabbaabbbabbbaaba
     baaabaababaabbbbbabbaaabbbabbaabbbbababbabbbababbbaabababaababab
     abbaabbaabbababbbabababbaababaabbbabbabbaabbbabbabbabbaaabbabbabaaabbaba
     bbabaabababbaabaababbabb
     ababaabbbaaababbbaaaababbbabbbab
     bbaababababbbaabbbaaaaab
     abbbbaaaaaababbaababaaaabbaaabba
     abbbaaaabbaaaaaaaabbbabb
     babbbbbbbbaaababbbbbbaaaaaaaababbaabbbbbbbbaaaababbabaaa
     babbaaabbbbbbaaabbababbb
     bbabbaaaabbaabaaababbbba
     aaaaaaabaabaabbbaaabbbaa
     aabbabaaaaaabbbbaabbaaaaaaabaaaa
     bbaaaaaababbaaaabbbaabaa
     baababbbabbbbaaabbaaabaabaaabaabaabaaaab
     abaaaabaaaaaababaabbbbaaaabbbbaabbaaaabaabaaaaab
     abaabbabbbabbabaabababaabbbbababbaaaaaab
     abaaabaabbabaabbaabbabbaaababbbbaaaaaaabaaababbaaabbaaabaaaabbaaababbaab
     aaaaabbbbbabbaaaabbaaaab
     babbaabaaaabbaababababbb
     bababaaabaabbaabababbabababaabbb
     aaaaabbbbbaaabaaabababbb
     bbabbaabbaaabaaabaabbaaa
     bbbbabbbaaaabbbaaabbaabbbbabaabaaababababbababbbaababbbb
     abbaaabbabbbabbbbbabbaaaabaabbabbbbaaaaa
     bbbbababbaaaaabaaaaaabaaabbbbabaabbbaabaaaaaabbabbbbbababaabaabbabbbbbabaaaabbab
     aaabbaababababaababbabbaaaabaabb
     abbbbabbbabaabbbabbbbbbaaabababbbabbabab
     aaaabbabaabbaaaaabababba
     abbababbbababbaabbbbbbab
     aaabaaabbbbabaababbaabaaaaaaaabbbaabbbbbbaaabbab
     baaabaaabbaaababbabbbbbababbbbbabababaabbabbbbab
     babbaabaaababaaaaabaaaaa
     bbbaaabaaaababbbbabbabab
     abaaabbbbbbaabbaabaabbbb
     aaaaabababaaaabaabaaabaa
     babbbaababbaaaaababaaaaa
     bbaababbbaabbbabbaaaaaab
     ababaaaaaabbbababbbaabab
     aaaaaaaababaaabaabbabbaabbbbbaba
     bbabbbbbaabbbbababaabaaa
     abbbbaaaaaaabbbaabbaabab
     abbbabbbaabaabbaaabbbaababbabaabbaabaabbbbbbbbbaabababba
     baabaabbbbabbbbbbbbbbaab
     bbaabbabbbababbaaababbbb
     aababaaaaabaaabababbbbba
     aaaabaaababaaabaaabbaaaabababbabababbaaa
     bbbbaaabaaaaaabbabaabbaa
     bbaabbaaaaaaaaaaabaaaaab
     aaababbbaabbababaaababbaabaaaaaabbbbbbab
     aaabbaaaabbbaabbaaabbbaaabbaabbbaabababaabababbbaabaaabaababbbbb
     baaabaaaaababaaabaabbaaa
     abbaabbbbbbaaabaaabbbaaa
     bababbababbaababbaabbabb
     abbababbaabbbabaabbbbbba
     babbaaaaabbbbaaabbbababbbaaabaaaabbababaababaaaaabaabaaaabaabbba
     baaabbbababbabaaaabbbabaabaabaababaaaaabbabbbbbbaabaaaab
     aabbaaaabaabbababaaaaabb
     babbaaabbaaaabbaabaaabab
     abaabbabbbaabbabbaabbabb
     babbbaabaaaaabbaabbababaabbaaabababbbbabaaaabbaaababaaabbbbaaaaa
     aabaaababbbabbaaaabaaabaaaabbaaabaaabaaabbaabbabaabaabababaaabbababbbaaaabbaababbaaababaaabbbbbb
     baababbaabbaabbbbabbabab
     aaaaaaaaaaaabbbbbbbaabbabbaaaabaaababbaaaabbaaababababbabaabaaba
     aabaaababbabbbaabbbaaaab
     abbbbbaaaaababbbbbabbababbbaabbaaabbaabaabaaabaa
     aabaabaaabababaabaabbababaaabaabaaaabababaabaabaabaaabaa
     bbbabbabbababbabaaabbaaa
     bbabbaaaaaababbaabbbbbaabababbbb
     ababbbbbbbabbabbabababba
     aabbbbabbbbbbbbbbbbaaaabbaababaabaababababbbabbbbaaaaaabbbabbabbabbabbba
     aaaaaabbbbbbaabbabaaabba
     aabbbbaabaaabbbbaabaaaaa
     aaabbbababaabababbaabbaaababbbbaaababaaababaabbbabbaabbb
     bbabbabbabbabaaaaaabbbba
     bbaabbabbababaaaabbbbaaaababbbababaaabab
     bbbbbbaaaaaabbbaabbaabba
     bbbaaababbaababaabbabaaababbbbabaabbaabbbabbbbaabaabaaaa
     aaabbaabbaabbbababaaaaab
     aaaabbbbabaabbabbabaabab
     aabaaabaabbabbabbabbaabaabaaabba
     aabbabaababaaabbabababbb
     aaaabaabbbbbbbbabbbbababaabaaabb
     aababbabaababaabaaabbbbb
     bbabaaabbaabbbbaaaaababaabbbbabbbbbaaaab
     abbaaaaabaaabbbababababa
     aababaaaaaaababaaabaabbbaabbabbb
     bbbabbabbbabbaaabbababbabbbbbaabbabbbbaa
     abbaaabbbbaababbaaabaaba
     aabaabaaaaaabababbbaaabbababaaaa
     bbbbbbbaaaaaabbabbabbbab
     bbbaaabaabababaabbbabbaa
     baaaabaababbbaaaabaabaaa
     bbbabaabaaababaaaababbba
     bbaaaababbabababaaabbbab
     baababaaaabbbbbabbaaaaab
     babbaaaaabaaaabbbbbaaaaa
     babaaabaaaaaababbbaaabbb
     aaaabbbbabaaaaaabbabbaabbbbaabaabaaabbbbaabaaababbabbbbb
     abbbbaabbbaabbaababbabaaaabbabbabababaab
     aababaabababbabababbabbb
     aababaaaaaaaaabbbbbaabbabababaaabbbbababbabaabbaaabbbabbbbbaaaaa
     bbbbaaabbbabaabaaaababbb
     aaaabbabbabbbaaababbaabb
     bbbaabbababbabbabaabbbabaaaabbaabbbaabbb
     bbbaaabbaabaaabaaaaabbbbaabbbbbbbbababababaaaabb
     bbababababbabaabbabbbbba
     bbbbbbbbbbaaaaaaaaaabaaaaaabbbbb
     babbabaabbabbaaabbaaabaabaaaaaab
     aababbabbabbbababbaaaababbaabababbaabbba
     aaabbaabbabaaabbabbabaaabbaaaaaababbabab
     ababaababbaaaabaabbbabab
     aababbaabbaababbbbabaabaaaabaaba
     aaaaaabbababbababababaabababbabbababbbbababbababaabbbaaa
     bbabababbaabbabaabbbaaab
     bababbabbaaabbbbaabbaaba
     baabbbaababaaaaaaabbabaabbabababaabaaabaabbbaabbabbaaaaaaababbbaaaaabaaabbababaaaaabbaba
     aabbbbbabaabaabbabbbbbabaabbbabb
     bbaaabbbbbbaababaabababaaabaaaab
     bbabbaaaabbbbaaabbababaa
     babaaabaababaababbbaaabbbaaabbaa
     babbabaabbbababbbbabbbab
     aaaabaaaabbabbaaaaabaabb
     aababaabbaabaabbbaabaaab
     babbbaabbaababbababbbabb
     aababbaabbabbbaaabaaaabb
     baababbaaaaaababababbbab
     aabbabbabbabababbababaab
     ababbbabbabbaaaaababbbbbbbbababa
     baaaabaabbabbbaaababbbaa
     bbabbbaaabaaabbbbabababa
     bbbbbbbabbbabaababaababb
     bbbaabbabaabababababbababbaababbababbaaaaabbbaaa
     abbaabaabbabbbaaaaaaabbbabbbbbabbbabaabbabbaabbaaaaabbaa
     aaabbbbaaabababbababaaabbbbbbaaabbbababbbbabbabaaabbabaaabbaaaaa
     baababaabbbabaabaaabbabb
     aaaabbabaabaaababaabbbbbbaabbbbabaaababbabbaabbababbbbab
     baabaabbaabbbbaaaabaabbabbbaabbbbaaababa
     aaabababbaababbabaabbbaaabbaaaaaabbbbbbbbabbbababbbababbaababababaaabbabbababbaa
     aabababbabaabaabbababbabaababbbababaabbbaababaabaaaabbbabbbabbaaabbabbaa
"))

(defun example ()
  '((
     (0 -> 1 2)
     (1 -> "a")
     (2 -> 1 3 or 3 1)
     (3 -> "b"))
    ()))

(defun example-2 ()
  '((
     (0 -> 4 1 5)
     (1 -> 2 3 or 3 2)
     (2 -> 4 4 or 5 5)
     (3 -> 4 5 or 5 4)
     (4 -> "a")
     (5 -> "b"))
    "aaaabb
 aaabab
 abbabb
 abbbab
 aabaab
 aabbbb
 abaaab
 ababbb
"))

(defun example-3 ()
  '((
     (0 -> 4 1 5)
     (1 -> 2 3 or 3 2)
     (2 -> 4 4 or 5 5)
     (3 -> 4 5 or 5 4)
     (4 -> "a")
     (5 -> "b"))
    "ababbb
bababa
abbbab
aaabbb
aaaabbb"))

(defun example-4 ()
  '((
(42 -> 9 14 or 10 1)
(9 -> 14 27 or 1 26)
(10 -> 23 14 or 28 1)
(1 -> "a")
(11 -> 42 31)
(5 -> 1 14 or 15 1)
(19 -> 14 1 or 14 14)
(12 -> 24 14 or 19 1)
(16 -> 15 1 or 14 14)
(31 -> 14 17 or 1 13)
(6 -> 14 14 or 1 14)
(2 -> 1 24 or 14 4)
(0 -> 8 11)
(13 -> 14 3 or 1 12)
(15 -> 1 or 14)
(17 -> 14 2 or 1 7)
(23 -> 25 1 or 22 14)
(28 -> 16 1)
(4 -> 1 1)
(20 -> 14 14 or 1 15)
(3 -> 5 14 or 16 1)
(27 -> 1 6 or 14 18)
(14 -> "b")
(21 -> 14 1 or 1 14)
(25 -> 1 1 or 1 14)
(22 -> 14 14)
(8 -> 42)
(26 -> 14 22 or 1 20)
(18 -> 15 15)
(7 -> 14 5 or 1 21)
(24 -> 14 1)
     )
    "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"))


(defun example-4c ()
  '((
(42 -> 9 14 or 10 1)
(9 -> 14 27 or 1 26)
(10 -> 23 14 or 28 1)
(1 -> "a")
(11 -> 42 31 or 42 11 31)
(5 -> 1 14 or 15 1)
(19 -> 14 1 or 14 14)
(12 -> 24 14 or 19 1)
(16 -> 15 1 or 14 14)
(31 -> 14 17 or 1 13)
(6 -> 14 14 or 1 14)
(2 -> 1 24 or 14 4)
(0 -> 8 11)
(13 -> 14 3 or 1 12)
(15 -> 1 or 14)
(17 -> 14 2 or 1 7)
(23 -> 25 1 or 22 14)
(28 -> 16 1)
(4 -> 1 1)
(20 -> 14 14 or 1 15)
(3 -> 5 14 or 16 1)
(27 -> 1 6 or 14 18)
(14 -> "b")
(21 -> 14 1 or 1 14)
(25 -> 1 1 or 1 14)
(22 -> 14 14)
(8 -> 42 or 42 8)
(26 -> 14 22 or 1 20)
(18 -> 15 15)
(7 -> 14 5 or 1 21)
(24 -> 14 1)
     )
    "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"))
