(defun day-12-2020-1 () (do-float (input)))

(defun day-12-2020-2 () (do-float-by-waypoint (input)))

(defun do-float (input)
  (loop for i in input
        for dir = (update-direction i "E") then (update-direction i dir)
        for x = (update-x i dir) then (+ x (update-x i dir))
        for y = (update-y i dir) then (+ y (update-y i dir))
        finally (return (+ (abs x) (abs y)))))

(defun update-direction (command direction)
  (let ((dir (direction command)))
    (cond ((equal "L" dir) (new-direction direction (/ (distance command) 90) (left)))
          ((equal "R" dir) (new-direction direction (/ (distance command) 90) (right)))
          (t direction))))

(defun new-direction (old times table)
  (loop repeat times
        for i = old then next for next = (second (assoc i table :test #'equal))
        finally (return next)))

(defun left () '(("E" "N") ("N" "W") ("W" "S") ("S" "E")))

(defun right () '(("E" "S") ("S" "W") ("W" "N") ("N" "E")))

(defun distance (symbol)
  (parse-integer (subseq (string symbol) 1)))

(defun direction (symbol)
  (subseq (string symbol) 0 1))

(defun update-x (command direction)
  (update-xy "E" "W" command direction))

(defun update-y (command direction)
  (update-xy "S" "N" command direction))

(defun update-xy (inc-direction dec-direction command direction)
  (let  ((command-direction (direction command)))
    (cond ((directional-move command-direction inc-direction direction) (distance command))
          ((directional-move command-direction dec-direction direction) (- (distance command)))
          (t 0))))

(defun directional-move (command-direction expected-direction current-direction)
  (or (equal command-direction expected-direction)
      (and (equal command-direction "F")
           (equal current-direction expected-direction))))

(defun do-float-by-waypoint (input)
  (loop for i in input
        for waypoint = (move-waypoint i '(10 -1)) then (move-waypoint i waypoint)
        for (x y) = (move-ship i waypoint '(0 0)) then (move-ship i waypoint (list x y))
        finally (return (+ (abs x) (abs y)))))

(defun move-waypoint (command waypoint)
  (let ((direction (direction command))
        (distance-or-angle (distance command)))
    (destructuring-bind (x y) waypoint
      (cond ((equal direction "E") (list (+ x distance-or-angle) y))
            ((equal direction "W") (list (- x distance-or-angle) y))
            ((equal direction "S") (list x (+ y distance-or-angle)))
            ((equal direction "N") (list x (- y distance-or-angle)))
            ((member direction '("L" "R") :test #'equal)
             (rotate-waypoint (/ distance-or-angle 90) direction waypoint))
            (t waypoint)))))

(defun rotate-waypoint (times direction waypoint)
  (loop repeat times
        for wp = waypoint then next
        for next = (rotate-it direction wp)
        finally (return next)))

(defun rotate-it (direction waypoint)
  (destructuring-bind (x y) waypoint
    (if (equal direction "R")
        (list (- y) x)
        (list y (- x)))))

(defun move-ship (command waypoint ship)
  (if (equal "F" (direction command))
      (destructuring-bind (x y) ship
        (destructuring-bind (wx wy) waypoint
                       (list (+ x (* (distance command) wx))
                             (+ y (* (distance command) wy)))))
      ship))

(defun input ()
  '(E2
    F24
    F60
    N3
    E1
    R180
    F14
    L90
    N4
    W3
    F81
    R90
    F43
    R90
    F40
    S2
    F56
    L90
    E1
    S1
    R90
    F66
    L180
    W2
    F84
    R90
    E4
    F32
    E5
    R90
    F44
    E1
    R90
    E4
    L90
    N1
    W2
    R90
    E5
    L90
    F86
    L180
    S2
    R180
    E2
    F22
    R180
    F42
    L180
    S4
    W4
    N5
    R270
    F23
    N3
    F18
    E2
    N2
    S3
    W5
    R90
    E4
    S1
    R180
    S1
    F69
    W2
    F55
    E5
    S3
    E4
    N1
    R180
    E1
    S5
    W1
    S5
    L270
    F17
    R180
    N5
    F6
    S3
    R180
    F75
    S3
    F90
    W3
    F98
    N2
    F54
    E5
    S5
    F4
    E5
    R90
    N4
    L90
    E4
    N3
    F75
    E3
    S2
    F89
    E1
    R180
    S3
    E2
    R270
    W4
    L90
    N5
    E4
    W3
    N1
    F37
    L90
    N5
    E3
    N3
    L90
    W1
    R90
    F25
    N2
    R90
    E4
    F81
    N5
    L90
    F47
    E1
    F86
    N5
    W1
    N2
    F35
    L90
    W4
    F35
    R90
    W3
    S3
    W1
    S3
    E5
    L180
    F1
    L90
    E4
    L90
    S4
    F58
    N2
    F95
    L180
    F27
    S4
    L90
    E4
    F90
    E5
    N2
    F36
    W2
    S5
    N3
    W4
    S1
    F58
    E4
    R90
    F60
    E1
    N2
    R90
    N1
    W2
    L90
    N3
    F16
    S2
    R90
    F3
    R270
    F98
    S4
    W1
    S4
    F42
    S5
    N2
    E1
    N4
    E3
    R180
    W3
    R180
    N4
    F47
    N5
    W5
    F55
    E2
    F12
    W3
    N1
    R90
    F55
    R180
    F89
    S4
    F38
    S5
    R180
    N2
    W4
    F16
    N2
    R180
    E2
    R180
    W1
    R90
    F50
    E1
    F36
    W4
    F79
    E3
    F19
    N4
    L270
    N5
    W3
    F48
    R90
    F36
    S5
    F100
    E3
    F55
    N3
    F16
    R180
    S5
    N3
    R180
    N1
    F8
    E5
    L90
    F21
    L90
    F80
    L90
    N5
    W5
    S5
    E5
    R90
    N4
    E1
    F20
    S3
    L90
    E1
    N4
    F16
    F26
    E1
    F75
    E1
    R90
    F69
    N1
    E3
    R90
    F62
    L180
    W4
    F80
    S5
    L90
    F90
    W5
    F40
    W1
    L90
    W1
    R180
    S2
    F65
    E2
    R180
    E1
    F37
    E5
    R90
    N3
    W4
    S5
    E3
    F36
    F8
    W5
    L90
    F54
    N5
    L180
    S4
    L90
    F88
    R180
    F46
    N5
    E3
    F93
    S2
    E5
    R270
    F15
    R90
    F28
    S2
    F61
    N5
    L90
    W3
    S5
    L90
    N1
    L90
    F41
    W4
    N3
    F4
    N3
    F35
    R90
    W5
    F69
    L90
    E3
    R90
    N2
    W4
    N5
    E4
    R90
    N1
    R90
    N2
    F44
    L90
    F15
    W2
    R180
    E3
    L90
    F26
    L90
    S4
    W4
    S1
    F46
    E2
    S5
    R180
    W3
    F6
    L90
    W3
    F99
    E3
    L90
    S1
    E1
    N2
    L90
    N3
    E4
    N3
    W1
    F87
    N2
    L180
    E1
    S1
    E1
    N4
    F20
    N1
    L90
    F41
    N1
    E5
    F20
    N4
    R180
    L180
    F69
    N3
    L90
    F80
    W4
    N5
    F95
    L90
    F27
    N2
    F22
    L270
    F74
    W2
    R180
    S3
    F64
    N3
    L270
    E2
    L90
    E2
    N1
    F72
    W3
    F78
    S3
    F56
    E3
    S4
    R180
    S1
    F53
    E4
    F96
    N1
    L90
    N3
    E5
    F83
    S1
    F26
    S3
    W5
    F20
    R270
    W1
    R90
    N4
    W1
    F69
    R90
    F7
    R180
    F15
    R90
    F65
    S1
    F68
    S5
    R180
    E5
    L90
    S1
    F68
    N1
    F7
    R90
    S1
    W5
    F29
    R270
    E5
    S4
    F14
    W2
    F91
    E2
    S5
    E4
    L90
    F81
    S4
    F69
    R90
    W3
    R180
    F33
    L180
    F52
    S2
    W5
    F2
    N4
    F13
    L270
    F63
    N5
    S3
    W5
    F67
    S1
    L90
    E5
    L90
    S3
    W5
    F67
    R90
    F64
    S4
    E5
    N4
    F97
    E2
    R90
    N1
    F5
    S3
    L90
    S2
    R90
    N3
    E3
    F76
    S4
    R90
    W1
    L180
    W4
    L90
    S3
    L90
    F66
    L90
    E1
    F71
    L270
    F42
    N1
    W1
    L90
    S4
    E3
    F41
    R180
    F68
    W5
    L90
    N5
    E5
    F96
    W3
    F90
    S1
    E1
    R90
    N3
    F38
    R90
    W3
    R90
    W1
    L90
    S5
    F76
    E3
    F36
    S1
    L180
    E1
    F22
    N1
    W1
    R270
    E4
    N5
    E3
    N3
    W2
    L180
    S2
    W5
    N4
    W5
    N3
    R90
    S2
    F45
    R90
    W2
    R90
    W3
    F75
    R180
    E4
    N1
    E4
    L90
    F59
    L90
    S2
    E2
    S2
    R90
    E2
    R90
    F37
    E2
    S1
    S5
    F97
    W4
    N1
    F58
    S2
    F14
    L90
    E3
    R90
    N1
    F4
    F28
    R180
    F55
    N2
    R90
    F48
    E4
    S4
    L90
    N4
    E1
    F67
    S4
    E5
    F16
    E5
    F82
    E4
    L270
    R90
    N5
    L90
    F77
    S3
    E1
    F85
    W3
    S1
    E2
    S5
    R90
    N2
    W5
    E1
    L90
    E3
    R90
    N5
    L270
    N5
    F77
    W4
    N5
    E4
    L90
    E2
    L180
    F13
    E5
    L270
    W1
    F52
    L90
    W3
    R90
    F23
    E1
    F25
    S2
    L180
    F80
    R180
    S5
    E3
    F90
    E4
    F60
    N4
    F75
    E3
    F10
    S3
    E1
    F8
    W5
    R90
    F20
    S3
    R90
    N2
    F41
    L90
    F8
    R270
    W1
    S1
    E2
    L180
    S4
    F52
    E5
    N2
    S3
    W1
    F67
    L90
    E3
    S2
    E2
    F30
    F12
    E3
    F11
    W4
    L90
    S4
    F5
    R90
    S5
    E2
    E3
    R90
    F37
    R90
    F64
    R90
    F68
    L90
    N1
    E2
    N4
    R180
    W4
    L90
    F68
    R90
    N5
    E2
    F54
    W2
    F84
    L90
    E3
    R90
    E2
    F29
    R90
    N1
    E2
    L90
    S5
    F90
    E2
    F11
    S3
    F99
    W3
    S5
    F94
    R90
    F36))

(defun example ()
  '(F10
    N3
    F7
    R90
    F11))
