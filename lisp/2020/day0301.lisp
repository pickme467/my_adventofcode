(defun day-03-2020-1 ()
  (do-traverse (input) 3 1))

(defun day-03-2020-2 ()
  (* (do-traverse (input) 1 1) (do-traverse (input) 3 1) (do-traverse (input) 5 1)
     (do-traverse (input) 7 1) (do-traverse (input) 1 2)))

(defun do-traverse (input step-x step-y)
  (loop for row in input for y from 0 with x = 0
        when (matching-row step-y y)
          count (matching-tree row (incf x step-x)) into cnt
        finally (return cnt)))

(defun matching-row (step-y y)
  (and (>= y step-y) (equal 0 (mod y step-y))))

(defun matching-tree (row x)
  (equal #\# (elt row (mod x (length row)))))

(defun input ()
  '("............#....#............."
    "...........##....#......#..#..#"
    "......#.......#......#........."
    "..#.#....#....#.............##."
    "..#........####....#...#......."
    "..##.....#.#.#..#.........#...."
    "...#.#..#..#....#..#..#........"
    "#.......#.........#....##.###.."
    "......##..#.#...#.......#.#...."
    "................##.........#.##"
    "..##..........#...#.........#.#"
    "..........#...##..............."
    "#...#......#..#.#..#...##..#..."
    "..##....#.......#......#..#...."
    "....#......#......#....#......."
    ".........#.....#..#............"
    ".#...#.#.........#........#...."
    "#..........####.....#.........."
    "......##.....#....#..#........#"
    "#......#......#...........#...."
    "....#.........#....#...#..#..#."
    ".#........#......#.#.....#....."
    "..#.#.#..........#....#.......#"
    "......#.#........##....##....##"
    ".....#.#..#...#................"
    "......#......##...............#"
    "..#..##.............#...##....."
    "......##......##..#......#....."
    "....#.............#..##.....##."
    "........#...............##....."
    "..#......#.##..#...#....#...#.."
    "#......#.......#.............#."
    ".....#....##..............#...."
    "#.#.........#....#..##....#...."
    ".#...#...#....#.#............#."
    "...#...#.#..##.##.......##....."
    "......#..#....##..#.#..#..#...."
    ".......##..#..#......#..#.....#"
    ".##..#......#..........#....#.."
    ".....#................#..#....#"
    "........#..#....#.......#....#."
    "..#......#.......#......#....#."
    "....#...#.##........##....#...."
    ".....#........#...........#...."
    "...#....##..........#..#...#.#."
    "...#.......#......#...##...#..."
    ".#.....#........#........#.#..#"
    ".#.........#..##.....#.......#."
    "....#..#....#.......#......#..."
    ".#.#...##..##................##"
    "......#.#...#.......#....#....#"
    "........#....#..#.....#......#."
    ".......#..........#......#....."
    "..............................."
    "..#..#####..#..#..........#.#.."
    ".....#....##................#.#"
    ".................##............"
    ".#...#...#..#...........#...##."
    "..#..#.#...........#.....##...."
    ".#.......#.....#..##..#.#....#."
    "..........#.#......##...##....."
    "........##..#......##...#......"
    "#......................#......."
    "............#.....#....#.#...#."
    "#......#..........##..#........"
    ".........#.......#...#.#......."
    "...........##....#........#...."
    "#........#.....#...#........##."
    ".#......##......#.##.......#..#"
    ".....#......#.#......#.......#."
    ".....#.#.........#............."
    "...........#..#....#.....#.#..."
    "...#............#...#.........."
    "..#..#...#....................."
    "......#..#...#....#............"
    ".#.#.#........#..#...#........."
    "..........#........#..#........"
    "..............#...#....#......."
    "..#....#....##.......#...#.##.."
    ".#.........#...#......#........"
    "..#......#...#.........##.#...#"
    "...#.....#...#..#.............#"
    ".##........#.#.#.............#."
    "..#.............#..#.#...#....#"
    "#...#.........#......#......#.."
    ".......##..#.#..........#...#.."
    ".......#.............#..#.#...."
    ".#..#....#.#..................."
    "....##...#..#....#..#.........."
    "....#.#............#..........."
    "###........##..#.#..#.........."
    ".#.#.#.......#...........#..#.#"
    "..........##..#.............#.."
    ".#...........#......#.#..#..##."
    "...###......#.##........#.....#"
    "....#..#..#...#................"
    "...#.....#........#............"
    "....#...#...#..#..##.##.......#"
    "#.......#......#....#.......#.."
    "#.............#...#............"
    "##......#..#...#....##.#...#..."
    ".##....................#....#.."
    "..#.....#....#.#....#......#..."
    ".......#..#..#............#...#"
    ".#.....#.......#..#..#..#......"
    "......##......................."
    "#..#...#.#.#....#.....#..#....."
    "...................#...#...#..."
    "........#....##..#....#........"
    "##......#.#......##.###........"
    ".........#...##................"
    ".......#...#...#.......##......"
    "....#.......#......#.........##"
    "....#....#.#..#.....#.........."
    "...........#.......#........#.."
    "..#.........###.#........#....."
    ".......#...........#.#.....##.."
    "..#...#..#..........#.........."
    "..........#.#....#............."
    ".##....#........##............."
    ".............#.#####........#.#"
    ".................##...#........"
    "##...#.#.......##........#....."
    ".#...#...#..#..#....#....#....."
    "..#...#........#..#............"
    "##...#.#........#......##.#..##"
    ".##......#..............##.#..#"
    ".........#...#............#...#"
    "....#..#....#...........#......"
    "........#..#....#...##...#....."
    "..#..............#...#.#.....#."
    ".#.......#.#.....#..###.......#"
    "...................#.......#..."
    "........##.....#..#.......##..."
    ".....#....................#...#"
    "...#.#....#............#.#....."
    "#.......#.......#....#........."
    "..#...............#............"
    "##...#...#...#..............#.."
    "...#..........#..#....##......."
    "#............##.##......#.#.#.."
    ".#...........#.........#....##."
    "..##....##.#....#.#.#.##...##.#"
    "........#.#.#.............#...."
    ".#...........#....##...#...#.#."
    ".##...#.................#......"
    "....#.#..#....................#"
    ".##......#........#..#........."
    "...#...............#..........."
    ".#.#..##..##.#........#........"
    "...........#....#.#.#......#..."
    "...................#........#.#"
    "..#............#...#.#........#"
    "....#....#.#.##......#...#....."
    "..................#............"
    "..........................#...."
    "........#......................"
    "......#.#...#.#..##......#.#.#."
    ".........#...#..#.............."
    "..#.......#..........##..#....."
    ".........#............#........"
    "......#..#..#...###....#....#.."
    "#..#..............##.###..##..#"
    ".#..................#.....#...#"
    "........#........#........#...."
    ".........#........#.##......#.."
    "..#.....#.#..###...#....#......"
    "..#................##....#....."
    "..#.#....##.....#......##...#.."
    "...#.......#........##........."
    "#........#...#.#..........##..#"
    "................#...#.#.....#.."
    ".........#..#..#.#..#.#...#...."
    "##....#...##.........#.#...#.##"
    "....#..#.....##.....#.....##..."
    "................#............#."
    "..#..#...#.....#......#.....##."
    "....#.......#...#...#...#..#..."
    "....#..##....#.###.#...#..#...."
    "#..##.....#.....#.##..##...##.#"
    ".............###..........#...."
    "..................#.....###...."
    "..........#....#...#......#...."
    "...#..##.......#......#.#...#.."
    "..#.......................##.#."
    "..#..#..#....#......#...#...##."
    "#.............#................"
    "..........#.#.#.........#.#...."
    ".....##..#......##.#..........."
    ".#.#.#.#....#.#...#.....#.#...#"
    "......#.....##..............##."
    "#..#.......##..##.............."
    "#..#..#................###....."
    ".....#......#.........#........"
    "#...........#........#.#......."
    "#........#.#...#....#....###..#"
    "###..#.#...........#.##.....#.#"
    "..#..........#..#............#."
    "...#....#.......#..#.....###..."
    ".#....#.##.#..###.............."
    ".....#.##.##.......###.##...#.#"
    "..#..##.......###.............."
    ".#.........###..#.............."
    "..................###.....#..#."
    "#....#....#.........#.....#...."
    ".........#.#..#....#.....#....."
    "....##.......##.......#.#......"
    ".....#...#.##.....#............"
    "....#.#.#.......#.............."
    ".##..#.#..#.......##..........."
    "....#....##..#.....##.......#.#"
    ".....##....#..#.#........#....."
    "........#.#.#....#....##...#..#"
    "..#......#.#.#..#.##....#.#.#.."
    "..#...#........#..#..........#."
    ".........#...................#."
    "........#.....##..#....#....#.."
    "#..............#..........#...."
    "#........#.#...........#.#....."
    "..#......................#.#..#"
    ".........#.#.....#.#..........#"
    "......#....#.#.##........#....."
    ".#....##......##..#...#.......#"
    "..#........#...#.##....#..#.#.."
    ".......#.....#..........#.....#"
    ".........#.#..#.........#....#."
    "..........#.##.........##..#..."
    "......#.#..#.....#.#..........#"
    "......#.#.#..#..#.#............"
    "...##.#..#..............#....#."
    "#..........#..................."
    ".#....#..#.#.......#........#.."
    "...#...#......#....#......#...."
    "..#.#.......#.......#.......#.#"
    "...#.#...#........#.....#......"
    "#.......#..#..................."
    "#..#..#.............#..#..#..#."
    "#.......................#....##"
    ".#.........#....#....#........."
    "...............#...#..#....#..#"
    "#.....#.#...#.#.....#.........."
    "....##.#..#...#.#....###...#.#."
    ".................#....#........"
    "####.......##...##.......#.##.."
    "#..#....#....##............#..."
    "..##......#..#........#........"
    "....#..#..........#......#...##"
    "..#.#.............#...........#"
    "#...............#...#.......#.#"
    "#..#.........#.##.#.......#...#"
    "......#.....#.............#...#"
    "......#.##.........##...#......"
    "..#......##.#........#.......#."
    "#..#.........#.##.............."
    "..#....#...#...#..#.....#.#...."
    "................#.......#......"
    "#.....#..............##....#.##"
    "##.....#...#.#.....#..##...#..."
    "#.#............##..........#..#"
    "..#.##......#..#....#.........."
    "....##.#....#.......##.....#..."
    "......#.#....###...#..........."
    "..................#......#....#"
    "..............##..............."
    "......#..#....#.....#.........."
    ".......#........#...#.........."
    "..#......#......##..#.##..#...."
    "..#.#...#...............#......"
    "....#.#.............#.#......#."
    "....#.#.....#......#..#.......#"
    "........................#..#..."
    ".................#...........#."
    "#......#......#.#.#.....##....."
    "..#....##...#.....##.#.....#..#"
    "....#.........#....#.##.#.#...."
    "..#....###....................."
    ".....#.#....#......#....##....#"
    "#.......#...#......##.......#.."
    "#....#.........##.....#........"
    "#.....#...........#..#.....#..."
    ".................#.....#..##..#"
    "..#...#......####...##........."
    "..............................."
    "#........#.....#..............."
    ".#.........#....#.#......##...."
    "...#..........#.........#.#.#.#"
    "......##......#....###........#"
    ".....................#.#.#....."
    "......#..#..#.......#...#......"
    "...##.#.............#.#.......#"
    "..#.#...#..#....#.....#.....#.."
    "..#..#.....................#..#"
    "........#....#..........#..#..."
    "#.##....#..#.#..#............#."
    "..............###.............#"
    ".#.#..........#.#....#...#....#"
    "....#..........#.#..#......#..."
    ".........##.#...#.............."
    "..................#.....#.#...."
    ".#....#.......#.##.#.........#."
    ".##..#...#......#..#..........."
    ".#.........#..........#.#......"
    "#.#......#.#.#.#.......#...#.#."
    ".......#....#.#......#......#.."
    "...#..#....#.#..#..##...##....."
    "#.#.#.......#....#.........##.."
    "#..#....#........###....#.#...."
    "....#..#.........#....#...#...."
    "...#.#.#.#..#..##.....#.##....."
    ".......#.......#..............."
    "#.#.#......##....#............."
    "...#.##........#.....#...##.#.."
    "...#.#.###..........#.......#.."
    ".....#...#.......#.........#..."
    "............#..#...#..##......."
    "...#....#..##.##..........#.##."
    "..................#........#..."
    "....#.##.#.##........#.#......."
    ".#...........##.....##.......#."
    "#...#.........#.....##........."
    "#..#....#.#.........#.........."
    "..#......#.#.#......#.....#..#."
    "..##......#..............#....."))

(defun example ()
  '("..##......."
    "#...#...#.."
    ".#....#..#."
    "..#.#...#.#"
    ".#...##..#."
    "..#.##....."
    ".#.#.#....#"
    ".#........#"
    "#.##...#..."
    "#...##....#"
    ".#..#...#.#"))
