(defun day-24-2020-1 ()
  (count-black (walk-list-of-steps (split-to-coords (input)))))

(defun day-24-2020-2 ()
  (loop repeat 100
        for hash = (flip-floor (walk-list-of-steps (split-to-coords (input))))
          then (flip-floor hash)
        finally (return (count-black hash))))

(defun split-to-coords (input)
  (loop for i in input
        collecting (split-it (string i))))

(defun split-it (string)
  (loop for rest = string then rest-rest
        for to-cut = (if (or (equal "E" (subseq rest 0 1)) (equal "W" (subseq rest 0 1)))
                         (subseq rest 0 1) (subseq rest 0 2))
        for rest-rest = (subseq rest (length to-cut))
        collect to-cut into output
        when (equal "" rest-rest)
          return output))

(defun walk-step (step current)
  (destructuring-bind (x y) current
    (cond ((equal step "W") (decf x))
          ((equal step "E") (incf x))
          ((equal step "NW") (decf y))
          ((equal step "NE") (incf x) (decf y))
          ((equal step "SW") (decf x) (incf y))
          ((equal step "SE") (incf y)))
    (list x y)))

(defun walk-list-of-steps (input)
  (let ((hash (make-hash-table :test #'equal)))
    (loop for steps in input
          do (walk-steps steps hash)
          finally (return hash))))

(defun walk-steps (steps hash)
  (loop for step in steps
        for current = '(0 0) then next
        for next = (walk-step step current)
        finally (flip-hash next hash)))

(defun flip-hash (coord hash)
  (if (equal (gethash coord hash) 'black) (setf (gethash coord hash) 'white) (setf (gethash coord hash) 'black)))

(defun flip-floor (hash)
  (let ((new-hash (make-hash-table :test #'equal)))
    (destructuring-bind (x-start x-end y-start y-end) (get-all-coords hash)
      (loop for x from x-start to x-end
            do (loop for y from y-start to y-end
                     for i = (list x y)
                     for count = (count-black-neighbours i hash)
                     do (cond ((and (equal 'black (gethash i hash))
                                    (or (equal 0 count) (> count 2)))
                               (setf (gethash i new-hash) 'white))
                              ((and (not (equal 'black (gethash i hash)))
                                    (equal 2 count))
                               (setf (gethash i new-hash) 'black))
                              (t (setf (gethash i new-hash) (gethash i hash)))))
            finally (return new-hash)))))

(defun count-black-neighbours (coord hash)
  (loop for i in (make-neighbours coord)
        count (equal 'black (gethash i hash))))

(defun make-neighbours (coord)
  (destructuring-bind (x y) coord
    (list (list (1- x) y) (list (1+ x) y)
          (list x (1- y)) (list x (1+ y))
          (list (1+ x) (1- y)) (list (1- x) (1+ y)))))

(defun count-black (hash)
  (loop for i being the hash-values in hash
        count (equal i 'black)))

(defun get-all-coords (hash)
  (loop for (x y) being the hash-keys in hash
        maximize x into x-max
        minimize x into x-min
        maximize y into y-max
        minimize y into y-min
        finally (return (list (1- x-min) (1+ x-max) (1- y-min) (1+ y-max)))))

(defun input ()
'(ewnwnwsenwnwnwwnweswnwnenwnwesenwwnww
wswwswwnwwswswswswnewswwwewsew
seseeseswwneseseseseseenenesewsesesew
nwwnwnenenwneseseenewnenenenenenwnenenene
enwnenwwnwwwswswseewnwenwwnw
nwsenwsenwewnwewwwwnwnenwwnwswwe
seseseeseeseneseseseeseseesesw
nwsweswsenwnenesweseenenwswnenwenwwe
nwswnwnwneenwnenwnwnwnwnwwnwswswnwwenwnw
nwwnwnwnwenwnwnwnwwnwnwnwswwwnwnwe
neswseseseeewnwsenwswsweseswwswsesw
swnenenwneeneeeeeneeneee
wesewseseseseswnesewseseseenwsenenwse
eneeseswswswswnwsewseenwnwswseswswnwsw
seswsweswneeswswnewwswnwnwnewnesesesese
enenweeneeeeewneeneswese
eneeneeeneeneneenwseeswnenenewsee
enwnwenwsenwnwnwnwnwnwnwwnwnwswnwnwnwnwsw
wwwwnwwwnwwsewwnwnwwnwwsenenww
eeneswneeneeeeesesewenwswnwenenwsw
seneswsewsewneneeeneeeeswwnwee
nwwwswwsewswewwwwswnewswwwwsew
senwseseseseswswseseseswseesesesese
wwswseswwswswswseswenwswwnenwswswswsw
seseneswswseswswneswneseswwswwswwneswwsw
nwswwswswseswswswwnwsweeswnwsweswsw
seseesesesenwseeseseseseseswse
neenesweswnwnwwwnwsenewnwnewwswwnw
senwseswseseseswnewsesweswswsweswse
wwnwwnwswwnwwwwwnwne
sesesesenenewsesesweseseswsewsewse
seswneseseswsewnenweswseswsenweswneswsesw
eneswnewneneneeneneneeneneneeneene
weseswwwwnwnwwwseswwneeswwww
seeeenwseenwnwswseeeseseeseeeee
sesesenwseswneseeenwseseseswswwswesenw
nwnwenwnwnwwswnwnwnwnwwenwsenwnwnww
nwnwnwneneswwnenwnenenwnwsenwnenwnwnenwne
senewnwneenwwswwnwnwneenenwsw
neneneneneenenwnenenesenenwnesenwswnene
neswswsweswswswswnenwswwswswewneswsw
sesesesesesenwswswsesesesesewswnese
eneseeseeeseeswsesese
wwseseseswswseseseeswwseswnewseenesw
eeeeeeeeswnewwenweewwnwswse
nwwswenwnwswenwnwnwwnwewnwnwwnwwnw
nwnwsewnwwnwenwnwnwnwwnwswnwnwnwnww
nwnwwswswnwneeneswnwnwenwenwnwnwnwwnw
neswneneneneneneenenesw
newenwswseeweseesweweseneewnesee
enwwswwwnwwnwseswwwwwnenwnenesw
seswnwseseneneseeswswswnewwswswew
wnwsewwnwwenwseeeww
swswswswswnwswswswnesenwswsesweswswwswsw
wneweswswswswswswswwwswwsewswnwsw
swwswswneswswswwswwneswswswswswswswesw
seeseseeeenwenweeenwsenweswswnwse
swswswswswswswswwneswneseswwswwsww
neeneneseneewsenenenenewnenenenenene
nwwnwseeneswswnwneswneeswseenwnwnesww
wneswseswswnwswwswwswwswwnewenese
seewesesesesewseseseeseseseneseesese
sesenwnwswneswswseswwsewneswneseenenww
swswewseseswnwswseseseswsewswnenwsesese
swwswswswswswwnewswewwswswwswswe
wsesesewneneeswsweeneswwnwswsw
neswswswswseswseseswswseswswneswsw
enwseewsweseeeeeeesesweneseenw
wewnewwwwwwnewwwswwwwsww
eswnwwswsenwwwenwswwswwsenenesw
ewnwnwswsenwnwnwnwwwnwenwwnwnwnwnw
wnwenenwenwnwsenwnwswnenwnewnenenese
sewwenwwwnwnwwwnwnesenwnwnw
eeeeeeeneeeeweeeseesw
wswsewnenwwwnwnwnwnwnwnwnwneswnwww
swenweeseeeewneneeeenenweese
neeesweswwneeeeeneeeneesenee
swwwneseswswswwwswsewswswnw
eswenenwnwswnwwnwwnwsewnwneswnwneswnwne
nenwneneneesenenwnesenwwnenenenenwnww
wwenwswswwswwnwsenwwwseewwswwww
swwswwwswwnwnwswweswswwswswswwe
eeneeseneseeeesweeeesesw
nweesewnewneseneneneneneseeeneenene
enwweswwseseenwswweee
wwwnwnwnenwwnwswnwnwwwswwnwenwwnw
nwseneneneeneneeeeneewswswwne
nesesesesweseeseseseeesenwse
eseesenwnwswseneswseseeese
nwseswneswnewnwsene
eeeeeeweseseweeneeeeesee
nenesewnesenwewwwsewwwewwnesesew
wwnwswseswsenesewsenwseeee
seseseswseswnwseseneseseswsenewswswsesw
seenwenweeseseesweswesenewewee
swneenenenenenwnwnenwnenenwnwnenwnwne
sewwwwwwsenewsewnwenwswswnenwnwnw
seseseswsewseseseewnewsesenwnesenene
nwnwnesenweswswnweswnwnenenwnewnwnwwnwne
swenenwseswnewweswnenenwewnwnewnw
nenenenesenewenenenenwneneswnwneenwwne
nesewwnwnwswseenenwnwwewnesenwnwswsw
eeewenwseeeseesesweseenwnwee
nwwswnenwsenwwswneewwwwwnwwswnwsenw
nwnwwnenenewnenenenenwnwnenwneneseenene
nwswswswswswswswswswswswseswswseswesw
swswseseseeseseseseneesesesewsenwnwseswse
wswwnewwwswwswwewswwwseneswww
wwwwwwwswwew
ewewnwswnwwswnwwwwnwewwwnwwnwnw
nesenwneneeneneeneeneeeneneeewne
seseseseseswwsesesesesenese
seswsweneswseeswswwseswnewseswnwsw
swweneswneneeeneeeeneeeenw
wnwnenweswewnwwwwnwswsewnwnwnwwwnw
swseneeeneswseewwneseweesewwese
nwwnwwnwwnwnwwnwwnewwnwnwwnwsesese
senenwswseswseesesenesesesewwsesenesesese
nwnwnwnenwnenwnwewnwneswwnwenenwnwnw
nwenwnweseswwwnwnwnwnwnenenenwnwnwnwnw
nwneneneenenenwneswnenenenenwnenenene
swsweswwswneswswswswswwwsww
wwsewwwnewwwwswnewwwwwww
nenweeewswneneswneeeseesweseswewne
sesesweeeenweeeeenwswwnwwee
neeewwnewesewwwnesewwewseenww
enwwwswnwwenwwnwwwenwwswwww
newnwsenwwswwswnwsewnenenwnwneesenwsew
nenenewneeneneneeneneeswnenenwswnee
neenewsenenwneneneswsenenenenwneneswnw
eswneesesenwwseseeeseeewnesesesese
eeswnwnwnwnwnenewnwnwsenwnwnwnwnwnwnwnw
seswsesesweneseswswwswwseswswseseseswsw
swnwsesenwwnewnwwwwnwnwwenwneneswnwnw
wsenwseeseseseweneseeesese
nwsenwnwwnwewenwnwsw
seeswseneeneswsesewseswneseseseseeenw
nwswswswswseeswswneswseseseseswswseseswswne
swnwnwswenwsenwswewwneewwswwnwne
wnwwsenwenwswnwnwnwnwnwnwnwnwnwenene
wwnwswnwwnwwwnwwnwwnwnwwenwnwwse
nwneswenenwnenenenenenenwneswneswnwnwnene
neneenenwneneeeswnenewneeseneewene
ewwswsenwwnwsenwnwnwnwwwnwnwwnwwse
eeeeneseeeeeenweeweseeneee
nwnwnwnesenwnwwnwneswnwnwnwnw
seseseseewesenewneseseseneesweesesw
weswnesweswwnenwswseneswseneswwswwsw
nwneeswnenenwnewnenenwneswswneneesenwne
swnewweewswwswswwnwewwwswswnwsww
swswswsenwswswswswsweswswwseswswneswse
weeeseeeeewneeeeneeseseewse
newseneneneswnenwneneeseseewneewsee
neneenwnewnweseswwneeswnwnwswnenwnwnw
neesenweneenweenesesweswenweeew
eeseseneseeseseseeeewenwseeesee
weseseswswswswswswseseseseswse
eewswwseseseseseseeeseseenenw
swwnwsenwnewswnwswsweeswwwwswsenese
neneewneeeeeeseeneeeneeeenw
swseswsweswswswwswnwenwswswswswswnew
swswswswswswnesewswseseenwseswswswswswsesw
wnenesweesenweeswwnwseenwnesweswee
nenwnwnwswswnwnwsenwwnwnenwesesenwnewswe
swsenwesesewswenwwseneswswseneswne
nwnwnenwnwenenenenwswnwnwnw
eeneneneeswnweneeswswneeneeneeene
nwswswewwnwswwwswwenewseswwww
senwseseswwseesenesweswnwseseseeenwe
eweneeeeseneeeeee
wwwswwwwesewnewwww
nwnwnwnwnwnwswnwnwnwnwnwnwenwswnwnwneenwnw
eeeseenweeeeeeeseewnesesewe
swswewswswswswswswswseswenwseswswnesw
nweswenwnwesweseseseeeeee
nwenenenewswwsenenwnwnwnenenwnenw
nwneneneswneneneneseneneneneneneneneene
swswnwwswwwswsweswsweneswswswswwswsww
swswneenenwwnwwsewswenenewwseseswnw
nenenesweneeswneeeeneneswenenenenee
nenwseenwnwneswnenenweeneswnwnewsenesw
swwwswwsewwwswswnewwwwwswswne
weesesenwenwneswneeesweweneswnew
nwenenesenwneneneneswswnwwneswnwnwenwne
senwnwwnwnwnwwsenwnenwnwswseneww
senwseseseseseseneneseseswesenwswsesesesw
wenweneeseeseweeeweeneeeenee
seseswewneseseseesese
seeseeewseeeeseewe
eeswnweeeeneeeneeee
eneweeeeeeeenee
sweseswswswswswswswwswswseseswnwswsesw
nwwsewnewwenwnwnwswswnw
weseswsewseswnwswnwsesesenwneseswsenese
newnwneeenewsenwswnwnwnenwnewnenenw
nwsenwnwnwnwnwnwsenenenwnwnwnwwnenenewne
wneswnwwwwsewwsewwwwwwenwe
wwswswswnenewswwswewwsenwweww
enenesweeneneeneeseeneeneeewnwe
neneneneneswnenwnewenwnenenwneneenenwne
nwnweeeswneseseswnwsesweseeseenwewe
ewnwnwswnwnwnwwswnwnwnewnwneseewww
wenwwsenwswwnenwnwnwwenwwnwnwseww
enwswnwnwnwsenwnwswnwnwnwnenwnwnenwnwnw
nwnwenwwwnwnwnwwsenwnwnwnwnwnwnw
nwswswnwnwenenwnwnwnwnwenwnenenwnenwnenw
nwwswwswswneneswsewwseswwwsewswnesw
nwsewwwwwwswwnwenwwwwwwnew
nwnwnwnwnwnwneneswnenwnenwnwnwne
nenenenenesesenenenenenenenenwnwnenenenew
swswwewswwnenwnewwwsewsw
nwnwnwnenwsewsewnwnewwneswnwnw
swswswswswseswseseseneswwswseneseswsesw
nwnwnwnwnwnwenwnwnwwnwnwnwnwwwenwnwsw
eneeeeeeeeeenesenenenenewewne
eneewewwseewenwsweeneeeesene
sewseseswswswseswseseseneseswseneswsesesw
ewwnwwswswnwnesesewnweesweseeenene
wswnewsenwwwwwnwnwenwwwwwwwwnw
eeseeneeweneeeeeeweeeseee
wswswswswswswwswswswnwswewswwnwswewsw
wseseeesenwseseeeeenwsesesesw
eseesewnweeswneeeewese
sweswswswnwswswwswwnwswwewewnewsew
eeeneeeneeneenwenwsweneswneeenee
swenwseneweseneneneeewswewnwneeese
wwnwnwsweeeneswnewseswneswswneese
seswsesesenwseseseseswswesenwseseswsese
senenenenweeeneeneeweeneeeesw
seseseneswseseswwswseseneswswseneeswse
wwswwwenwswwwswseswww
sweswswseseswswnwswseswsw
eeeweeeneneweneweseeneee
seswwseseneneswswseswneeseswwswwsesw
nwenenewneswnewwnewnesenwenwnesenwne
nwnenwwnwnenwseswsenesenenenenwesenwne
neswswwswwswwsww
nwwsewwwsewenewwswwnewwwswwse
nwsenwnwsenenwnwnwnwnwnwnwnwnwnwwnwnwnwnw
nwsenwwnwnesenenenwnwnwnwnwnwenwnwnenene
seseseswnewseseswwneneseswe
neswwswswswswsweseswsesesesenwseswnwsese
eseesweeeneeseseseenwseneseseswse
wswwsewewwwwwwwwwswwwwnew
newnwwswnewwswwwwswswsesewwsweswsw
eeeneneneneneseneswnwneenenenenenwne
nwswnwenenenenenwnenenewnesenenwnwnwne
wsenwwwwewwnewwnwsewwswwnwww
neseseewneeesenweswneeesesewesese
nwnwnenenwnwnwnwnwnwnwnwnwnwnesenw
nwwwnwswnwwnwnwenwwnwwewwwwse
neseswswswseswswswswswsesw
sweeneneeenewnwswneeeneneeenee
nwswenwenwswnenenenwseesenwwswswese
eeeswseeeneneeewseesesweeeee
nenenwnesenwsenenenenwnwneseneneswnenese
enwnwnenwnenwnenenewnwswnwnenwnenwnwnesw
eeeeewseeenwsw
nesenenenwwenwnwnenwnwswnenesenwnwnenw
sewswneneswsewww
weswswswswswnwwswwewwwwswswwswsw
nwswnweswnenwnenwnwnwnwseene
enwenwnenesewneeeswnewseenenwnee
eeeeseenwneenenenenwnenweeeseswse
nwseswseswswnwswesenwesesene
wnwwnwwewnwnwnwwwwnewwswnewwnwse
eswseswswswwswswswswswswswnwsw
wwneswwwewnwswewwwswwwwsew
seseseseseswsewneswseseseneswseseswswnesese
nwneswswswsweswewswswswwseswswnwswswesw
neswnenenewswnwswsenwnwenwnenenenwnwnwswne
swnwneseneeeneneeeeneneneswneene
eseeeenweeeeesweese
seseeseseseseseseswwseseseneesesesese
eseseeseenwseseseenweseeseenwesee
nwswneneneneneneeneneeneeneesenenwse
nesewnenenenenenewnenenenenenenenesene
nenwnwwsenwwnwswsewnwnwseeswenwwnww
swseswswsenwseseswsweseswswswsw
neswswnwnwneswnwswnweneswnwweenwnwswne
swswsweswswswswwswnwswwwswwswseeswnw
nwnwnwnwnwswnwwnwnwnwenwnwnwsesenenwnw
nwseneseswseseswswnwseseswenewswsesese
neneeeswnenwneneeseswneneeneswwnene
seneswswswswswnwseneseseswswswseesewsee
neesenenenweeesweenwww
wseswwswewswwwswwwswwwwwnwnewsw
swnwswneswswswswswseswswswseseswsweswsw
seswseswswswsesweseswwenwswsesw
sewseseseseseeseseenewsesesesesesesene
sewsesesesesenesenesewseseseeswsesesese
enenenenenenenenenenenenenewnenesene
sewwnesewwwnweneeewnwwww
enwswewswwnwnwnwnwwnwswwnwnwenwnwenw
wwwweeswnwwsenwsenwnwwswnwenwwnw
nwnwneneeneseenenwwneswnenwwswnenwneenw
nwnwwwnwswnwwnwwweneswew
wnwnwwwsesenwnwwwnwnwnwwnwnwnenwnwne
seswswwsewswneswswnwswsenwneswseswswesw
nwnwnwnenesweswnwnwnenenenenwnwneeswnwnw
newnwsewsewwswswwswwswneewwnew
wwwnwwnwwwsenwwnwwwwewnwwew
seneneswwnewsenwswnesesewnenwsee
wnwnwnenwnenwswnwnwnenweswnwnwnwsenwne
nesenwswneseswnwneswswswsewse
nenenwneseneneeeswnw
eeeweeeeesenweeesenw
wswsenwnweeeswnesenwsesenwwswesww
nenenwsewneswnwnwneenewnenwenenenwne
eeeseeeseeseeeewewseeeee
neeseewseeeeewneeeeeeesenwesw
swewseeneswswswswwnwswswwswswswwnw
swswswswswnwenenewswswseswswswnesw
wneswseneneneenenewnenenene
nwnwswswnwenenwnene
eeeeswneneswwsenweneseeenweene
seseneswswnweswswsenesesew
eesenwnweseeeseeseeeeeseseese
wwwsewnwewwwswswwswwwwwswneww
neseeneneesesesesweeesesewsewsee
nwnwnenwwwnwesenwwnwnwwnwwnwwwnwnw
newseseswseswwesesese
sweenwnewsenwnwwwwwwswe
wnwswneswnewnwwnewnwwwwwwswww
eeeseseseseeeseneeeswweenwnwse
neswswswswnenwswswswwswswswswswseswsww
eeeneseeswneenewnweseeneeeneene
enesenwsesesewseseseseweseseseseseee
wswseenwwnwwnwnwnwwnewenwwwwnw
wnwwwwsewswwswenwwwswwwwsww
nwnwnwnwnwnwnenwnwnwwsenwwsenwnwnwnwnw
seseeswswnwswsenwnesw
swsesesesewseseneswswseeseswsewsesesw
nenewwswewswwnwwsesweswwwswwwsenw
swswswswswseswseseseseswsenweneseswsesesw
wswnewwwwwwnwneseswnewswsewwww
wswnwsweseneseseesesesesesesesesesww
neneneneewneswnenenenenenenenenenenese
wnwnwnwnwneneesenwnwewnwnewsenwnwnw
neneneeneseswenenwneneeenweneene
neeneneneswneneeeswneenenwneeneene
nwnenwnwnwnwnwnwnwsenwnwnwnwnwneswnwnwnese
swwenwnwnwnwswwnwnwnenwnwnwenwweww
wweneeneeneneneneneenene
enwseeswesenwseswee
sewnwswwwsenenenweswswnwnweeeeee
swenwsenenwnwnwwwnwwnwnweenwnwwnwnw
newseneswnwwwwswwwwwwewnwswnw
wwsenwsweesweseeenewsenwnwseee
neneswenwnwneneneneneneneneneneswneswne
seweeesenweeseneesweeeeseseee
nwwwnwwwnwnewwwswwnwsewnwwwwew
nwnenwsenwnwnwnwnwnwwnwnwenwnwnwnwswnwnw
neneweswnenenenene
eeeeseeewenweeeeeneneeee
senenenweswneneneswnee
swnenwswnwnwsenwwnwnwwnwwwwnwewnwenw
eswnwnenenwnenenenwnenwneswnwnenenw
nesenwnwnwnenenenenewwnenwsenenenwswne
wweseswwwenenewnwseeeswenewnwsw
swsesweeeseswnesewnwenwseseneenenwesw
swwswesenwsweswswswswseswseswwswswneswse
ewwnwnwswwwwsenwwnwnwwwnwnewseww
nwnwswswseneswseseseswseewswwseseneswse
newswsweswswneseswswswswswswnenwseswsesew
ewseenwswnwwnwnenwnwneneneeswswenene
seseswswwseseseswswswseswneswswneseswsese
wswesewwwwnewwnwnwesewnwwsesw
nesesesweseeseeeseswesenwenwewse
neneneswnwnenwwsenenenwnwnenwsenwenee
eeeweeeeeeeeweneeeewese
nenwnwwnwwwnwnwwsesewwnwnwnwnwwnww
seseeseesweesesesenweesenweesese
eweenwseeeeeeeeweeeewe
nwneewneneneneneswnewnenenenenenwenene
swswneswwswwwnwwseneseswwsw
neneenwewnwswesesene
seseseswswnwswseswsesesesesewneeswnwse
wswnewnwwnwnwwnwwwwwnwwewswnew
wnenwnwnenenenwneenwnenenenenenesewnesw
nweswenenwwnenwnenenwneseswnenww
swwnwnwswnwseneswsesesweseswneee
seewnwwneseneswseswewnwnwnewwnwwnw
enwseseeenweeseseseee
seswseswseswnenwsenwseswswseswwseseswswsw
seesesesesesesesewsesenenwseseseneesesw
eeeseenwseeeseesewesesesenesese
swsweswswswswswswweswswswswnwseswswswsw
wwnwsweswswwnewwswwswswseswswwww
swswswseneewwswnwwwseeswnwnwswnwswse
ewseeeeeenweeeeeeseeesese
ewwwwswsewwnwswnwwwswwwsenesww
wwswswwnwwwwswwwswewswwwswse
esenwsesesewsesesenweswseseswseeesee
nwnenenenwswnwnwewneseneenenenenenenene
esesesenwwseseseeseenweseseswswe
wwnwwsewwwwwswnwseewwnwww
seneneseenenwwewneneneenewneeneee
nwneenenenwnwneswswnwnenwwneenwnwsene
seseeeweseseseseeneweeeseeesese
nwwnwnwnwnwnwenwnwenwnwwnwswwwsenw
swnwnwenwnwnwnwnwnwsenwnenw
swwseswswswswnenwswswswswswswswswswswsw
newwwnwsewewnwnwsewwnwnwww
eeeeeseenweeeeeene
swwnwnwsenwwnwnwwewewwswnwseew
ewnwnwnenwsesenwnwwswwnewwnwwnewsw
eseswnesewsesenweneseenwweseseese
nwnenwnwswwnwenwwswwwnwnwnwwnwwne
neneeeeeeenweswnee
nwnweswnwswnewswnwsenwweswenenwesw
neenewneewenenenwsee
nenwnenwwnwnenwenwnenwnwnenwnwnesenesew
swswswswnwswswwswswseneswswwswswswswsw
newseeswwseeneeeeeneneneneneswwe
neneneneneswneeewenwwsewnenwnwswswne
eneeneeneneeneeseeeewnesweene
nwnwneswwseswswswneeweenesewnwseswswsw
nwseseswnwseenwsenwwsenwnwnwnwenwwnenew
enenwwswsewwwneeswnewswsewnew
nenwnwnwnwenweswsenwenwnwswnwswswse
eenwwnesesenesesesewsesesewsesew
nwwnwsenwneseneneswneneneswenwsw
nenwnenesenwnenenwnwnwsenesenenenwnenenwnw
seeeewswnwewseneewne
wswwswsweswswswsweswsw
enesenwseseswesenwsenenwswenwsweswsesw
eeswnenwnenenenenwneneweseswne
wwnewwwwwnwewsewwwwwwew
newseseneswnwnwnenwneneweseseewnesw
seeewsenesesweseweeseenwnwesene
nwnwnenenwnwnwnwsenenwnesenenenwnenwnwne
nwwnweswwwwnwenwwnwnwwwweswe
wsewwwwwnwwwsewsenenewwwwwswsw
swneneeneeneswnesewnenewseenesewe
nenwswswswswswswswswswseswwswswswswsw
seseewesweseseeseenweeseseseesenwsw
esewneseseneseseswseswwswseswseseswsese
nwnwnwnwneeweswnenenenwnwenwswseew
wnwenwneenwwnwnwwnwenwsenwwnwnwnw
swswneswswswseswswneswswwswnwswswwew
nwnenesweneeeeneeswnweeeeene
nwswswwswswseseneswswwnewsweswswswsw
wswwwwwewwwe
nenenenesewnenenenenwnenenenwwswnwsene
eeseseenweeeesweeeeewee
wswnwneswnwwsewwswswswwweswseswswew
wsesesewenwswseswseweeswswesesw
senwswseswswseswswnwswnwnesweweenesewse
weneneneeneneswneeeenenwneswswnee
nwnwnwnenwnwenwnenenwwnwsewsewenenwnw
nenenenenewneneneneneneneneneene
eswseswwwwwwwnwswsewweswnewe
wseweenweeneewseeseswesese
wnewenwsweeneenesenesweneeneee
nwnwwnwnwwwnwnwsenwwnewwenwsenww
swneseswswswsenwwenwswwwswswneswwwnew
esenwenwswwswenwswsewnwswswswenw
swnwewneeswswswswswswswnwseseseseesesw
nenewnwnenesenenenesweneneeneneneneenene
neswsewnewswswswwseswswneswwswseswswsw
nwswwnwnwenwnwnwnwnwnwnwnwwnwwnwnw
seseneswnwneswsesenwswnewnwnwsenwewnenene
neneeesesenwneneeeneneeswneeenww
eeesweeswseeseweeeneseeeeenw
seseseeeswwswwneswswwnwnwseeswese
swnwnwenwnenenwnwneenenwnenwnwnenenwswsene
wwsenewnwwnwwwwwwwsenwswnwwww
enenewnwswseeneseewneseseswesesee
nwwwswsewwnwwnwwwwswswswseswswe
wseswwseswnwseeseseeseseseseeswne
enwsesweeeeeneeeeeeeeeenw
seseeseseseeseswsesenwweeseenesenwsesw
senwneseseseswseswseswseswnwsesesewsesesese))

(defun example ()
  '(sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew))
