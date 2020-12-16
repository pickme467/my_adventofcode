(defun day-16-2020-1 ()
  (validate-tickets (input)))

(defun day-16-2020-2 ()
  (let ((my-ticket (second (parse-input (input)))))
    (loop repeat 6 for i in (allocate-correct-ticket-part (map-ticket-part-on-validator (input)))
          collect (nth (first i) my-ticket) into output
          finally (return (reduce #'* output)))))

(defun validate-tickets (input)
  (destructuring-bind (rules my-ticket ticket-list) (parse-input input)
    (declare (ignore my-ticket))
    (reduce #'+ (mapcar #'(lambda (tickets) (sum-invalid tickets rules)) ticket-list))))

(defun sum-invalid (tickets rules)
  (reduce #'+ (remove-if (lambda (part) (validate-ticket-part part rules)) tickets)))

(defun validate-ticket-part (part rules)
  (some (lambda (r) (funcall r part)) rules))

(defun allocate-correct-ticket-part (index-lambda-list)
  (loop for (index lambdas) in (sort index-lambda-list (lambda (x y) (<= (length x) (length y))) :key #'second)
        with to-remove = ()
        collecting (list index (first (set-difference lambdas to-remove))) into output
        do (setf to-remove lambdas)
        finally (return (sort output #'< :key #'second))))

(defun map-ticket-part-on-validator (input)
  (destructuring-bind (rules my-ticket ticket-list) (parse-input input)
    (let ((group-fields (apply #'mapcar #'list my-ticket (remove-invalid ticket-list rules))))
      (loop for fields in group-fields for index = 0 then (1+ index)
            collect (list index (find-rule-for-fields fields rules output)) into output
            finally (return output)))))

(defun find-rule-for-fields (fields rules found-list)
  (loop for rule in rules for index = 0 then (1+ index) with found = (mapcar #'second found-list)
        when (and (not (member index found))
                  (every (lambda (part) (funcall rule part)) fields))
          collect index into output
          finally (return output)))

(defun parse-remove-invalid (input)
  (destructuring-bind (rules my-ticket ticket-list) (parse-input input)
    (declare (ignore my-ticket))
    (remove-invalid ticket-list rules)))

(defun remove-invalid (ticket-list rules)
  (remove-if (lambda (ticket) (not (valid ticket rules))) ticket-list))

(defun valid (ticket rules)
  (every (lambda (part) (validate-ticket-part part rules)) ticket))

(defun parse-input (input)
  (destructuring-bind (header nearby-tickets) (split-by-word "nearby tickets:" input)
    (destructuring-bind (rules ticket) (split-by-word "your ticket:" header)
      (list (parse-rules rules) (mapcar #'parse-integer (split-by-word-all "," ticket))
            (mapcar (lambda (x) (mapcar #'parse-integer (split-by-word-all "," x)))
                    (split-by-word-all (string #\linefeed) nearby-tickets))))))

(defun parse-rules (rules)
  (let ((rules (split-by-word-all (string #\linefeed) rules)))
    (loop for rule in rules
          collect (destructuring-bind (range1 range2)
                 (split-by-word-all "or" (second (split-by-word-all ":" rule)))
                    (make-lambda range1 range2)))))

(defun split-by-word (word input)
  (let ((pos (search word input)))
    (if (null pos) (list nil input)
        (list (string-trim '(#\linefeed #\space) (subseq input 0 pos))
              (string-trim '(#\linefeed #\space) (subseq input (+ pos (length word)) (length input)))))))

(defun split-by-word-all (word input)
  (loop for to-split = input then rest
        for (part rest) = (split-by-word word to-split)
        when (null part)
          collect rest into output
          and return output
        collect part into output))

(defun make-lambda (rule1 rule2)
    (destructuring-bind (start1 stop1) (split-by-word-all "-" rule1)
    (destructuring-bind (start2 stop2) (split-by-word-all "-" rule2)
      (lambda (x) (or (and (>= x (parse-integer start1))
                           (<= x (parse-integer stop1)))
                      (and (>= x (parse-integer start2))
                           (<= x (parse-integer stop2))))))))
(defun input ()
  "departure location: 33-679 or 691-971
departure station: 48-646 or 671-966
departure platform: 37-601 or 619-950
departure track: 41-863 or 875-973
departure date: 37-145 or 168-965
departure time: 26-246 or 257-972
arrival location: 30-542 or 556-960
arrival station: 30-75 or 89-954
arrival platform: 48-274 or 299-958
arrival track: 41-561 or 567-957
class: 40-237 or 243-952
duration: 33-317 or 336-972
price: 47-365 or 381-957
route: 29-415 or 435-951
row: 45-762 or 784-972
seat: 34-888 or 914-968
train: 50-502 or 513-960
type: 45-802 or 825-961
wagon: 44-458 or 475-963
zone: 28-721 or 735-972

your ticket:
53,67,73,109,113,107,137,131,71,59,101,179,181,61,97,173,103,89,127,139

nearby tickets:
556,988,567,847,219,631,519,625,634,639,362,750,703,882,798,931,177,888,59,312
190,445,591,270,798,677,575,789,880,880,748,788,6,597,475,746,172,221,925,97
437,141,105,837,357,587,363,309,185,887,181,833,929,945,716,445,247,800,795,65
593,594,96,761,314,258,851,560,224,788,403,635,829,946,857,572,383,67,944,160
939,408,531,121,885,513,556,762,757,261,847,596,713,527,645,84,693,348,847,487
105,877,638,130,451,336,197,188,711,695,320,245,879,57,518,853,354,57,705,217
274,835,350,884,258,637,834,145,199,269,943,508,405,231,645,624,385,622,210,537
313,365,269,879,96,718,887,571,383,635,894,484,266,389,628,634,458,271,752,408
591,449,223,337,492,75,922,579,489,60,738,775,364,696,355,174,413,216,354,175
586,837,358,790,108,268,739,180,215,477,831,700,570,940,61,382,196,982,107,312
568,596,52,791,556,947,145,72,402,384,746,941,95,312,644,907,916,452,398,184
142,478,886,862,213,788,315,704,192,707,699,335,862,485,212,118,221,346,344,398
91,21,269,525,136,624,340,944,458,99,792,488,716,560,693,746,691,876,698,538
190,227,645,202,383,916,797,88,802,675,271,793,785,943,539,446,678,735,229,218
147,600,245,521,587,642,354,119,745,878,744,259,700,110,124,486,630,701,707,265
715,757,411,397,699,755,83,637,640,120,260,89,264,264,496,518,620,837,245,916
741,695,405,860,556,55,111,574,831,716,920,359,638,873,678,601,412,943,360,484
192,139,396,360,439,540,103,481,679,340,542,597,947,538,357,560,581,182,608,451
117,346,940,631,354,355,65,116,357,787,878,559,789,784,262,701,453,340,255,558
743,304,621,312,770,691,138,261,695,494,633,833,747,624,479,145,313,760,536,600
480,497,784,853,846,134,217,204,859,170,493,535,559,539,133,182,452,872,580,532
860,861,415,351,933,920,385,107,109,266,990,264,200,568,881,640,798,753,490,582
878,844,938,274,232,307,196,109,719,102,568,761,997,93,114,168,587,529,268,708
736,522,134,704,357,441,676,501,857,531,191,917,542,66,267,220,627,982,172,569
171,345,89,943,258,678,939,738,397,672,396,530,194,408,614,123,499,883,393,717
560,887,761,937,520,702,209,437,629,797,860,302,578,794,212,103,773,186,338,539
633,586,66,835,533,801,205,50,395,746,336,359,751,560,573,98,74,310,722,312
353,354,856,582,835,755,447,879,518,71,447,861,637,586,81,938,339,259,57,488
679,884,542,943,128,93,740,110,830,748,784,468,949,789,226,880,826,172,748,713
542,178,826,494,941,880,361,404,89,50,305,223,914,206,716,53,608,878,942,193
404,751,924,842,208,831,176,761,225,695,129,704,829,750,195,746,492,702,625,774
104,675,103,476,107,495,184,127,353,588,735,476,415,553,645,360,920,300,222,581
361,595,320,592,798,177,590,311,836,140,634,364,199,263,309,677,388,453,784,525
405,534,694,305,756,752,337,645,343,578,825,406,74,184,831,634,736,212,577,9
403,59,268,259,411,336,558,841,359,406,96,702,849,784,185,841,316,924,789,11
270,305,937,393,196,699,712,129,567,485,535,717,590,659,801,595,94,624,193,244
357,230,257,226,393,834,63,122,385,914,583,826,829,162,642,302,270,72,884,125
119,797,408,302,358,920,588,525,72,52,698,60,266,447,307,486,856,455,80,392
94,885,239,399,759,104,861,486,692,595,343,209,840,178,446,355,521,193,572,739
946,718,918,179,173,585,946,120,827,797,721,188,134,949,686,494,708,572,620,497
832,841,850,497,590,573,530,638,304,914,106,531,833,457,704,580,749,632,551,593
515,68,397,128,232,382,300,569,949,859,235,70,527,475,510,834,232,265,317,207
181,442,204,793,455,439,90,561,998,489,705,860,840,391,90,479,190,436,391,216
674,829,106,259,861,635,587,509,395,458,716,756,671,145,191,215,529,173,115,936
691,557,450,519,212,184,89,639,450,799,679,802,540,504,171,676,105,948,140,761
130,341,915,105,588,861,934,643,120,92,752,119,111,310,888,710,408,897,495,220
940,113,619,789,129,853,314,299,800,568,124,595,588,455,394,115,755,87,518,90
884,620,942,949,226,634,945,920,918,764,447,75,620,757,619,752,135,599,125,583
220,307,629,757,495,878,887,140,792,558,58,382,393,364,259,206,811,625,626,273
16,571,58,171,497,578,351,721,852,57,760,626,832,66,497,404,944,847,211,535
75,751,410,71,304,215,145,163,66,404,121,875,62,477,493,412,103,196,938,131
883,834,262,203,68,209,177,199,340,645,491,808,718,739,569,207,716,196,748,111
933,706,635,561,941,575,414,736,244,795,398,517,834,61,111,348,737,207,482,253
739,693,845,718,576,179,312,862,171,623,308,621,494,302,92,89,242,588,521,207
182,644,924,828,826,698,319,591,745,712,121,235,63,365,787,523,75,96,213,311
711,834,244,348,197,381,746,849,576,406,839,592,755,221,314,522,210,392,880,242
127,627,826,786,620,339,880,344,916,344,542,209,919,584,443,210,884,826,994,640
710,71,101,110,185,90,232,557,583,209,679,439,264,431,834,70,792,129,310,795
101,758,596,311,208,398,831,644,452,557,492,197,522,825,323,386,197,527,599,534
119,214,703,590,596,449,526,576,542,932,349,719,531,256,558,478,641,212,414,702
710,314,578,300,172,527,383,938,531,639,479,984,572,107,791,536,791,850,120,634
778,527,301,677,557,695,629,310,534,119,589,760,363,56,212,271,526,794,595,877
944,229,883,317,94,267,845,692,599,58,408,886,573,926,645,317,658,171,691,236
709,338,494,919,876,454,642,169,139,489,398,640,497,539,234,383,801,66,581,321
492,538,230,841,258,108,937,3,787,111,740,341,936,597,304,841,93,513,739,478
640,799,139,336,113,695,210,231,639,584,557,189,304,224,408,481,627,240,940,928
480,134,348,708,583,531,403,145,561,940,949,287,337,185,760,394,134,589,531,128
110,400,168,852,578,768,392,838,693,261,435,832,497,230,114,223,397,170,521,561
386,595,743,300,643,686,357,527,761,209,477,578,497,354,218,858,644,754,693,802
475,389,921,993,202,855,798,348,72,212,397,918,131,599,937,178,63,186,228,135
792,721,568,481,475,883,500,533,398,529,631,195,354,202,502,307,119,581,658,573
351,53,339,204,176,203,62,385,529,226,742,489,119,933,306,936,55,995,207,925
169,755,718,68,567,800,625,786,756,412,101,64,99,62,629,542,155,513,696,447
246,354,625,401,490,695,641,517,410,309,178,92,743,214,21,717,71,263,856,542
107,214,403,573,392,708,132,261,305,708,436,399,63,68,95,384,266,449,868,879
204,336,884,930,439,917,610,210,133,477,181,268,583,584,173,198,696,142,393,204
203,268,219,96,189,201,533,832,139,559,274,408,71,66,221,699,113,406,429,194
445,587,482,694,571,136,309,444,839,720,87,200,98,919,56,260,932,404,112,218
57,360,877,300,389,835,343,112,172,527,51,575,586,231,862,929,727,246,177,231
65,919,315,640,534,586,700,352,54,703,396,476,636,494,528,313,901,168,140,69
581,862,859,827,114,850,359,640,236,257,95,243,705,357,851,556,271,211,320,90
195,90,310,560,600,692,802,177,402,232,601,358,623,443,919,989,272,588,410,130
589,200,550,787,757,915,124,269,405,364,502,133,928,415,571,568,413,797,453,581
213,112,489,738,760,887,457,268,447,528,698,70,224,733,479,630,131,838,500,802
206,596,130,445,523,204,115,104,641,443,678,57,309,696,341,452,801,608,939,790
172,121,596,700,126,718,629,357,572,381,181,116,497,349,52,506,825,257,800,343
145,144,559,143,863,757,880,850,144,398,929,132,23,625,191,930,271,920,355,362
623,921,755,539,481,707,853,128,180,720,523,94,458,314,394,839,939,209,807,102
918,927,573,83,209,404,212,437,96,406,827,834,526,934,717,114,597,691,494,264
217,403,616,930,837,826,486,637,878,537,452,196,640,704,619,518,855,760,741,839
59,626,110,413,182,506,178,54,213,601,886,66,705,754,521,501,386,855,622,145
570,981,518,740,212,400,739,487,707,884,435,751,571,99,450,583,397,301,676,718
400,122,844,703,509,578,137,528,577,520,115,342,480,880,674,183,300,129,573,336
222,946,110,695,245,556,53,709,202,271,184,58,826,239,847,122,454,188,790,720
570,95,574,197,494,349,299,116,849,915,777,850,119,591,390,137,51,456,91,192
85,833,452,314,226,439,637,535,576,737,258,921,131,568,272,599,144,925,215,453
193,521,996,486,751,208,676,916,178,210,752,108,184,237,499,343,74,673,69,443
488,441,135,169,207,750,57,622,486,73,945,139,559,446,209,266,327,127,64,707
567,450,542,203,830,711,796,715,213,225,62,385,829,631,752,128,914,690,634,304
218,343,524,338,696,222,99,438,875,557,144,590,236,2,353,591,671,125,180,55
496,457,838,753,575,451,854,930,862,97,131,115,87,577,596,575,391,397,676,525
561,845,817,437,518,65,714,174,491,735,190,739,786,413,858,885,742,521,581,881
485,749,711,523,123,881,50,100,391,856,561,798,792,712,741,452,520,517,456,911
697,691,469,713,836,91,675,457,339,854,492,169,584,116,881,89,392,591,475,880
237,457,688,454,495,204,170,69,827,102,361,263,107,138,884,755,308,73,575,827
944,705,643,262,201,743,719,579,402,299,129,675,929,362,891,917,387,632,515,223
917,52,99,123,937,100,104,751,523,224,67,115,186,750,211,123,976,788,705,735
624,63,73,343,844,559,412,596,836,883,273,221,934,58,447,570,259,835,512,443
683,947,735,145,845,568,790,128,117,273,122,124,597,90,352,60,182,673,401,114
348,397,802,934,110,715,986,346,313,235,488,597,756,622,406,399,452,798,596,535
844,301,884,70,516,855,257,401,535,754,266,624,736,528,578,229,91,791,462,918
844,921,85,203,455,922,259,585,404,74,568,750,446,59,568,541,635,441,243,405
232,703,873,245,884,304,946,105,632,825,265,880,274,217,717,119,362,311,300,626
56,917,423,514,949,178,171,382,578,928,718,265,923,129,570,60,272,299,914,74
585,918,497,752,489,620,559,105,181,694,479,912,501,201,400,640,61,933,343,365
168,625,55,101,847,915,364,710,75,272,245,386,143,102,830,212,190,772,257,92
720,59,408,534,233,513,338,582,692,413,491,986,70,571,100,759,170,177,188,355
845,790,263,381,189,352,74,894,476,137,641,674,588,487,112,759,110,523,442,918
268,990,640,851,520,347,169,524,210,219,143,715,60,701,708,596,673,235,214,536
207,91,743,537,538,835,135,450,570,362,840,427,233,788,691,496,342,914,736,875
672,393,351,272,936,701,349,872,485,246,793,924,762,264,790,533,704,382,355,597
858,257,521,120,585,787,89,265,187,935,556,449,195,711,265,837,211,746,775,837
310,455,447,415,351,586,626,387,403,700,389,211,645,178,192,931,981,393,205,383
938,847,384,914,110,191,58,860,928,623,91,202,52,936,242,559,527,243,931,677
885,92,396,630,231,719,212,643,936,5,145,524,784,359,189,192,93,220,101,72
863,476,398,939,435,271,350,266,391,171,406,708,58,526,542,714,570,692,412,864
894,316,124,878,497,557,591,643,125,114,915,452,356,94,383,793,518,411,743,590
195,395,415,792,572,269,719,216,511,104,829,676,699,800,315,798,442,502,267,245
447,721,105,983,714,885,516,796,598,580,740,745,625,301,204,207,791,735,361,53
939,360,919,317,56,884,679,126,494,435,585,761,642,207,843,795,21,740,235,560
798,485,185,752,410,135,481,924,709,257,52,365,401,144,212,483,526,568,737,512
736,632,189,395,788,124,483,805,339,502,486,441,246,125,569,207,64,752,453,226
271,303,392,918,519,387,836,883,677,396,301,937,177,491,129,314,18,224,384,796
944,942,791,595,341,801,103,695,696,349,236,646,640,756,72,108,867,184,589,883
237,185,582,561,267,932,139,630,585,696,632,853,55,852,934,679,710,169,154,92
97,187,54,299,134,239,932,924,916,220,139,143,947,65,625,184,125,308,358,926
525,72,17,926,936,833,703,497,743,587,138,313,493,230,206,576,101,446,863,587
141,316,135,345,500,933,186,264,91,513,114,683,95,855,388,126,575,225,388,515
790,118,235,228,51,399,913,56,922,933,748,784,593,624,675,785,117,362,834,677
713,855,539,747,920,352,186,315,588,532,571,788,345,571,501,833,93,175,996,232
497,99,347,188,539,856,508,928,264,449,363,398,68,385,592,585,317,760,452,90
226,172,65,738,199,927,679,56,171,401,442,731,863,627,835,794,885,679,557,488
762,144,847,975,877,341,542,454,784,717,842,487,173,214,227,139,861,741,388,495
197,72,397,573,878,521,257,475,940,396,442,317,934,803,856,517,394,305,574,943
305,263,985,537,882,829,930,593,741,314,118,191,317,698,835,914,534,861,485,338
74,840,527,752,352,626,626,851,143,388,888,614,944,624,577,932,937,144,440,571
948,755,488,72,914,644,478,887,919,317,607,675,847,134,845,700,579,497,178,922
919,623,671,747,702,75,832,129,854,942,932,97,102,709,918,837,519,335,244,719
52,619,197,230,557,833,888,609,619,358,451,593,170,835,790,234,353,168,839,216
705,72,491,222,718,921,64,228,319,598,181,850,800,700,169,57,582,267,502,597
697,95,542,301,601,173,443,344,833,878,681,832,518,260,673,411,218,520,646,789
98,936,71,245,531,457,408,558,880,259,932,121,494,334,343,832,391,762,134,317
64,860,236,480,863,577,569,574,220,925,508,356,400,444,918,180,400,674,358,941
344,534,348,634,931,631,502,852,92,525,869,599,574,535,303,113,306,530,949,588
141,415,251,539,50,643,721,582,130,299,480,483,791,243,516,491,111,838,532,392
61,93,694,61,339,706,105,753,236,353,923,301,975,920,639,117,638,66,188,570
862,921,828,219,243,68,537,97,923,840,198,534,788,486,222,234,138,4,122,646
838,92,119,920,177,84,593,337,171,388,530,582,337,757,787,514,916,828,99,364
713,590,565,559,948,931,631,181,883,573,629,55,735,502,401,172,344,939,303,361
453,309,514,107,478,311,558,534,628,557,943,530,741,180,736,876,740,209,686,855
349,177,645,91,191,344,162,501,246,96,210,883,116,481,365,537,638,389,582,271
387,442,838,140,449,581,344,407,631,101,720,272,307,637,848,240,109,265,884,357
208,362,585,387,948,205,946,248,833,127,533,315,65,457,693,231,825,561,712,517
753,261,266,187,600,441,170,919,453,136,58,67,524,125,339,572,523,55,898,577
703,585,66,312,485,979,523,214,831,244,405,245,303,853,679,136,96,193,262,599
829,853,532,347,329,875,193,236,639,540,884,215,184,56,946,348,691,457,191,243
483,382,313,830,217,193,766,127,135,835,190,835,739,396,715,172,857,526,95,702
205,97,24,719,411,337,221,585,207,736,232,502,876,941,214,115,885,626,458,916
863,935,182,673,885,273,440,641,302,671,311,193,413,682,69,636,303,440,530,394
487,50,53,855,759,222,349,690,199,542,97,888,675,130,713,634,109,704,701,799
483,227,345,328,697,945,173,342,274,672,944,299,576,485,66,359,169,100,272,201
388,846,449,218,718,324,207,301,63,570,585,455,678,499,527,129,67,518,500,835
570,213,94,531,582,520,748,114,530,576,736,792,878,602,105,60,825,794,382,569
701,270,11,933,556,274,179,515,858,750,534,638,202,600,387,131,406,796,221,483
240,641,483,89,561,363,516,839,234,801,710,540,833,437,637,761,493,184,699,844
927,799,884,944,218,192,755,273,138,557,934,208,123,350,135,213,689,415,637,223
506,169,623,358,484,486,924,624,496,710,578,926,588,916,596,60,310,360,595,525
753,861,781,941,519,66,634,178,846,144,863,350,207,134,835,528,920,828,224,802
826,264,404,92,404,524,356,884,860,838,315,442,591,145,360,884,592,516,720,3
92,394,65,403,983,220,839,397,630,258,263,832,587,120,302,634,855,497,646,923
759,55,112,849,270,502,190,915,699,515,383,91,947,894,625,95,260,212,830,109
701,74,408,388,630,895,169,59,592,752,485,59,303,795,710,936,596,269,348,482
484,338,672,224,858,144,751,833,303,744,975,181,488,175,875,94,199,302,440,587
698,193,435,408,61,475,397,219,842,112,749,202,8,560,412,714,923,300,261,390
919,231,450,876,531,700,573,103,206,875,305,760,761,446,920,384,80,346,744,572
409,133,876,195,485,857,863,64,840,227,659,675,301,169,801,641,263,928,181,838
219,540,410,347,712,70,880,841,479,636,581,159,594,716,205,305,454,302,699,169
527,720,944,944,847,784,170,699,120,123,500,359,445,863,773,178,479,521,439,363
519,129,229,259,439,184,398,484,922,942,856,629,412,339,862,126,503,692,520,691
210,936,579,785,542,62,935,571,757,600,340,537,721,106,236,7,709,52,917,55
637,458,354,856,698,439,677,767,499,884,936,926,523,456,214,747,453,582,491,443
739,273,230,346,83,202,204,120,640,108,523,173,174,405,557,675,790,748,489,407
446,258,348,246,447,884,866,880,920,560,884,116,946,91,594,89,69,452,559,405
132,921,145,231,60,702,209,346,458,519,749,948,206,100,849,711,70,125,488,680
852,219,99,199,923,936,855,272,646,233,599,388,777,310,569,490,123,312,738,355
840,91,578,483,400,65,350,310,496,929,441,208,835,863,103,111,740,929,717,84
261,91,747,393,855,128,718,732,523,938,299,339,715,182,227,56,67,720,205,51
712,339,841,111,574,435,842,168,168,646,263,496,229,395,561,490,350,586,241,396
719,757,652,879,225,706,218,795,212,70,71,263,745,447,246,834,170,626,594,187
514,532,744,401,533,308,62,861,128,497,540,415,515,100,793,858,14,397,876,933
630,751,733,517,353,791,501,299,56,633,945,231,703,192,586,89,258,536,169,181
114,198,71,877,931,258,623,707,983,122,344,186,143,53,691,390,176,716,180,171
705,313,175,51,233,455,190,262,385,51,928,228,393,699,883,442,789,510,524,601
637,623,119,263,758,593,514,575,825,514,455,248,205,517,716,679,931,382,740,499
311,179,337,96,128,839,519,66,642,125,236,714,845,102,190,200,708,157,760,177
615,458,141,631,938,519,476,475,601,847,714,126,541,216,671,484,743,478,274,208
186,436,391,673,583,639,669,541,929,923,398,229,751,753,383,798,789,533,441,361
264,924,910,530,446,387,920,180,187,313,499,524,587,452,105,516,516,346,556,853
98,947,363,120,589,948,846,215,475,485,523,864,934,709,524,486,886,705,354,71
639,542,619,437,409,243,196,535,700,595,354,240,258,884,352,532,946,123,709,756
181,799,435,216,452,852,300,528,832,404,882,555,862,672,342,215,74,235,715,888
798,595,642,695,490,701,949,89,441,542,533,101,737,671,217,774,90,593,877,854
100,698,886,442,720,177,338,448,76,392,55,698,587,386,742,845,477,315,269,341
299,359,481,518,839,830,301,336,336,172,173,88,756,311,576,569,834,359,860,692
717,520,860,627,706,218,559,713,234,859,944,563,211,536,826,357,55,640,695,136
337,270,90,878,759,57,573,941,301,337,533,451,884,70,494,596,196,357,856,609
63,946,696,779,536,704,514,914,53,583,715,96,198,882,748,227,487,693,586,348
574,187,498,93,175,832,248,357,837,539,835,539,794,348,304,522,833,948,439,338
712,676,198,693,317,484,90,242,832,717,888,848,485,356,199,692,787,452,675,454
65,621,622,407,918,492,681,337,528,217,707,929,537,315,477,525,537,145,757,862
879,90,311,402,352,227,237,260,913,704,114,397,708,403,363,636,826,485,383,183
189,493,389,929,344,857,211,708,775,567,456,224,72,525,402,311,358,755,170,116
482,475,535,216,717,533,995,926,128,260,885,748,212,265,596,439,576,887,834,187
542,116,483,715,169,132,760,653,306,540,847,516,235,389,529,831,386,388,636,313
273,499,454,626,627,693,845,638,191,405,239,174,198,177,636,236,123,879,702,136
437,498,789,317,737,442,65,490,482,336,15,270,437,938,757,934,843,513,845,561
58,447,137,170,643,439,384,691,701,92,204,647,827,246,266,691,258,201,192,558
742,357,344,758,524,800,625,329,398,691,115,587,943,100,305,89,519,134,305,694
561,918,199,175,929,202,674,742,136,57,391,128,497,995,233,394,306,435,221,200
720,269,919,8,221,230,559,382,759,412,184,644,347,588,538,572,757,71,262,135
324,182,260,832,786,246,855,559,445,262,487,571,860,625,355,917,145,450,396,235
709,260,392,537,262,174,491,595,926,133,793,413,840,223,394,385,532,213,587,730
787,390,401,832,198,360,107,456,573,797,675,478,800,86,487,630,945,747,219,931
761,92,825,692,642,176,262,544,310,875,737,348,625,64,916,310,437,224,753,258
762,848,300,583,933,442,242,883,476,386,674,401,888,74,259,107,935,119,350,700
194,169,495,644,497,107,386,676,786,273,736,89,885,941,444,307,751,483,454,661
301,862,125,261,527,571,999,922,355,218,834,523,193,188,517,105,945,206,839,522
348,742,641,735,259,948,228,438,879,584,219,518,114,235,96,331,74,855,183,701
704,882,510,678,243,859,515,925,623,448,848,202,846,587,121,310,436,691,236,697
845,453,939,349,884,940,0,621,933,188,73,204,440,918,207,671,629,735,487,627
879,694,125,501,580,274,451,885,834,397,146,104,185,173,528,717,590,412,350,936
944,91,450,631,262,501,196,401,199,946,105,236,634,765,66,385,582,437,262,581
924,594,156,702,128,858,133,695,140,486,399,534,115,627,748,560,518,700,883,206")

(defun example ()
  "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12")

(defun example-2 ()
  "class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9")
