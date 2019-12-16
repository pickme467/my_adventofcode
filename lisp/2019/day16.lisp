(defun input ()
  "59790132880344516900093091154955597199863490073342910249565395038806135885706290664499164028251508292041959926849162473699550018653393834944216172810195882161876866188294352485183178740261279280213486011018791012560046012995409807741782162189252951939029564062935408459914894373210511494699108265315264830173403743547300700976944780004513514866386570658448247527151658945604790687693036691590606045331434271899594734825392560698221510565391059565109571638751133487824774572142934078485772422422132834305704887084146829228294925039109858598295988853017494057928948890390543290199918610303090142501490713145935617325806587528883833726972378426243439037")


(defun char-to-number (char)
  (- (char-int char) (char-int #\0)))

(defun string-to-number-list (string)
  (map 'list #'char-to-number string))

(defun get-pattern-for-iteration-index (iteration index)
  (let ((pattern-size (* 4 iteration)))))
