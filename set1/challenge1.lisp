

;; hex - base64


(defun hex-import-string (hex-str)
  "returns bit array holding string with LSB first"
  ; is LSB first a good idea?
  ; currently works bit-by-bit, should do it by bytes or words...
  (let* ((number (parse-integer hex-str :radix 16))
	 (bitarray (make-array (+ 1 (floor (log number 2))) :adjustable t :element-type 'bit :initial-element 0)))
    (labels ((rec-helper (num idx)
	       (cond ((= num 0) bitarray)
		     (t (setf (aref bitarray idx) (rem num 2))
			(rec-helper (truncate num 2) (+ idx 1))))))
      (rec-helper number 0))))
		      


(defun bit-array-to-integer (bitarray)
  (let ((balen (car (array-dimensions bitarray))))
    (labels ((rec-helper (pos ret)
	       (cond ((= pos balen) ret)
		     (t (rec-helper (+ pos 1) (+ ret (* (aref bitarray pos) (expt 2 pos))))))))
      (rec-helper 0 0))))

  
(defun get-base64-rep (bitarray)
  (let ((b64chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
	(number (bit-array-to-integer bitarray)))
    (labels ((rec-helper (num retstr)
	       (cond ((= 0 num) retstr)
		     (t (rec-helper (truncate num 64)
				    (concatenate 'string (string (aref b64chars (rem num 64))) retstr))))))
      (rec-helper number ""))))

						  
