
;; answers

(defun print-answers ()
  (print "Challenge 1:")
  (print (get-base64-rep (hex-import-string "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")))
  (print "Challenge 2:")
  (print (get-hex-rep (fixed-xor (hex-import-string "1c0111001f010100061a024b53535009181c")
				 (hex-import-string "686974207468652062756c6c277320657965"))))
  nil)


;; challenge 1
;; hex - base64


(defun hex-import-string (hex-str)
  "returns bit array holding string with LSB first"
  ; is LSB first a good idea?
  ; currently works bit-by-bit, should do it by bytes or words...
  (let ((number (parse-integer hex-str :radix 16)))
    (uinteger-to-bitarray number)))
	


(defun bit-array-to-uinteger (bitarray)
  (let ((balen (car (array-dimensions bitarray))))
    (labels ((rec-helper (pos ret)
	       (cond ((= pos balen) ret)
		     (t (rec-helper (+ pos 1)
				    (+ ret (* (aref bitarray pos) (expt 2 pos))))))))
      (rec-helper 0 0))))

  
(defun get-base64-rep (bitarray)
  (let ((b64chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
	(number (bit-array-to-uinteger bitarray)))
    (labels ((rec-helper (num retstr)
	       (cond ((= 0 num) retstr)
		     (t (rec-helper (truncate num 64)
				    (concatenate 'string
						 (string (aref b64chars (rem num 64)))
						 retstr))))))
      (rec-helper number ""))))


(defun get-hex-rep (bitarray)
  (let ((hexchars "0123456789ABCDEF")
	(number (bit-array-to-uinteger bitarray)))
    (labels ((rec-helper (num retstr)
	       (cond ((= 0 num) retstr)
		     (t (rec-helper (truncate num 16)
				    (concatenate 'string
						 (string (aref hexchars (rem num 16)))
						 retstr))))))
      (rec-helper number ""))))



;; challenge 2
;; fixed XOR


(defun uinteger-to-bitarray (number)
  (let ((bitarray (make-array (+ 1 (floor (log number 2)))
			      :adjustable t :element-type 'bit :initial-element 0)))
    (labels ((rec-helper (num idx)
	       (cond ((= num 0) bitarray)
		     (t (setf (aref bitarray idx) (rem num 2))
			(rec-helper (truncate num 2) (+ idx 1))))))
      (rec-helper number 0))))



(defun fixed-xor (bs1 bs2)
  (let* ((longer (if (> (car (array-dimensions bs1))
			(car (array-dimensions bs2)))
		     bs1 bs2))
	 (shorter (if (> (car (array-dimensions bs1))
			 (car (array-dimensions bs2)))
		      bs2 bs1))
	 (maxlen (car (array-dimensions longer)))
	 (minlen (min (car (array-dimensions shorter))))
	 (retarray (make-array maxlen :adjustable t
			       :element-type 'bit
			       :initial-element 0)))
	 
    (labels ((rec-helper (index)
	       (cond ((= index maxlen) retarray)
		     ((>= index minlen)
		      (setf (aref retarray index) (aref longer index))
		      (rec-helper (+ index 1)))
		     (t (setf (aref retarray index)
			      (logxor (aref longer index) (aref shorter index)))
			(rec-helper (+ index 1))))))
      (rec-helper 0))))


;; challenge 3
;; single-byte xor

(defun adjust-to-word-length-n (bitarray wordlength)
  "Takes a bit array and changes it to divide evenly with the wordlength"
  (let* ((arrlen (car (array-dimensions bitarray)))
	 (bits-needed (- wordlength (rem arrlen wordlength))))
    (adjust-array bitarray (+ arrlen bits-needed)
		  :element-type 'bit :initial-element 0)))

(defun remove-superfluous-zeros-n (bitarray)
  "Basically the opposite of adjust-to-word-length-n"
  (adjust-array bitarray (+ 1 (floor (log (bit-array-to-uinteger bitarray) 2))) :element-type 'bit))

(defun copy-bit-array (bitarray)
  (fixed-xor (make-array (array-dimensions bitarray)
			 :adjustable t
			 :element-type 'bit
			 :initial-element 0)
	     bitarray))

(defun single-word-xor (bitarray word)
  "XORs bitarray by word, adjusting as needed. word is also a bitarray"
  ;; wait, do I extend or just leave it? extending for now
  (let ((retarray (adjust-to-word-length-n (copy-bit-array bitarray)
					   (car (array-dimensions word)))))
    (labels ((rec-helper (index)
	       (cond ((= index (car (array-dimensions retarray))) retarray)
		     (t (setf (aref retarray index)
			      (logxor (aref retarray index)
				      (aref word (rem index
						      (car (array-dimensions word))))))
			(rec-helper (+ index 1))))))
      (print (array-dimensions bitarray))
      (print (array-dimensions retarray))
      (print (array-dimensions word))
      (rec-helper 0))))

						
