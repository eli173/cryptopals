
;; answers

(defun print-answers ()
  (print "Challenge 1:")
  (print (get-base64-rep (hex-import-string "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")))
  (print "Challenge 2:")
  (print (get-hex-rep (fixed-xor (hex-import-string "1c0111001f010100061a024b53535009181c")
				 (hex-import-string "686974207468652062756c6c277320657965"))))
  (print "Challenge 3:")
  (print (nth 5 (get-top-n-strings 6 (hex-import-string "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"))))
  (print "Challenge 4:")
  (print (caar (filter-search-by 1.4 (search-the-file))))
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
    ( if (= bits-needed wordlength)
	 bitarray
	 (adjust-array bitarray (+ arrlen bits-needed)
			  :element-type 'bit :initial-element 0))))

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
      (rec-helper 0))))


(defun get-sub-array (bitarray index size)
  "Returns bit array of length size starting at index (zero-indexed) in bitarray"
  (let ((ret-array (make-array size :initial-element 0 :element-type 'bit)))
    (labels ((rec-helper (num)
	       (cond ((= num size) ret-array)
		     (t (setf (aref ret-array num) (aref bitarray (+ index num)))
			(rec-helper (+ num 1))))))
      (rec-helper 0))))

(defun get-word-list (bitarray wordsize)
  (let* ((working-array (adjust-to-word-length-n (copy-bit-array bitarray) wordsize))
	 (num-words (truncate (car (array-dimensions working-array)) wordsize)))
    (labels ((rec-helper (words ret-list)
	       (cond ((= words num-words) ret-list)
		     (t (rec-helper (+ words 1)
				    (cons (get-sub-array working-array
							 (* wordsize words)
							 wordsize)
					  ret-list))))))
      (rec-helper 0 nil))))

(defun get-word-uint-list (bitarray wordsize)
  "Returns a list of uints of wordsize size from bitarray"
  (mapcar #'bit-array-to-uinteger (get-word-list bitarray wordsize)))

(defun bit-array-to-char-list (bitarray)
  (mapcar #'code-char (get-word-uint-list bitarray 8)))

(defun bit-array-to-string (bitarray)
  (coerce (bit-array-to-char-list bitarray) 'string))

(defun string-to-bit-array (string)
  )

(defvar char-freq-table
  '("abcdefghijklmnopqrstuvwzyz"
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    #(0.081 0.015 0.027 0.043 0.120
      0.023 0.020 0.059 0.073 0.001
      0.007 0.040 0.026 0.070 0.077
      0.018 0.001 0.060 0.063 0.091
      0.029 0.011 0.021 0.002 0.021
      0.001)))


(defun score-bitarray (bitarray)
  "Scores a bitarray according to char-freq-table"
  (labels ((get-char-score (char)
	     (cond
	       ((position char (car char-freq-table))
		(aref (caddr char-freq-table)
		      (position char (car char-freq-table))))
	       ((position char (cadr char-freq-table))
		(aref (caddr char-freq-table)
		      (position char (cadr char-freq-table))))
	       (t 0))))
    (reduce #'+ (mapcar #'get-char-score (bit-array-to-char-list bitarray)))))

(defun score-string (string)
  (labels ((get-char-score (char)
	     (cond
	       ((position char (car char-freq-table))
		(aref (caddr char-freq-table)
		      (position char (car char-freq-table))))
	       ((position char (cadr char-freq-table))
		(aref (caddr char-freq-table)
		      (position char (cadr char-freq-table))))
	       (t 0))))
      (reduce #'+ (map 'list #'get-char-score string))))

(defun get-scores-of-char-xor (bitarray)
  (labels ((rec-helper (num x)
	     (cond ((eq num 0) x)
		   (t (rec-helper
		       (- num 1)
		       (cons (score-bitarray
			      (single-word-xor
			       bitarray
			       (uinteger-to-bitarray num)))
			     x))))))
    (rec-helper 255 nil)))

(defun first-n (n x)
  (labels ((rec-helper (n wx rx)
	     (cond ((= n 0) rx)
		   (t (rec-helper (- n 1) (cdr wx) (cons (car wx) rx))))))
    (reverse (rec-helper n x nil))))

(defun get-top-n-scores (n bitarray)
  (first-n n (sort (get-scores-of-char-xor bitarray) #'>)))

(defun get-top-n-strings (n bitarray)
  (mapcar #'(lambda (ba)
	      (coerce (bit-array-to-char-list ba) 'string))
	  (mapcar #'cdr
		  (first-n n (sort (get-scores-and-strings bitarray)
				   #'(lambda (a b)
				       (> (car a) (car b))))))))

(defun get-scores-and-strings (bitarray)
  (labels ((get-pair (thebyte)
	     (cons (score-bitarray
		    (single-word-xor bitarray
				     (adjust-to-word-length-n
				      (uinteger-to-bitarray thebyte) 8)))
		   (single-word-xor bitarray
				    (adjust-to-word-length-n
				     (uinteger-to-bitarray thebyte) 8))))
	   (rec-helper (byte retlist)
	     (cond ((= byte 256)
		    retlist)
		   (t (rec-helper (+ byte 1) (cons (get-pair byte) retlist))))))
    (rec-helper 1 nil)))


;; challenge 4
;; find encrypted line in file

(defun search-the-file ()
  (let ((f (open "./set-1-4.txt")))
    (labels ((build-setup (curr-line x)
	       (cond ((null curr-line) x)
		     (t (build-setup
			 (read-line f nil nil)
			 (cons (get-top-n-strings
				10
				(hex-import-string curr-line))
			       x))))))
      (build-setup (read-line f nil nil) nil))))

(defun filter-search-by (r search-results)
  (labels ((filter-one-of-em (result-list)
	     (remove-if
	      #'(lambda (e)
		  (< (score-string e) r))
	      result-list)))
    (remove-if #'null (mapcar #'filter-one-of-em search-results))))

;; challenge 5
;; repeating-key XOR


(defun cbc-mode-encrypt (plain key)
  "Chains XOR over plain bitarray using key bitarray"
  (let ((retarray (adjust-to-word-length-n (copy-bit-array plain)
					   (car (array-dimensions key))))
	(newkey (copy-bit-array key)))
    (labels ((rec-helper (index)
	       (cond ((= index (car (array-dimensions retarray)))
		      retarray)
		     (t (setf (aref retarray index)
			      (logxor (aref retarray index)
				      (aref newkey (rem index
							(car (array-dimensions newkey))))))
			(setf (aref newkey (rem index
						(car (array-dimensions newkey))))
			      (aref retarray index))
			(rec-helper (+ index 1))))))
      (rec-helper 0)))))
