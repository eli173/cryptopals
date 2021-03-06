
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
  (print "Challenge 5:")
  (print (get-hex-rep (byte-repeating-key-xor (string-to-bit-array "Burning 'em, if you ain't quick and nimble
I go crazy when I hear a cymbal") (string-to-bit-array "ICE"))))
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
  (if (= number 0) #*0 
      ;; can't do zero... should zero be #*0 or #*?
      ;; leaning towards #*0... fix later if need be
      (let ((bitarray (make-array (+ 1 (floor (log number 2)))
				  :adjustable t :element-type 'bit :initial-element 0)))
	(labels ((rec-helper (num idx)
		   (cond ((= num 0) bitarray)
			 (t (setf (aref bitarray idx) (rem num 2))
			    (rec-helper (truncate num 2) (+ idx 1))))))
	  (rec-helper number 0)))))



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
  (labels ((uint-byte-list-to-uint (ls index ret)
	     (cond ((null ls) ret)
		   (t
		    (uint-byte-list-to-uint
		     (cdr ls)
		     (+ index 1)
		     (+ (* ret 256) (car ls)))))))
    (adjust-to-word-length-n (uinteger-to-bitarray
			      (uint-byte-list-to-uint
			       (map 'list #'char-code string) 0 0))
			     8)))

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

(defun gsasw (ba wl)
  (labels ((get-pair (theword)
	     (cons (score-bitarray (single-word-xor
				    (adjust-to-word-length-n (copy-bit-array ba) wl)
				    (adjust-to-word-length-n (uinteger-to-bitarray theword) wl)))
		   (single-word-xor (adjust-to-word-length-n (copy-bit-array ba) wl)
				    (adjust-to-word-length-n (uinteger-to-bitarray theword) wl))))
	   (rec-helper (word &optional retls)
	     (cond ((= word (expt 2 wl))
		    retls)
		   (t (rec-helper (+ word 1) (cons (get-pair word) retls))))))
    (rec-helper 1 nil)))

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

(defun offset-byte-xor (bitarray byte offset-bytes)
  "XORs the byte with offset-bytes position offset in bitarray"
  (let ((retarray (adjust-to-word-length-n (copy-bit-array bitarray) 8))
	(offset-pos (* 8 offset-bytes)))
    (loop for i from 0 to 7 do
	 (setf (aref retarray (+ offset-pos i))
	       (logxor (aref retarray (+ offset-pos i))
		       (aref byte i))))
    retarray))

(defun get-word (bitarray n wl)
  "get nth word, 0-indexed, wl is wordlength"
  (let ((ret-word (make-array wl :element-type 'bit :initial-element 0 :adjustable t)))
    (loop for i from 0 to (- wl 1) do
	 (setf (aref ret-word i)
	       (aref bitarray (+ i (* wl n)))))
    ret-word))

(defun get-byte (bitarray n)
  "get nth byte 0-indexed ofc"
  ;; doesn't check if it's in bounds or not
  (get-word bitarray n 8))

(defun set-word-n (bitarray n wl word)
  (loop for i from 0 to (- wl 1) do
       (setf (aref bitarray (+ i (* n wl)))
	     (aref word i)))
  bitarray)

(defun set-byte-n (bitarray n byte)
  "sets nth byte in bitarray to byte"
  (set-word-n bitarray n 8 byte)
  bitarray)

(defun bitarray-append (b1 b2)
  "Appends b2 to the end of b1"
  (concatenate 'bit-vector b1 b2))

;; okay not sure how to specify... kinda just for strings?

;; OHH-KAY think about the strings being backwards,
;; so it is off if the word length doesn't divide
;; think about working backwards
(defun repeating-key-xor (plain key)
  ;; wait.. this is just swxor with var-length key...
  ;; No! you can't adjust the plain, it loads the end with junk
  ;; I'll just always make it 8 bits...
  (let ((retarray (adjust-to-word-length-n (copy-bit-array plain) 8)))
    (labels ((rec-helper (index)
	       (cond ((= index (car (array-dimensions retarray)))
		      retarray)
		     (t		      
		      (setf (aref retarray index)
			      (logxor (aref retarray index)
				      (aref key (rem index
						     (car
						      (array-dimensions key))))))
			(rec-helper (+ index 1))))))
      (rec-helper 0))))

(defun new-repeating-key-xor (plain key)
  ;; wait.. this is just swxor with var-length key...
  ;; No! you can't adjust the plain, it loads the end with junk
  ;; I'll just always make it 8 bits...
  (let ((retarray (adjust-to-word-length-n (copy-bit-array plain) 8)))
    (labels ((rec-helper (index)
	       (cond ((< index 0)
		      retarray)
		     (t		      
		      (setf (aref retarray index)
			      (logxor (aref retarray index)
				      (aref key (rem index
						     (car
						      (array-dimensions key))))))
			(rec-helper (- index 1))))))
      (rec-helper (- (car (array-dimensions retarray)) 1)))))

(defun byte-repeating-key-xor (plain key)
  (let* ((retarray (adjust-to-word-length-n (copy-bit-array plain) 8))
	 (key-bytes (truncate (car (array-dimensions key)) 8))
	 (plain-bytes (truncate (car (array-dimensions retarray)) 8))
	 (the-offset (rem plain-bytes key-bytes))
	 )
    (labels ((pos-rem (a b)
	       (if (< (rem a b) 0) (+ (rem a b) b) (rem a b)))
	     (rec-helper (byte-index)
	       (cond ((= byte-index plain-bytes)
		      retarray)
		     (t
		      (set-byte-n retarray byte-index
				  (adjust-to-word-length-n
				   (fixed-xor
				    (get-byte retarray byte-index)
				    (get-byte key (pos-rem (- byte-index the-offset) key-bytes)))
				   8))
		      (rec-helper (+ byte-index 1))))))
      (rec-helper 0))))

(defun old-new-repeating-key-xor (plain key)
  (let ((retarray (adjust-to-word-length-n (copy-bit-array plain) 8))
	(workarray (make-array 8 :element-type 'bit :initial-element 0 :adjustable t)))
    (labels ((rec-helper (index) ;byte index
	       (cond ((= index (/ (car (array-dimensions retarray)) 8))
		      retarray)
		     (t (set-byte-n retarray index
				    (fixed-xor
				     (get-byte retarray index)
				     (get-byte key
					       (rem
						index
						(car (array-dimensions key))))))
			(rec-helper (+ index 1))))))
      (aref workarray 0)
      (rec-helper 0))))

;; not what c5 is asking!
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
      (rec-helper 0))))

;; challenge 6
;; break repeating-key-xor

;; currently fails on different-length arrays...
(defun hamming-distance (ba1 ba2)
  (let ((longer (if (> (car (array-dimensions ba1)) (car (array-dimensions ba2)))
		    ba1 ba1))
	(shorter (if (< (car (array-dimensions ba1)) (car (array-dimensions ba2)))
		     ba2 ba2)))
    (labels ((rec-helper (index total)
	       (cond ((= index (car (array-dimensions longer)))
		      total)
		     ((>= index (car (array-dimensions shorter)))
		      (rec-helper (+ index 1) (+ total (aref longer index))))
		     (t
		      (rec-helper
		       (+ index 1)
		       (+ total
			  (logxor (aref longer index)
				  (aref shorter index))))))))
      (rec-helper 0 0))))


(defun base64-to-bitarray (b64str)
  (let ((b64chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
    (labels ((lookup-b64 (char)
	       (cond ((not (position char b64chars))
		      #*)
		     (t (adjust-to-word-length-n (uinteger-to-bitarray (position char b64chars)) 6))))
	     (rec-helper (chls arr)
	       (cond ((equal chls nil) arr)
		     (t
		      (rec-helper
		       (cdr chls)
		       (concatenate 'bit-vector
				    (lookup-b64 (car chls))
				    arr))))))
      (rec-helper (coerce b64str 'list) #*))))


(defun separate-to-keysize (data keysize)
  "Takes data, returns list of keysize-long sections of data"
  (let ((wdata (adjust-to-word-length-n data keysize)))
    (labels ((rec-helper (index retls)
	       (cond ((= (* index keysize) (car (array-dimensions wdata)));;yeah??
		      retls)
		     (t (rec-helper
			 (+ index 1)
			 (cons
			  (get-word wdata index keysize)
			  retls))))))
      (rec-helper 0 nil))))

(defun transpose-blocks (blocks-list)
  (labels ((append-to-nth (ls n e)
	     (setf (nth n ls)
		   (append (nth n ls) (list e)))
	     ls)
	   (rec-helper (e)
	     nil))
    nil))

(defun transpose (list-of-lists)
  (apply #'mapcar #'list list-of-lists))

(defun get-keys-and-scores-sb-xor (bitarray)
  (labels ((mk-list (a b r)
	     (cond ((= b a) (append r (list a)))
		   (t (mk-list (+ 1 a) b
			       (append r (list a)))))))
    (mapcar #'cons (get-scores-of-char-xor bitarray) (mk-list 0 255 nil))))

(defun transpose-bitarray (ba n wl)
  "Takes ba and gives n lists with wl-long chunks"
  (transpose (mapcar
	      #'(lambda (e) (separate-to-keysize e wl))
	      (separate-to-keysize ba (* wl n)))))

(defun import-b64-file (path)
  (let ((f (open path))
	(curr-line nil))
    (labels ((rec-helper (retarray)
	       (if (not (setf curr-line (read-line f nil nil)))
		   retarray
		   (rec-helper
		    (bitarray-append
		     retarray
		     (base64-to-bitarray curr-line))))))
      (rec-helper #*))))

(defun is-ascii-char (chr)
  (and (> (char-code chr) 0)
       (< (char-code chr) 128)))

(defun is-printable-char (chr)
  (or (and (< (char-code chr) 177)
	   (> (char-code chr) 31))
      (= (char-code chr) 9)
      (= (char-code chr) 10)
      (= (char-code chr) 11)
      (= (char-code chr) 12)
      (= (char-code chr) 13)))

(defun is-printable-str (str)
  (every #'is-printable-char str))

(defun sort-by-num-chars (strs)
  "sorts by num of alphanums in str"
  (labels ((is-alphanum (c)
	     (or (and
		  (>= (char-code c) 97)
		  (<= (char-code c) 122))
		 (and
		  (>= (char-code c) 65)
		  (<= (char-code c) 90))
		 (and
		  (>= (char-code c) 48)
		  (>= (char-code c) 57))))
	   (num-alphanums (s)
	     (reduce #'+
		     (map 'list #'(lambda (e) (if (is-alphanum e) 1 0)) s
		      )))
	   (more-alphanums (a b)
	     (> (num-alphanums a) (num-alphanums b))))
    (sort strs #'more-alphanums)))

(defun is-ascii-string (str)
  (every #'identity (map 'list #'is-ascii-char str)))

(defun ba-list-to-ba (ba-ls)
  (labels ((rec-helper (ls retarr)
	     (cond ((null ls) retarr)
		   (t (rec-helper (cdr ls) (bitarray-append (car ls) retarr))))))
    (rec-helper ba-ls #*)))

(defun break-single-byte-xor (ciphertext n)
  "returns top n options for single-byte xor keys"
  (mapcar #'(lambda (el)
	      (cons (fixed-xor (get-byte ciphertext 0)
			       (get-byte (string-to-bit-array el) 0))
		    el))
	  (get-top-n-strings n ciphertext)))

(defun break-ks-xor (ciphertext ks n )
  "like break-sbxor but for keysize ks"
  (mapcar #'(lambda (e) (break-single-byte-xor (ba-list-to-ba e) n))
	  (transpose-bitarray ciphertext ks 8)))

(defun unsplice-strings (str-ls)
  (reduce #'(lambda (a b) (concatenate 'string a b))
	  (mapcar #'(lambda (e) (coerce e 'string))
		  (apply #'mapcar #'list
			 (mapcar #'(lambda (e) (coerce e 'list)) str-ls)))
	  :initial-value ""))

(defun break-repeating-key-xor (ciphertext keysize-min keysize-max n)
  "Gets top n {keysizes, matches, strings} options I guess? Keysizes are bytes"
  (labels ((make-list-between (a b &optional retls)
	     (cond ((= b a) (append  retls (list a)))
		   (t (make-list-between
		       (+ 1 a)
		       b
		       (append retls (list a))))))
	   (get-keysize-distance (ks)
	     (/ (hamming-distance (get-word ciphertext 0 (* 8 ks))
				  (get-word ciphertext 1 (* 8 ks)))
		ks))
	   (get-top-n-keysizes (ks-list n)
	     (first-n n (sort ks-list
			      #'(lambda (a b) (< (cdr a) (cdr b)))))))
    (let ((keysize-dist-list
	   (mapcar #'(lambda (e) (cons e (get-keysize-distance e)))
		   (make-list-between keysize-min keysize-max))))
      (get-top-n-keysizes keysize-dist-list 5))))

(defun s-1-6-playground ()
  (transpose-bitarray  (import-b64-file "set-1-6.txt") (* 8 3) 3))
