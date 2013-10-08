;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RayTracer implement in Lisp.
;; Based on Paul Hackbert's business card raytracer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;;; Initialize
;;;;;;;;;;;;;;;;;;
(setf *random-state* (make-random-state t))

;;;;;;;;;;;;;;;;;;
;; Vector3D Class
;;;;;;;;;;;;;;;;;;
(defun mkv (x y z)
  (vector x y z))

(defun x (v)
  (aref v 0))
(defun y (v)
  (aref v 1))
(defun z (v)
  (aref v 2))

;; Vector Operations
(defun v-add (v1 v2)
  (mkv (+ (x v1) (x v2))
       (+ (y v1) (y v2))
       (+ (z v1) (z v2))))

(defun v-mul (v1 n)
  (mkv (* (x v1) n)
       (* (y v1) n)
       (* (z v1) n)))
(defun v-dot (v1 v2)
  (+ (* (x v1) (x v2))
     (* (y v1) (y v2))
     (* (z v1) (z v2))))

(defun v-crs (v1 v2)
  (mkv (- (* (y v1) (z v2)) (* (z v1) (y v2)))
       (- (* (z v1) (x v2)) (* (x v1) (z v2)))
       (- (* (x v1) (y v2)) (* (y v1) (x v2)))))

(defun v-nrm (v1)
  (v-mul v1 (/ 1 (sqrt (v-dot v1 v1)))))

;;;;;;;;;;;;;;;;;;
;; Sphere positions.
;;;;;;;;;;;;;;;;;;
(defvar *G* #(247570 280596 280600 249748 18578 18577 231184 16 16))

(defun v-trace (o d tt n)
  (when (zerop (z d))
    (setf (z d) .0001))
  (let ((tt 1e9)
	(m 0)
	(p (- (/ (z o) (z d)))))
    (when (> p .01)
      (setf tt p
	    n (mkv 0 0 1)
	    m 1))
    (loop for k from 18 downto 0
	 do
	 (loop for j from 8 downto 0
	      if (not (zerop (logand (aref *G* j) (ash 1 k))))
	      do
	      (let* ((p (v-add o (mkv (- k) 0 (- (- j) 4))))
		     (b (v-dot p d))
		     (c (1- (v-dot p p)))
		     (q (- (* b b) c)))
		(when (> q 0)
		  (let ((s (- (- b) (sqrt q))))
		    (when (< 0.01 s tt)
		      (setf tt s
			    n (v-nrm (v-add p (v-mul d tt)))
			    m 2)))))))
    (values m tt n)))

(defun v-sample (o d)
  (let ((tt 0.0)
	(n (mkv 0 0 0)))
    (multiple-value-bind (m tt n) (v-trace o d tt n)
      (if (zerop m)
	  (v-mul (mkv 0.7 0.6 1) (expt (- 1 (z d)) 4))
	  (let* ((h (v-add o (v-mul d tt)))
		 (l (v-nrm (v-add (mkv (+ 9 (random 1.0)) (+ 9 (random 1.0)) 16)
				  (v-mul h -1))))
		 (r (v-add d (v-mul n (* (v-dot n d) -2))))
		 (b (v-dot l n)))
	    (when (or (< b 0) (not (zerop (v-trace h l tt n))))
	      (setf b 0))
	    (let ((p (expt (* (v-dot l r) (if (> b 0) 1 0)) 99)))
	      (if (= 1 (mod m 2))
		  (progn (setf h (v-mul h 0.2))
			 (let ((x (if (= 1 (mod (+ (ceiling (x h)) (ceiling (y h))) 2))
				      (mkv 3 1 1) (mkv 3 3 3))))
			   (v-mul x (+ (* b 0.2) 0.1))))
		  (v-add (mkv p p p) (v-mul (v-sample h r) 0.5)))))))))

(defun main ()
  (format t "P3 512 512 255 ")
  (let* ((g (v-nrm (mkv -6 -16 0)))
         (a (v-mul (v-nrm (v-crs (mkv 0 0 1) g)) 0.002))
         (b (v-mul (v-nrm (v-crs g a)) 0.002))
         (c (v-add (v-mul (v-add a b) -256) g))
         (view-point (mkv 17 16 8)))
    (let ((rr 13) (gg 13) (bb 13))
      (loop for y from 511 downto 0
            do
            (loop for x from 511 downto 0
                  do
                  (let ((p (mkv rr gg bb)))
                    (loop for r from 63 downto 0
                          do
                          (let ((tt (v-add (v-mul (v-mul a (- (random 1.0) 0.5)) 99)
                                           (v-mul (v-mul b (- (random 1.0) 0.5)) 99))))
                            (setf p
                                  (v-add (v-mul 
                                           (v-sample 
                                             (v-add view-point tt)
                                             (v-nrm (v-add (v-mul tt -1) 
                                                           (v-mul (v-add 
                                                                    (v-mul a (+ (random 1.0) x))
                                                                    (v-add (v-mul b (+ (random 1.0) y)) c)) 16))))
                                           3.5) p))))
                    (format t "~A ~A ~A " (floor (x p)) (floor (y p)) (floor (z p)))))))))
(main)
