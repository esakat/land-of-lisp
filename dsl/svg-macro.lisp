(defun print-tag (name alst closingp)
  (princ #\<)
  (when closingp
    (princ #\/))
  (princ (string-downcase name))
  (mapc (lambda (att)
	  (format t " ~a=\"~a\"" (string-downcase (car att)) (cdr att)))
	alst)
  (princ #\>))

;; let1
(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

;; split
(defmacro split (val yes no)
  (let1 g (gensym)
	`(let1 ,g ,val
	       (if ,g
		   (let ((head (car ,g))
			 (tail (cdr ,g)))
		     ,yes)
		   ,no))))

;; pairs関数
(defun pairs (lst)
  (labels ((f (lst acc)
	     (split lst
		    (if tail
			(f (cdr tail) (cons (cons head (car tail)) acc))
			(reverse acc))
		    (reverse acc))))
    (f lst nil)))

;; tagマクロ
(defmacro tag (name atts &body body)
  `(progn (print-tag ',name
		     (list ,@(mapcar (lambda (x)
       			       `(cons ',(car x) ,(cdr x)))
				     (pairs atts)))
		     nil)
	  ,@body
	  (print-tag ',name nil t)))

;; html生成用
(defmacro html (&body body)
  `(tag html ()
     ,@body))

(defmacro body (&body body)
  `(tag body ()
     ,@body))

;; svg生成用
(defmacro svg (width height &body body)
  `(tag svg (xmlns "http://www.w3.org/2000/svg"
		   "xmlns:xlink" "http://www.w3.org/1999/xlink" height ,height width ,width)
     ,@body))


;; 色の生成を行う関数
(defun brightness (col amt)
  (mapcar (lambda (x)
	    (min 255 (max 0 (+ x amt))))
	  col))

;; svgのスタイルを設定
(defun svg-style (color)
  (format nil
	  "~{fill:rgb(~a, ~a, ~a);stroke:rgb(~a, ~a, ~a)~}"
	  (append color
		  (brightness color -100))))

;; 円を描く関数
(defun circle (center radius color)
  (tag circle (cx (car center)
	       cy (cdr center)
	       r radius
	       style (svg-style color))))

;; ポリゴンを描く関数
(defun polygon (points color)
  (tag polygon (points (format nil
			       "~{~a,~a ~}"
			       (mapcan (lambda (tp)
					 (list (car tp) (cdr tp)))
				       points))
		       style (svg-style color))))

;; ランダムウォーク
(defun random-walk (value length)
  (unless (zerop length)
    (cons value
	  (random-walk (if (zerop (random 2))
			   (1- value)
			   (1+ value))
		       (1- length)))))

;; 画像を生成する
;; (with-open-file (*standard-output* "random_walk.svg"
;; 				   :direction :output
;; 				   :if-exists :supersede)
;;   (svg 400 200 (loop repeat 10
;; 		     do (polygon (append '((0 . 200))
;; 					 (loop for x
;; 					       for y in (random-walk 100 400)
;; 					       collect (cons x y))
;; 					 '((400 . 200)))
;; 				 (loop repeat 3
;; 				       collect (random 256))))))
