;; 場所と描写の定義
(defparameter *nodes* '((living-room (you are in the living-room.
				      a wizzard is sbirubg loudly on the couch.))
			(garden (you are in a beautiful garden.
				 there is a well in front of you.))
			(attic (you are in the attic.
				there is a giant welding torch in the corner.))))
;; 通り道の定義
(defparameter *edges* '((living-room (garden west door)
			(attic upstairs ladder))
			(garden (living-room east door))
			(attic (living-room downstairs ladder))))

;; オブジェクトリスト
(defparameter *objects* '(whiskey bucket frog chain))
;; オブジェクトと場所のマッピング
(defparameter *object-locations* '((whiskey living-room)
				   (bucket living-room)
				   (chain garden)
				   (frog garden)))

;; 現在地の定義
(defparameter *location* 'living-room)

;; 場所の描写
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

;; 通り道の描写
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;; 通り道の候補一覧を出す
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;; 場所からオブジェクトリストを返す
(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
	     (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

;; ある場所で見えるオブジェクト一覧
(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
	     `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

;; 見えるものすべてを描写
(defun look ()
  (append (describe-location *location* *nodes*)
	  (describe-paths *location* *edges*)
	  (describe-objects *location* *objects* *object-locations*)))

;; 指定した方角へ進む
(defun walk (direction)
  (let ((next (find direction
		    (cdr (assoc *location* *edges*))
		    :key #'cadr)))
    (if next
	(progn (setf *location* (car next))
	       (look))
	'(you cannot go that way.))))

;; オブジェクトを取る
(defun pickup (object)
  (cond ((member object
		 (objects-at *location* *objects* *object-locations*))
	 (push (list object 'body) *object-locations*)
	 `(you are now carrying the ,object))
	(t '(you cannot get that.))))

;; 持ってるものを調べる
(defun inventory()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))


;; ゲーム用のread
(defun game-read ()
  (let ((cmd (read-from-string
	      (concatenate 'string "(" (read-line) ")"))))
	(flet ((quote-it (x)
		 (list 'quote x)))
	  (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

;; ゲーム用の評価関数
(defparameter *allowed-commands* '(look walk pickup inventory))
(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(i do not know that command.)))

;; 見やすい表示にする
(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
	  (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
	    ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
	    ((eql item #\") (tweak-text rest caps (not lit)))
	    (lit (cons item (tweak-text rest nil lit)))
	    (caps (cons (char-upcase item) (tweak-text rest nil lit)))
	    (t (cons (char-downcase item) (tweak-text rest nil nil)))))))
(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
						  (prin1-to-string lst))
				     'list)
			     t
			     nil)
		 'string))
  (fresh-line))

;; ゲーム用のインタフェース定義
(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))