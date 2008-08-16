
(mac propage body
 `(tag html
		(tag head
			(tag (script src "processing.js"))
			(tag (script src "init.js")))
		(tag body
			,@body)))

(mac canvas (w h . body)
 `(do (tag (script type "application/processing")
				,@body)
			(tag (canvas width ,w height ,h))))

(def lp () #\( )
(def rp () #\) )
(def lc () #\{ )
(def rc () #\} )

(= pvars* nil)

(def ptype (val)
	(if (acons val)
			 'float ; wild guess
			(in val t nil)
			 'boolean
			(isa val 'num)
			 'float
			(isa val 'int)
			 (if (find #\. (string val))
					 'float
					 'int)))

(def p= (vars)
	(tostring
		(map pr
			(map (fn ((var val))
						 (string (if (mem var pvars*)
												 ""
												 (do (push var pvars*)
														 (string (ptype val) " ")))
										 var " = " (peval val) ";\n"))
				(pair vars)))))

(def p+= (exprs)
	(tostring
		(map pr
			(map (fn ((var expr))
						 (string var " += " (peval expr) ";\n"))
				(pair exprs)))))

(def pfn (name expr)
	(tostring
		(pr name (lp))
		(prall (map peval expr))
		(pr (rp))))

(def pdef (name exprs)
	(tostring
		(prn 'void " " name "()" (lc))
		(prall (map peval exprs) "" "")
		(prn (rc))))

(def pif (exprs)
	(tostring
		(pr 'if (lp))
		(pr (peval exprs.0)) ; test
		(prn (rp) (lc))
		(pr (peval exprs.1)) ; then
		(prn (rc))
		(if (is len.exprs 3) ; else
				(do (prn " else " (lc))
						(prn (peval exprs.2) (rc))))))

(def end (s)
	(string s ";\n"))

(def psign (sign exprs)
	(string (lp) (peval exprs.0) " " sign " " (peval exprs.1) (rp)))

(def peval (expr)
	(if (atom expr)
			(case expr
				t				'true
				nil			'false
				width		'width
				height	'height
				mousex	'mouseX
				mousey	'mouseY
				rgb			'RGB
				center	'CENTER
								expr)
;				(aif (posmatch "." (string expr))
;						 ;(peval (list (cut expr 0 it) (cut expr it)))
;						 expr
;						 expr))
			(case expr.0
				=					(p= cdr.expr)
				dist			(pfn 'dist cdr.expr)
				setup			(pdef 'setup cdr.expr)
				nostroke	(string (pfn 'noStroke nil) ";\n")
				draw			(pdef 'draw cdr.expr)
				bg				(end (pfn 'background cdr.expr))
				sqr				(end (pfn 'rect (list expr.1 expr.2 expr.3 expr.3)))
				circ			(end (pfn 'ellipse (list expr.1 expr.2 expr.3 expr.3)))
				;+=				(string expr.1 " += " (peval expr.2) ";\n")
				+=				(p+= cdr.expr)
				-=				(string expr.1 " -= " (peval expr.2) ";\n")
				radians		(pfn 'radians cdr.expr)
				+					(psign "+" cdr.expr)
				-					(psign "-" cdr.expr)
				*					(psign "*" cdr.expr)
				/					(psign "/" cdr.expr)
				cos				(pfn 'cos cdr.expr)
				sin				(pfn 'sin cdr.expr)
				if				(pif cdr.expr)
				>					(string (peval expr.1) " > " (peval expr.2))
				<					(string (peval expr.1) " < " (peval expr.2))
				colormode	(end (pfn 'colorMode cdr.expr))
				rgb				"colorMode(RGB, 255, 255, 255, 100);"
				standard	"colorMode(RGB, 255, 255, 255, 100);\nsmooth();\nnoStroke();\n"
				rectmode	(end (pfn 'rectMode cdr.expr))
				half			(string (lp) (peval expr.1) " / " 2 (rp))
									(end (pfn expr.0 cdr.expr)))))

(mac pro exprs
 `(do (= pvars* nil)
			(tostring
				(map pr (map peval ',exprs)))))


; examples

(defop sincos req
	(propage
		(canvas 200 200
			(pr
				(pro
					(= i 45
						 j 225
						 pos1 0.0
						 pos2 0.0
						 pos3 0.0
						 pos4 0.0
						 sc 40)
					(setup
						(size 200 200)
						(nostroke)
						(smooth))
					(draw
						(bg 0)
						(fill 51)
						(sqr 60 60 80)
						(fill 255)
						(circ pos1 36 32)
						(circ pos3 164 32)
						(fill 153)
						(circ 36 pos2 32)
						(circ 164 pos4 32)
						(+= i 3)
						(-= j 3)
						(if (> i 405)
								(= i 45 j 225))
						(= ang1 (radians i)
							 ang2 (radians j)
							 w (/ width 2)
							 pos1 (+ w (* sc (cos ang1)))
							 pos2 (+ w (* sc (sin ang1)))
							 pos3 (+ w (* sc (cos ang2)))
							 pos4 (+ w (* sc (sin ang2))))))))))

(defop mouse1 req
	(propage
		(canvas 200 200
			(pr
				(pro
					(setup
						(size 200 200)
						(nostroke)
						(colormode rgb 255 255 255 100)
						(rectmode center))
					(draw
						(bg 51)
						(fill 255 80)
						(sqr mousex (half height) (+ (half mousey) 10))
						(sqr (- width mousex) (half height)
								 (+ (half (- height mousey)) 10))))))))

(defop mouse2 req
	(propage
		(canvas 200 200
			(pr
				(pro
					(setup
						(size 200 200)
						(nostroke)
						(colormode rgb 255 255 255 100)
						(rectmode center))
					(draw
						(bg 0)
						(fill 0 255 255 50)
						(sqr mousex (half height) (+ (half mousey) 10))
						(fill 255 255 0 50)
						(sqr (- width mousex) (half height)
								 (+ (half (- height mousey)) 10))))))))

(defop simple1 req ; pg 37
	(propage
		(center
			(canvas 800 600
				(pr
					(pro
						(= x 0 y 0)
						(setup
							(size 800 600)
							(standard))
						(draw
							(bg "#000000")
							(fill "#ffffff")
							(circ x y 100)
							(+= x 1 y 1))))))))

(defop motion1 req ; pg 68
	(propage:center:canvas 800 600
		(pr:pro
			(= angle 0.0 y 0.0)
			(setup
				(size 800 600)
				(standard))
			(draw
				(bg "#cccccc")
				(fill "#ff0000")
				(= y (+ (half height) (* (sin angle) 100)))
				(circ (half width) y 50)
				(+= angle 0.2)))))

(defop motion2 req ; pg 68
	(propage:center:canvas 800 600
		(pr:pro
			(= angle 0.0 x 0.0 y 0.0)
			(setup
				(size 800 600)
				(standard))
			(draw
				(bg "#cccccc")
				(fill "#ff0000")
				(= y (+ (half height) (* (sin angle) 200)))
				(circ x y 50)
				(+= angle 0.2 x 2)))))









