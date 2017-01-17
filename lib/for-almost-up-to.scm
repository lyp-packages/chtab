(define-module (scm for-almost-up-to)
	#:use-syntax (ice-9 syncase) 
  #:export-syntax (for))

; usage: (for i almost-up-to 10 (begin (display i)(newline)))  displays #s from 0 to 9
(define-syntax for (syntax-rules (almost-up-to) 
		((for xy1 almost-up-to xy2 xy-rest)(do ((xy1 0 (1+ xy1)))((= xy1 xy2)) xy-rest))))
		
