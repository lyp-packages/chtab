(lyp:load "for-almost-up-to.scm")
(use-modules (scm for-almost-up-to)) 

(define (chtab:algorithm ly-music) (begin 
;
; ch means Chuanjun He or Christopher Heckman ... take your pick
; 
	(letrec	( 

		;; parameters
		(max-fret 24)
		(hand-width 5)
		(string-number-strength 100) ; bigger will obey supplied string numbers
		(use-open-strings #t)	; for now, although this will change soon
		(use-harmonics #f) 		; for now
		(keep-this-many-voicings 4) 
			; how many "best tabs" should we keep track of, per chord in the music-semitones-no-overlap?
	
		(display-matrix (lambda () (display "\n matrix: \n")
			(hash-for-each (lambda (xy xyz) 
				(writeln (list xy xyz 
					(list (list (1- (car xy)) (car xyz) (caddr xy))
						xyz
						(list-ref music-semitones (car xy))
						(list-ref (hash-ref chord-positions (list-ref music-semitones (car xy))) 
							(cadr xy))
					)
				))) 
				matrix)
			(newline))) ; for debugging
	
	
	
	
		;; lots and lots of procedures; first some one-line utilities
		(list-width (lambda (xy) (if (> 2 (length xy)) 0 (- (apply max xy) (apply min xy)))))
		(list-average (lambda (xy) (/ (* 1.0 (apply + xy)) (length xy))))
		(fret-average (lambda (xy) (list-average (sf-list->frets xy))))
		(list-non-zero-average (lambda (xy) (list-average (remove zero? xy)) ))
		(list-position-data (lambda (xy) (remove (lambda (xyz) (not (integer? xyz))) xy) ))
		(get-pitch (lambda (xy) (ly:music-property xy 'pitch)))
		(get-penalty (lambda (xy) (last xy)))
		(NoteEvent->semitones (lambda (xy) (ly:pitch-semitones (get-pitch xy))))
		(moment=? (lambda (a b) (ly:moment? ly:moment?)(not (or (ly:moment<? a b) (ly:moment<? b a)))))
		(isChord? (lambda (xy) (eq? (ly:music-property xy 'name) 'EventChord)))
		(isNote? (lambda (xy) (eq? (ly:music-property xy 'name) 'NoteEvent)))
		(fs->semitones (lambda (xy1 xy2)(+ (list-ref tuning xy2) xy1)))
		(sf->semitones (lambda (xy2 xy1)(+ (list-ref tuning xy2) xy1)))
		(writeln (lambda (xy) (display xy)(newline)))

		;; multi-line utilities

		(string-of-semitones-in-chord (lambda (x y)
			(if (> 2 (length y)) #f
			(if (= x (sf->semitones (car y) (cadr y)))
				(car y)
				(string-of-semitones-in-chord x (cddr y))
			))))

		; (s f s ... f s f p) -> (s s ... s)
		(sf-list->strings (lambda (xy)
			(if (> 2 (length xy)) '() (append (list (car xy)) (sf-list->strings (cddr xy))))))

		; (s f s f s ... f p) -> (f f ... f)
		(sf-list->frets (lambda (xy)
			(if (> 2 (length xy)) '() (append (list (cadr xy)) (sf-list->frets (cddr xy))))))

		(sort-by-car-moment (lambda (xy) (sort (delete-duplicates xy) 
			(lambda (a b) (ly:moment<? (car a) (car b))))))
		
		(index-of-moment (lambda (xy) (list-index (lambda (xyzw) 
			(moment=? xyzw xy)) critical-moments)))

		;; now the main algorithm.
		
		; build original-notes, critical-moments, and same-string-conditions
		(get-original-notes (lambda* (xy now) (ly:music? ly:moment?) 
			(let ((
				my-name (ly:music-property xy 'name) )
				(my-duration (ly:music-property xy 'duration)) ; for now
				(my-moment '()) 		; for now
				(property-ops '())		; for now
				(stringTunings-index 0) ; for now
				(new-tuning '()) 		; for now
				(articulation-names '()); for now
				(prev-object '())		; used when analyzing prev-ties
				(this-object '())		; used when analyzing prev-ties
				(num-semitones 0)		; used when analyzing prev-glisses
				)
			(if (< 0 (length prev-ties))(begin
				(if (isChord? (car prev-ties))(begin
					(set! prev-object '())
					(for-each (lambda (xyz) 
						(if (isNote? xyz) (set! prev-object (append prev-object (list xyz)))))
						(ly:music-property (car prev-ties) 'elements))
					(set! prev-ties prev-object)
					))
				(for-each (lambda (xyz)
					(set! same-string-conditions (append same-string-conditions
						(list (list now (NoteEvent->semitones xyz) (NoteEvent->semitones xyz)))
						)))
					prev-ties)
				))
			(if (and (< 0 (length prev-glisses)) (member my-name '(EventChord NoteEvent))) (begin
				(set! prev-object '())
				(set! this-object '())
				(if (isChord? (car prev-glisses))
					(for-each (lambda (xyz) 
						(if (isNote? xyz) (set! prev-object 
							(append prev-object (list (NoteEvent->semitones xyz))))))
						(ly:music-property (car prev-glisses) 'elements))
					(set! prev-object (list (NoteEvent->semitones (car prev-glisses)))) ; one note
					)
				(cond ((isChord? xy)
					(for-each (lambda (xyz) 
						(if (isNote? xyz)
							(set! this-object (append this-object (list (NoteEvent->semitones xyz)))))	
						) (ly:music-property xy 'elements)))
					((isNote? xy) (set! this-object (list (NoteEvent->semitones xy))))
					)
				(if (> (length this-object) 0)(begin
	 				(set! same-string-conditions 
						(append same-string-conditions
							(map (lambda (xyzw)
								(list now 
									(list-ref this-object (cdr xyzw))
									(list-ref prev-object (car xyzw)) 
									)
								)
								(filter (lambda (xyz)
									(and (< (car xyz) (length prev-object)) 
										(< (cdr xyz) (length this-object)))
									)
									(if (null? glissando-map) 
										(map (lambda (xyz) (cons xyz xyz))(iota (length this-object)))
										glissando-map))
								)))
					(set! prev-glisses '())
					))
				))
			(set! prev-ties '())
			(case my-name ; ignore most events
				((NoteEvent) 
					(set! original-notes (append original-notes (list (list now xy))))
					(set! critical-moments (append critical-moments 
							(list now (ly:moment-add now (ly:duration-length my-duration)))))
					(set! articulation-names (map 
						(lambda (xy) (ly:music-property xy 'name))
						(ly:music-property xy 'articulations)
						))
					(if (member 'GlissandoEvent articulation-names)
						(set! prev-glisses (list xy)))
					(if (member 'TieEvent articulation-names)
						(set! prev-ties (append prev-ties (list xy))))
					(if (member 'StringNumberEvent articulation-names)
							(set! string-number-conditions (append string-number-conditions 
								(list (list now (NoteEvent->semitones xy) 
									(1- (ly:music-property (find 
										(lambda (x3) (eq? (ly:music-property x3 'name) 'StringNumberEvent))  
										(ly:music-property xy 'articulations)) 'string-number)
									)))
								))) 
						
					)
				((EventChord SimultaneousMusic)
					(for-each (lambda (xyzw) (get-original-notes xyzw now))
						(ly:music-property xy 'elements))
					(set! articulation-names (map 
						(lambda (xyz) (ly:music-property xyz 'name))
						(ly:music-property xy 'elements)))
					(if (eq? my-name 'EventChord)(begin
						(if (memq 'GlissandoEvent articulation-names)(set! prev-glisses (list xy)))
						(if (memq 'TieEvent articulation-names)
							(set! prev-ties (append prev-ties (list xy))))
						))
					)
				((RelativeOctaveMusic GraceMusic)
					(get-original-notes (ly:music-property xy 'element) now))
				((SequentialMusic)
					(for-each (lambda (xyzw)
						(get-original-notes xyzw now)
						(set! my-duration (ly:music-property xyzw 'duration))
						(if (null? my-duration) (begin
							(if (eq? 'EventChord (ly:music-property xyzw 'name))
								(set! my-duration 
									(ly:music-property 
										(car (ly:music-property xyzw 'elements)) 'duration)
									)
								)
							))
						(if (not (null? my-duration))
							(set! now (ly:moment-add now (ly:duration-length my-duration)))
							)
						)
						(ly:music-property xy 'elements)
						)
					)
				((ContextSpeccedMusic)
					(get-original-notes (ly:music-property xy 'element) now)
					(if (not (null? (ly:music-property xy 'property-operations)))
						(begin 
							(set! property-ops (car (ly:music-property xy 'property-operations))) 
							(set! stringTunings-index (list-index (lambda (xyz) (eq? xyz 'stringTunings)) 
								property-ops))
							(set! new-tuning (list-ref property-ops (1+ stringTunings-index)))
							(set! tuning (map (lambda (xyz) (ly:pitch-semitones xyz)) new-tuning))
							(set! tuning-not-found #f)
							)
						)	
					)
				((PropertySet) (set! glissando-map (ly:music-property xy 'value)))
				)
			)))
			
		;; get notes and put them in music-semitones-no-overlap
		; builds music-semitones-no-overlap, isd
		(build-music-semitones-no-overlap (lambda () 
			(let ((i 0) (my-duration 0) (moment-on 0) (moment-off 0)) 
			(set! music-semitones-no-overlap (make-list (length critical-moments)))
			(for-each (lambda (xy) 
				(set! moment-on (list-index (lambda (xyz) (moment=? (car xy) xyz)) critical-moments))
				(set! my-duration (ly:music-property (cadr xy) 'duration))
				(set! i moment-on)
				(set! isd (append isd 
					(list (list (car xy) (NoteEvent->semitones (cadr xy)) my-duration))
					))
				; next line might go away
				(list-set! music-semitones-no-overlap i  
					(append (list-ref music-semitones-no-overlap i) (list (NoteEvent->semitones (cadr xy)))) 
					)
				)
				original-notes)
			)))
		
		; convert moments to integers in note-ties, isd.
		(replace-moments-in-notes-and-glisses-and-isd (lambda ()
			(set! note-ties (map (lambda (xyz) 
				(list (index-of-moment (car xyz)) (cadr xyz))
				) 
				note-ties))
			(set! isd (map (lambda (xyz) 
				(list 
					(index-of-moment (car xyz))
					(cadr xyz)
					(- (index-of-moment (ly:moment-add (car xyz) (ly:duration-length (caddr xyz)))) 
						(index-of-moment (car xyz)))
					)
				) 
				isd))
			))
		
		; collate and build 'chords', consisting of all notes being played at a particular time.
		; (a.k.a. Algorithm E) builds music-semitones. to be modified
		(build-music-semitones (lambda () (letrec (
				(remaining-notes isd)	; notes we haven't looked at yet
				(chordnum 0)			; chord number
				(queue '())				; queue
				(queue-length 0)		; length at a given time
				(object '())			; element of isd
				(push-queue (lambda (x1)(set! queue (append queue (list x1)))))
				(pop-queue (lambda () (set! object (car queue))
					(set! queue (cdr queue))))
				)
			(set! music-semitones (make-list len-music-semitones))
			(for-each (lambda (xy) (push-queue xy))
				(filter (lambda (x2) (eq? chordnum (car x2))) isd))
			(set! remaining-notes (drop-while (lambda (x2) (eq? chordnum (car x2))) remaining-notes))
			(do ((chordnum 1 (1+ chordnum)))
				((>= chordnum len-music-semitones))
				(set! queue-length (length queue))
				(for i almost-up-to queue-length (begin
					(pop-queue)
					(list-set! music-semitones (car object) 
						(append (list-ref music-semitones (car object)) (list (cadr object)))
						)
					(if (< 1 (caddr object)) (begin
						;; add to music-semitones, add condition to note-ties
						(push-queue (list chordnum (cadr object) (1- (caddr object))))
						(set! note-ties (append note-ties (list (list (car object) (cadr object)))))
						(list-set! music-semitones chordnum (append (list-ref music-semitones chordnum)
							(list (cadr object))))
						))
					))
				(set! queue (append queue 
					(filter (lambda (x2) (eq? chordnum (car x2))) remaining-notes)
					))
				(set! remaining-notes (drop-while (lambda (x2) (eq? chordnum (car x2))) remaining-notes))
				)
				(set! music-semitones (map (lambda (x1) 
					(sort (delete-duplicates x1)(lambda (a b) (> a b)))) music-semitones))
			)))

		; find where each note can be played on the instrument
		; builds note-positions
		(get-note-positions-from (lambda (mx) 
			(for-each (lambda (xy)
				(for-each (lambda (xyz) 
					(if (not (hash-ref note-positions xyz))
						(let* ((finger-list '()) (count 0))
							(for-each (lambda (xyzw)
								(if (<= xyzw xyz (+ xyzw max-fret))
									(set! finger-list (append finger-list 
										(list (list count (- xyz xyzw)))))
									)
								(set! count (1+ count))
								)
								tuning)
							(hash-set! note-positions xyz finger-list)
							)
						)
					)
					xy))
				mx
			)))

		; a helper for calculate-all-voicing-penalties, and a neat hack on its own
		; (cross-lists ((a b) (1 2) (z))) is ((a 1 z) (a 2 z) (b 1 z) (b 2 z)) (in some order)
		(cross-lists (lambda (xy) 
			(if (and (= (length xy) 1) (list? xy))
				(car xy)
				(let ((outlist '()))
					(for-each (lambda (xyz)
						(for-each (lambda (xyzz)
							(set! outlist (append outlist (list (append xyz xyzz))))
							)
							(cross-lists (cdr xy))
							))
						(car xy))
					outlist
					)
				)
			))


		; another helper for calculate-all-voicing-penalties
		; calculates the internal penalty. Note this cannot be an integer, or the program crashes!
		
		(internal-penalty (lambda (xy) 
			(if (= 1 (length xy)) 0.0001
			(* 1.0 (+
					(list-width (remove zero? (sf-list->frets xy)))
					(length (sf-list->frets xy))
					(if (member 0 (sf-list->frets xy)) -1.0 0.0)
					(sqrt (list-average (sf-list->frets xy)))
					(log (+ 1 (apply + (sf-list->strings xy))))
				))
			)))
		
		; list all ways to play 'chords', and provide internal penalty ("fitness") for each
		; builds chord-positions, a hash from a list of semitones to a list of "positions"
		; each "position" looks like (f s f s ... f s p), where (f s) is a fret-string pair
		;		and p is the non-integral internal penalty.
		(calculate-all-voicing-penalties (lambda ()
			(set! chord-positions (make-hash-table (length critical-moments)))
			(hash-set! chord-positions '() (list (list 0.0001)))
			(for-each (lambda (xy) (let 
				((position-list-temp '())(position-list '())(sorted-xy (sort xy >)))
				(if (not (hash-ref chord-positions sorted-xy)) (begin
					(set! position-list-temp (cross-lists (map (lambda (xyz) 
							(hash-ref note-positions xyz)) sorted-xy)))
					(set! position-list '())
					(for-each (lambda (xyz) 
						(if (= (length (delete-duplicates (sf-list->strings xyz))) 
								(length (sf-list->strings xyz))) 
							(set! position-list (append position-list (list xyz)))
							)
						)
						position-list-temp
						) ; can't have two notes on one string
					(set! position-list-temp '())
					(for-each (lambda (xyz) (let 
						((xyzw (delete-duplicates (sf-list->frets xyz))))
						(if use-open-strings (set! xyzw (delete 0 xyzw)))
						(set! xyzw (sort xyzw <))
						(if (>= hand-width (list-width xyzw)) 
							(set! position-list-temp (append position-list-temp 
								(list (append xyz (list (internal-penalty xyz))))))
							)
						))
						position-list
						) ; remove if the frets are too far apart
					(hash-set! chord-positions sorted-xy position-list-temp)
					(if (= 0 (length (hash-ref chord-positions sorted-xy)))
						(ly:error "one of your 'chords' cannot be played in this tuning\n")
						)
					))
				))
				music-semitones-no-overlap
				)
			))

		; Calculates violations if a note is played on the "wrong" string
		
		(add-string-number-violation-penalties (lambda () (let ((p 0) (chordnum 0))
			(for chordnum almost-up-to (length music-semitones)(begin
				(for-each (lambda (xy)
					(set! p 0)
					(for-each (lambda (xyzw) 
						(if (= (string-of-semitones-in-chord (cadr xyzw) xy) (caddr xyzw))
							0 (set! p (1+ p)))
					(list-set! xy (1- (length xy)) (+ (* string-number-strength p) (last xy)))
					) (filter (lambda (xyzwv) (eq? chordnum (car xyzwv))) string-number-conditions))
					) (hash-ref chord-positions (sort (list-ref music-semitones chordnum) >)))
				))
			)))

		; helper for set-up-matrix. main calculator for external (chord-to-chord) penalties
		(external-penalty (lambda (chord-num pos-num prev-chord-num old-pos-num) (letrec (
			(arguments (list (list-ref (hash-ref chord-positions 
						(sort (list-ref music-semitones chord-num) >)) pos-num)
					(list-ref (hash-ref chord-positions 
						(sort (list-ref music-semitones prev-chord-num) >)) old-pos-num)
					chord-num)
				))
			(* 1.0 (+
				(apply standard-external-penalty arguments)
				(apply same-string-violation-penalty arguments)
			)))))
		
		(same-string-violation-penalty (lambda (xy xyz chordnum) 
			(* 1e12 (apply + (map (lambda (xyzw) 
					(if (= (string-of-semitones-in-chord (cadr xyzw) xy)
						(string-of-semitones-in-chord (caddr xyzw) xyz))
						0 1))
					(filter (lambda (xyzwv) (eq? chordnum (car xyzwv))) same-string-conditions)))
				)
			))

		; helper for external-penalty: the default external penalty (no ties or gliss's)
		(standard-external-penalty (lambda (xy xyz xyzw)
			(if (or (= 1 (length xy)) (= 1 (length xyz))) 0.0001
			(* 1.0 (+
					(abs (- 
						(fret-average (list-position-data xy)) 
						(fret-average (list-position-data xyz))
						))
				))
			)))

		; sets up He's "matrix"
		(set-up-matrix (lambda()
		(set! chord-poses (hash-ref chord-positions (sort (list-ref music-semitones 0) >)))
 			(for i almost-up-to (length (hash-ref chord-positions 
 					(sort (list-ref music-semitones 0) >))) (begin
				(hash-set! matrix (list 0 i 0) (list -1 -1 (last (car chord-poses))))
				(set! chord-poses (cdr chord-poses))
				))
		 	(for prev-chord-num almost-up-to (1- len-music-semitones) (begin
				(set! chord-num (1+ prev-chord-num))
				(set! num-pos (length (hash-ref chord-positions 
					(list-ref music-semitones chord-num))))
				(for pos-num almost-up-to num-pos (begin
					(set! my-penalty (last (list-ref (hash-ref chord-positions 
						(sort (list-ref music-semitones chord-num) >)) pos-num)))
					(set! num-old-pos (length (hash-ref chord-positions 
						(list-ref music-semitones prev-chord-num))))
					(set! contenders '())
					(for old-pos-num almost-up-to num-old-pos
						(for old-path-num almost-up-to num-paths (begin
							(set! contenders (append contenders (list (list 
								old-pos-num 
								old-path-num
								(+	(get-penalty (hash-ref matrix (list prev-chord-num 
										old-pos-num old-path-num)))
									my-penalty
									(external-penalty chord-num pos-num prev-chord-num old-pos-num)
									)
								))))
							))
						) ; end for old-pos-num
					(set! contenders (sort-list contenders (lambda (xy1 xy2) (< (last xy1) (last xy2))) 
						))
					(if (< keep-this-many-voicings (length contenders)) 
						(set! contenders (list-head contenders keep-this-many-voicings)) )
					(for kth almost-up-to (length contenders)
						(hash-set! matrix (list chord-num pos-num kth) (list-ref contenders kth))
						)
					)) ; end for-pos-num
				(set! num-best num-paths)
				(set! num-paths (length contenders))
				)) ; end for prev-chord-num
			))

		; helper for assign-frets. Assign string for one note
		(set-string! (lambda (xy xyz)
			(set! (ly:music-property xy 'articulations) 
					(cons (make-music 'StringNumberEvent 'string-number xyz)
						(ly:music-property xy 'articulations)))
			))


		; helper for set-frets. assigns frets for one "chord"
		(assign-frets (lambda (voicing) 
			(if (< 1 (length voicing))(begin
				(set! pitch-to-change (sf->semitones (car voicing) (cadr voicing)))
				(set! note-to-change (find (lambda (xy)
					(= pitch-to-change (NoteEvent->semitones (cadr xy))))
					original-notes))
				(set-string! (cadr note-to-change) (1+ (car voicing)))
				(set! original-notes (delete note-to-change original-notes))
				(assign-frets (cddr voicing))
				))
			))
		
		; walk through the matrix and pick out the positions in the "best" tab
		(set-frets (lambda (ch-num v-num kth-best) ; where's the bug? no string assignment!?
			(let* (
				(chunk (hash-ref matrix (list ch-num v-num kth-best)))
				(p-v-num (car chunk))
				(p-kth-best (cadr chunk))
				(this-chord (list-ref music-semitones-no-overlap ch-num))
				)
				(assign-frets (list-ref (hash-ref chord-positions (sort this-chord >)) v-num))
				(if (> ch-num 0) (set-frets (1- ch-num) p-v-num p-kth-best))
				)
			))


		;; the following variables will be filled in later.
		(note-positions (make-hash-table 100)) 
			; keys: Notes from music-semitones-no-overlap, values: positions where note can be played
		(chord-positions 0) 
			; keys: Chords from music-semitones-no-overlap, values: positions where chords can be played
		(matrix (make-hash-table 100)) ; ch's MAT
			; from (chord#, voicing#, best#)  : 0 <= best# < (list-ref num-paths chord#)
			; to (voicing#, best#, cumulative-penalty) : chord# is one less
		(harmonics-list '()) 	; only need if use-harmonics is #t. NOT YET IMPLEMENTED
		(note-ties '())			; list of (mom semitones dur): ties in tablature. NOT YET IMPLEMENTED
		(same-string-conditions '())	; list of (mom semitones semitones) 
								; 		(later (int semitones semitones)) for ties and slurs
								;		semitones1 in chord# index is on the same string as 
								;		semitones2 in chord# (1- index).
		(string-number-conditions '())	; list of (moment semitones string-number) suggesting that that note
								;	be played on that string at that point
		(num-paths 1)			; number of paths to next level, per position,
								;	per position, bounded above by track-paths
		(music-semitones-no-overlap '())	; list of list of semitones: the chords themselves
		(isd '())				; list of (semitones mom dur)
		(music-semitones '())	; list of list of semitones: the chords themselves, with ties and long notes broken up
		(strings 6) 			; unless we find out otherwise
		(tuning '(4 -1 -5 -10 -15 -20))	; default: guitar 
		(tuning-not-found #t)	; was a string tuning found in the music-semitones-no-overlap?
		(critical-moments '())	; list of Moment's: when notes start and stop
		(original-notes '())	; list of NoteEvent's
		(glissando-map '())		; used when 'chords' are glissed to other 'chords'
		(len-music-semitones 0) ; number of chords
		(chord-num 1)			; chord#
		(num-pos 0)				; number of positions for chord#
		(pos-num 0)				; position number for chord#
		(my-penalty 0)			; penalty for this position
		(chord-poses '())		; list of chord positions (voicings)
		(prev-chord-num 0)		; chord#-1
		(num-old-pos 0)			; number of positions in chord#-1
		(old-pos-num 0) 		; position number pos2 for chord#-1 
		(num-best 1)			; number of paths from chord#-1 to check out
		(old-path-num 0)		; which path we're taking from chord#-1, old-pos-num
		(contenders '())		; all paths into current voicing (ch#, pos#)
		(kth 0)					; loop var for contenders
		(prev-ties '())			; previous semitone(s), if last 'chord' was tied
		(prev-glisses '())		; previous note(s)/chord, if it was glissed to this one
								; prev-tie and prev-gliss are only used in the case of SequentialMusic
		(pitch-to-change 0)		; in assign-frets
		(note-to-change 0)		; in assign-frets
	)
;		(display "\n-------------------------- start of chTab --------------------------\n")
;		(display-scheme-music ly-music)(newline)
	(get-original-notes ly-music (ly:make-moment 0/1))
	(set! original-notes (sort original-notes (lambda (a b) (ly:moment<? (car a) (car b)))))
;		(for-each (lambda (xy) (display (car xy))(display-lily-music (cadr xy) parser)) original-notes)
	(set! critical-moments (sort (delete-duplicates critical-moments) ly:moment<?))
;		(display 'critical-moments:)(display critical-moments)(newline) 
	(set! same-string-conditions (map (lambda (xyz)
			(cons (index-of-moment (car xyz))(cdr xyz)))
			same-string-conditions))
;		(display 'same-string-conditions:)(display same-string-conditions)(newline)
	(set! string-number-conditions (map (lambda (xyz)
			(cons (index-of-moment (car xyz))(cdr xyz)))
			string-number-conditions))
;		(display 'string-number-conditions:)(display string-number-conditions)(newline)
	(build-music-semitones-no-overlap)
	(set! isd (sort-by-car-moment isd))
	(replace-moments-in-notes-and-glisses-and-isd) 
;		(display 'music-semitones-no-overlap:)(display music-semitones-no-overlap)(newline)
;		(display 'isd:)(display isd)(newline)
	(set! len-music-semitones (length critical-moments))
	(build-music-semitones) ; with overlap 
;		(display 'music-semitones:)(display music-semitones)(newline)
	(get-note-positions-from music-semitones-no-overlap)
;		(display 'note-positions:)(newline)
;		(hash-for-each (lambda (xy xyz) (display (list xy xyz))(newline)) note-positions)(newline)
;		(display "\n original notes: \n")
;		(for-each (lambda (xy) (display (car xy))(display-lily-music (cadr xy) parser)) original-notes)
	(for-each (lambda (xy)	
		(if (zero? (length (hash-ref note-positions (NoteEvent->semitones (cadr xy)))))
			(ly:error "~s cannot be played on this instrument; other pitches might also be unplayable\n" 
				(get-pitch (cadr xy)))
			)) original-notes)
	(calculate-all-voicing-penalties)
	(add-string-number-violation-penalties)
;		(display "\n------------- input data -------------\n")
;		(display (list 'tuning: tuning))(newline)
;		(display "\n chord-positions: \n")
;		(hash-for-each (lambda (xy xyz) (display (list xy xyz))(newline)) chord-positions)(newline)
;		(display "\n--- warning: ignoring gliss's and ties and harmonics for now ---\n")
	(set-up-matrix)
;		(display-matrix)
;		(display "tracing back ...\n")
	(set! original-notes (reverse original-notes))
	(set-frets (1- len-music-semitones) 0 0) ; start off tablature retrace at right place
;
;		(display "--------------------------- end of chTab ---------------------------\n")
	ly-music
	)))
