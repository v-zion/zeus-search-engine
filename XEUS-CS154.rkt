#lang racket
(require racket/gui/base)
(require racket/draw net/url)
(require net/sendurl)
(require sxml)
(require (planet neil/html-parsing:3:0)) ; will be downloaded when code is first run
(require math/matrix)

;Abstraction for frames
(define (frame-h)                          
  (new frame% [label "Xeus"]
       [width 1000]
       [height 500]))

(define sframe (frame-h))             ;   search-frame
(define rframe (frame-h))             ;   result-frame

;Abstraction for logos
(define (logo-h url-h)
  (read-bitmap (get-pure-port (string->url url-h))))

;Abstraction for buttons
(define (button-h lab)
  (new button% [parent dialg]
       [label lab]
       [vert-margin 5]
       [callback (lambda (button event)
                   (begin (set-field! lst c-vas (search lab))
                          (send c-vas refresh)
                          (send dialg show #f)))]))

;Abstraction for text-field
(define (query pfram)
  (new text-field% [parent pfram]
       [label #f]
       [vert-margin 5]
       [style '(single)]
       [horiz-margin 200]))

(define logo-s (logo-h "file:///home/animesh/Desktop/CS154 Project/Capture2_1.jpg"))
(define logo-r (logo-h "file:///home/animesh/Desktop/CS154 Project/Capture2.jpg"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Change callback to PROJECT REPORT
(void (new button% [parent sframe]
           [label logo-s]
           [vert-margin 30]
           [callback (lambda (button event)
                       (send-url "http://docdro.id/7YyjNDz"))]))

(void (new button% [parent rframe]
           [label logo-r]
           [vert-margin 5]
           [callback (lambda (button event)
                       (send sframe show #t)
                       (send rframe show #f))]))

(define msg (new message% [parent sframe]
                 [label "Enter your search query:"]                 
                 [font (make-object font% 10 'default 'normal 'bold)]))

(define query-s (query sframe))        
(define query-r (query rframe))

(define button1-r
  (new button% [parent rframe]
       [label "Search"]
       [vert-margin 5]
       [style '(border)]
       [callback (lambda (button event)
                   (begin (set! word (send query-r get-value))
                          (button-dym word)))]))

(define button1
  (new button% [parent sframe]
       [label "Search"]
       [vert-margin 5]
       [style '(border)]
       [callback (lambda (button event)
                   (begin (set! word (send query-s get-value))
                          (button-dym word)
                          (send rframe show #t)
                          (send sframe show #f)))]))

(define button2
  (new button% [parent sframe]
       [label "I'm feeling lucky"]
       [vert-margin 5]
       [callback (lambda (button event)
                   (begin (set! word (send query-s get-value))
                          (send-url (get-field url (list-ref (search word) 0)))))]))


(define dialg
  (new dialog% [label "Did you mean?"]
       [width 700]
       [height 300]))

(define word "")

(define (button-dym word)
  (define (buttons l r)
    (cond [(and (< r (length l)) (< r 4))
           (begin (button-h (list-ref l r))
                  (buttons l (+ r 1)))]
          [else (send dialg show #t)]))
  (cond [(not (equal? (member word (dym word)) #f)) (begin (set-field! lst c-vas (search word))
                                                           (send c-vas refresh))]
        [else (buttons (dym word) 0)]))

;  STRUCT DEFINITIONS

(struct gnode (val subtrees) #:transparent #:mutable)
(struct leaf (val links) #:transparent #:mutable)

;CLASS DEFINITIONS

(define hyperlink%
  (class object%
    (init-field url)
    (init-field title)
    (init-field text)
    (init-field [pagerank (/ size)])
    (init-field [in-links '()])
    (init-field [stored-name #f])
    (super-new)
    (define/public (send-all-links)
      (map (lambda (link) 
             (let ((a (member link stored-files-list (lambda (a b) (string-contains? (get-field url a) (get-field url b))))))
               (cond [(not (boolean? a)) (send (vector-ref stored-files (- size (length a)))
                                               add-to-inlinks (cons stored-name (/ 1 (length (vector-ref links stored-name)))))])))
           (vector-ref links stored-name)))
    (define/public (add-to-inlinks x)
      (set! in-links (cons x in-links)))))

;GLOBAL VARIABLE DECLARATIONS
(define size 51)    ; size of the web used for searching

(define gtree (gnode 'tree '()))  ; global tree which stores the indexed words

(define links (make-vector size '()))   ; vector which stores the list of outgoing links from a page in the corresponding position 

(define damping-factor 0)  ; damping factor which is used in pagerank algorithm

(define stored-files (make-vector size #f))   ; stored-files is a vector which contains the downloaded webpages as class hyperlink
; after the function call initialize-stored-files

(define (sort1 l)
  (sort l (lambda (a b) (string<? (cdr a) (cdr b)))))

;  a (defined below) is the list of url-title pairs as we cannot get the url directly from the downloaded web-page 
(define a (sort1 '(("https://en.wikipedia.org/wiki/North_Korea" . "North Korea")
                   ("https://en.wikipedia.org/wiki/Angola" . "Angola")
                   ("https://en.wikipedia.org/wiki/Yemen" . "Yemen")
                   ("https://en.wikipedia.org/wiki/Ghana" . "Ghana")
                   ("https://en.wikipedia.org/wiki/Mozambique" . "Mozambique")
                   ("https://en.wikipedia.org/wiki/Nepal" . "Nepal")
                   ("https://en.wikipedia.org/wiki/Uzbekistan" . "Uzbekistan")
                   ("https://en.wikipedia.org/wiki/Malaysia" . "Malaysia")
                   ("https://en.wikipedia.org/wiki/Venezuela" . "Venezuela")
                   ("https://en.wikipedia.org/wiki/Peru" . "Peru")
                   ("https://en.wikipedia.org/wiki/Saudi_Arabia" . "Saudi Arabia")
                   ("https://en.wikipedia.org/wiki/Afghanistan" . "Afghanistan")
                   ("https://en.wikipedia.org/wiki/Morocco" . "Morocco")
                   ("https://en.wikipedia.org/wiki/Canada" . "Canada")
                   ("https://en.wikipedia.org/wiki/Iraq" . "Iraq")
                   ("https://en.wikipedia.org/wiki/Poland" . "Poland")
                   ("https://en.wikipedia.org/wiki/Uganda" . "Uganda")
                   ("https://en.wikipedia.org/wiki/Algeria" . "Algeria")
                   ("https://en.wikipedia.org/wiki/Sudan" . "Sudan")
                   ("https://en.wikipedia.org/wiki/Argentina" . "Argentina")
                   ("https://en.wikipedia.org/wiki/Ukraine" . "Ukraine")
                   ("https://en.wikipedia.org/wiki/Spain" . "Spain")
                   ("https://en.wikipedia.org/wiki/Kenya" . "Kenya")
                   ("https://en.wikipedia.org/wiki/Colombia" . "Colombia")
                   ("https://en.wikipedia.org/wiki/South_Korea" . "South Korea")
                   ("https://en.wikipedia.org/wiki/Myanmar" . "Myanmar")
                   ("https://en.wikipedia.org/wiki/South_Africa" . "South Africa")
                   ("https://en.wikipedia.org/wiki/Tanzania" . "Tanzania")
                   ("https://en.wikipedia.org/wiki/Italy" . "Italy")
                   ("https://en.wikipedia.org/wiki/France" . "France")
                   ("https://en.wikipedia.org/wiki/United_Kingdom" . "United Kingdom")
                   ("https://en.wikipedia.org/wiki/Thailand" . "Thailand")
                   ("https://en.wikipedia.org/wiki/Turkey" . "Turkey")
                   ("https://en.wikipedia.org/wiki/Democratic_Republic_of_the_Congo" . "Democratic Republic of the Congo")
                   ("https://en.wikipedia.org/wiki/Iran" . "Iran")
                   ("https://en.wikipedia.org/wiki/Germany" . "Germany")
                   ("https://en.wikipedia.org/wiki/Egypt" . "Egypt")
                   ("https://en.wikipedia.org/wiki/Vietnam" . "Vietnam")
                   ("https://en.wikipedia.org/wiki/Ethiopia" . "Ethiopia")
                   ("https://en.wikipedia.org/wiki/Philippines" . "Philippines")
                   ("https://en.wikipedia.org/wiki/Japan" . "Japan")
                   ("https://en.wikipedia.org/wiki/Mexico" . "Mexico")
                   ("https://en.wikipedia.org/wiki/Russia" . "Russia")
                   ("https://en.wikipedia.org/wiki/Bangladesh" . "Bangladesh")
                   ("https://en.wikipedia.org/wiki/Nigeria" . "Nigeria")
                   ("https://en.wikipedia.org/wiki/Pakistan" . "Pakistan")
                   ("https://en.wikipedia.org/wiki/Brazil" . "Brazil")
                   ("https://en.wikipedia.org/wiki/Indonesia" . "Indonesia")
                   ("https://en.wikipedia.org/wiki/United_States" . "United States")
                   ("https://en.wikipedia.org/wiki/India" . "India")
                   ("https://en.wikipedia.org/wiki/China" . "China"))))


;FUNCTION DEFINITIONS

(define (initialize-stored-files)     ; initializes elements of vector stored-files to hyperlink class
  (for-each (lambda (i pair) (begin
                               (vector-set! stored-files i
                                            (make-object hyperlink% (car pair) (cdr pair) #f))
                               (set-field! stored-name (vector-ref stored-files i) i)))
            (build-list size (lambda (i) i)) a))

(initialize-stored-files)

(define stored-files-list (vector->list stored-files))

(define (integer->string n)    ; converts integer to string
  (cond [(= (quotient n 10) 0) (string (integer->char (+ n 48)))]
        [else (let* ((a (quotient n 10))
                     (b (remainder n 10)))
                (string-append (integer->string a) (integer->string b)))]))

(define (map1 f l)
  (if (null? l) ""
      (string-append (f (car l)) (map1 f (cdr l)))))

(define (database)     ; parses the webpages one-by-one and adds their content to the gtree
  (define (help n)
    (define (iter link)
      (let ((atlist (sxml:attr-list link)))
        (vector-set! links n (cons (make-object hyperlink%
                                     (sxml:text (car atlist))
                                     (cond [(> (length atlist) 1)
                                            (if (equal? (sxml:element-name (cadr atlist)) 'title)
                                                (sxml:text (cadr atlist))
                                                (if (> (length atlist) 2)
                                                    (sxml:text (caddr atlist)) #f))])
                                     (sxml:text link))
                                   (vector-ref links n)))))
    (define (extract-text x)
      (if (sxml:empty-element? x)
          (sxml:text x)
          (map1 (lambda (p)
                  (if (string? p) p
                      (cond [(equal? (sxml:element-name p) 'b)
                             (sxml:text p)]
                            [(equal? (sxml:element-name p) 'a)
                             (begin (iter p) (sxml:text p))]
                            [else ""]))) (sxml:content x))))
    (define (ext-txt-gen x)
      (cond [(string? x) ""]
            [(equal? (sxml:element-name x) 'p)
             (extract-text x)]
            [(sxml:empty-element? x) ""]
            [(equal? (sxml:element-name x) 'li)
             (extract-text x)]
            [else (map1 ext-txt-gen (sxml:content x))]))
    
    (let* [(x (html->xexp (open-input-file (string-append "countries/" (integer->string n)))))
           (content (ext-txt-gen x))]
      (insert-text gtree (separate content) n)))
  (for-each (lambda (n) (help n)) (build-list size (lambda (n) n))))

(define (separate str)      ; splits a string into the constituent words, ignoring commas and full stops
  (let* ((a (string-replace str "," " "))
         (b (string-replace a "." " ")))
    (string-split (string-downcase b))))

;  TREE FUNCTIONS

(define (insert-word tree word n)    ; inserts a single word into the tree with a value n in the leaf-links of the
  ; leaf of the final character
  (let* ((w (string-ref word 0))
         (stree (gnode-subtrees tree)))
    (define (insert-link tree a n)
      (let ((subtree (remove a stree)))
        (cond [(not (equal? (caar (leaf-links a)) n))
               (set-gnode-subtrees! tree (cons (begin (set-leaf-links! a (cons (cons n 1) (leaf-links a)))
                                                      a)
                                               subtree))]
              [else (let ((i (cdar (leaf-links a))))
                      (set-gnode-subtrees! tree (cons (begin (set-leaf-links! a (cons (cons n (+ i 1)) (cdr (leaf-links a))))
                                                             a)
                                                      subtree)))])))
    (cond [(= (string-length word) 1)
           (let ((present-leaf (present leaf? leaf-val w stree)))
             (if present-leaf
                 (insert-link tree present-leaf n)
                 (insert-leaf tree w n)))]
          [else (let* ((present-node (present gnode? gnode-val w stree))
                       (subtree (remove present-node stree)))
                  (if present-node (set-gnode-subtrees! tree (cons (begin
                                                                     (insert-word present-node (substring word 1) n)
                                                                     present-node)
                                                                   subtree))
                      (set-gnode-subtrees! tree (cons (single-tree word n) stree))))])))

(define (insert-text tree lis n)                  ; inserts a list of words into the tree one-by-one
  (for-each (lambda (x) (insert-word tree x n))
            lis))

(define (single-tree word n)    ; generates a tree only containing word 
  (cond [(= (string-length word) 1) (leaf (string-ref word 0) (list (cons n 1)))]
        [else (gnode (string-ref word 0) (list (single-tree (substring word 1) n)))]))

(define (insert-leaf tree w n)     ; inserts a leaf in the tree when length of w is 1
  (set-gnode-subtrees! tree (cons (leaf w (list (cons n 1))) (gnode-subtrees tree))))

; PAGERANK



;(define vec2 (make-vector (* size size) (/ damping-factor size)))

(define (matrix-maker)  ; makes a matrix with the corresponding values of probability of jumping from one page to another
  (define transition-vec (make-vector (* size size) 0))
  (begin 
    (for-each (lambda (i)
                (for-each (lambda (pair)
                            (vector-set! transition-vec (+ (* size i) (car pair)) (+ (/ damping-factor size) (* (- 1 damping-factor) (cdr pair)))))
                          (get-field in-links (vector-ref stored-files i))))
              (build-list size (lambda (x) x)))
    (vector->matrix size size transition-vec)))

;tranition matrix is the matrix to be used to calculate pagerank according to the random surfer model
;(define transition-matrix (matrix+ (matrix-scale (matrix-maker) (- 1 damping-factor))
;                                   (matrix-scale (vector->matrix size size vec2) damping-factor)))

(define (eigen-vector matr)
  (define (helper i)
    (if (= i 0) (let ((n (matrix-num-rows matr)))
                  (make-matrix n 1 (/ n)))
        (matrix* matr (helper (- i 1)))))
  (helper 10))

(define (assign-pagerank-values)
  (let ((l (matrix->list (eigen-vector (matrix-maker)))))
    (define (helper i l1)
      (cond [(< i size)
             (begin
               (set-field! pagerank (vector-ref stored-files i) (car l1))
               (helper (+ i 1) (cdr l1)))]))
    (helper 0 l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (present f g x l)       ;  hof : s.t. f: gnode?    g: gnode-val
  (let ((a (member x l (lambda (p q) (if (f q) (equal? p (g q))
                                         #f)))))
    (if a (car a) #f)))

(define (find word tree)    ; predicate which determines if the word is present in the tree or not
  (let ((s (string-ref word 0)))
    (cond [(= (string-length word) 1)
           (let ((a (present leaf? leaf-val s (gnode-subtrees tree))))
             (if a #t #f))]
          [else (let ((a (present gnode? gnode-val s (gnode-subtrees tree))))
                  (if a (find (substring word 1) a) #f))])))

;   DID YOU MEAN

(define (dym-common n f)      ; hof for the different types of mistakes in typing the word
  (lambda (word)
    (define (make-list n f word)
      (define (make-helper i)
        (let* [(str1 (substring word 0 i))
               (str2 (substring word (+ i n)))]
          (f str1 str2 (build-list 26 (lambda (i) (integer->char (+ i 97)))))))
      (append* (build-list (string-length word) (lambda (i) (make-helper i)))))
    (remove-duplicates (filter (lambda (word2) (find word2 gtree)) (make-list n f word)))))

(define dym-alteration                ; one letter typed wrong while typing
  (dym-common 1 (lambda (s1 s2 l)
                  (map (lambda (x) (string-append s1 (string x) s2)) l))))

(define dym-insertion                 ; one letter missed while typing
  (dym-common 0 (lambda (s1 s2 l)
                  (map (lambda (x) (string-append s1 (string x) s2)) l))))

(define dym-deletion                  ; one letter extra typed while typing
  (dym-common 1 (lambda (s1 s2 l) (list (string-append s1 s2)))))

(define (dym word)                    ; final list of possible correct words after applying the 3 types of dym defined above
  (if (> (length (separate word)) 1) (list word)
      (remove-duplicates (append (dym-alteration word) (dym-insertion word) (dym-deletion word)))))

(define (search query)
  (define words-to-remove '("to" "of" "a" "the" "if" "about" "over" "this" "that" "why" "what" "who" "whose" "is" "which" "from" "country" "in"))
  (define list-of-words (filter (lambda (x) (not (member x words-to-remove)))
                                (separate query)))
  (define (search-single word tree)
    (let ((s (string-ref word 0)))
      (cond [(= (string-length word) 1)
             (let ((a (present leaf? leaf-val s (gnode-subtrees tree))))
               (if a (leaf-links a) '()))]
            [else (let ((a (present gnode? gnode-val s (gnode-subtrees tree))))
                    (if a (search-single (substring word 1) a) '()))])))
  
  (let* ((l (map (lambda (t) (search-single t gtree)) list-of-words))
         (l1 (append* l)))
    (if (null? l1) (make-length-7 (list (make-object hyperlink% "" "Query not found." #f)))
        (list-of-hyperlinks (sorted-list l1)))))

(define (sort-helper l LIST)
  (if (null? l) LIST
      (let ((m (member (caar l) (cdr l) (lambda (y z) (equal? y (car z))))))
        (if (not m) (sort-helper (cdr l) (cons (car l) LIST))
            (sort-helper (cons (list (caar l) (+ (cadar l) (cadar m)) (* (caddar l) (caddar m)))
                               (remove (car m) (cdr l))) LIST)))))
(define (sorted-list l)
  (let* ((l1 (map (lambda (x) (list (car x) 1 (cdr x))) l)))
    (sort-helper l1 '())))


(define (list-of-hyperlinks l)
  (let* ((l1 (sort l (lambda (x y) (>= (cadr x) (cadr y)))))
         (l2 (sort l (lambda (x y) (>= (caddr x) (caddr y)))))
         (l3 (map (lambda (x) (vector-ref stored-files (car x))) l2)))
    (make-length-7 l3)))

(define (make-length-7 l)
  (let ((a (length l)))
    (cond [(>= a 7) (take l 7)]
          [else (make-length-7 (append l (list (make-object hyperlink% "" "" #f))))])))
(database)

(define my-canvas%
  (class canvas%
    (init-field [lst (make-length-7 '())])
    (define/override (on-event event)
      (let* ([x (send event get-x)]
             [y (send event get-y)])
        
        (define (on-event-proc i)
          (cond [(<= i 6)
                 (if (and (> x 150) (< x 400)
                    (> y (+ (* 30 i) 10)) (< y (+ (* 30 i) 35))
                    (send event button-down?))
                    (send-url (get-field url (list-ref lst i)))
                    (on-event-proc (+ i 1)))]))
        (on-event-proc 0)))
    (super-new)))
(define c-vas (new my-canvas% [parent rframe]
                   [paint-callback
                    (lambda (canvas dc)
                      (send dc set-scale 1 1)
                      (let ((a (get-field lst c-vas)))
                        (for-each (lambda (i)
                                    (send dc draw-text
                                          (get-field title (list-ref a i))
                                          150 (+ (* 30 i) 15)))
                                  (build-list 7 (lambda (i) i)))))]))
(send sframe show #t)

(send c-vas on-event (new mouse-event%
                          [event-type 'left-down]))

(for-each (lambda (i) (send (vector-ref stored-files i) send-all-links)) (build-list size (lambda (i) i)))

(assign-pagerank-values)