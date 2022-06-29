;;; KendrickSmith-hw6.scm             Kendrick Smith              25 Apr 2021

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PART 1

;;; Association lists that represents the periodic table.
(define PERIODIC-TABLE
  '((H  Hydrogen       1.0080) (He Helium          4.0026) (Li Lithium       6.9410)
    (Be Beryllium      9.0122) (B  Boron          10.8100) (C  Carbon       12.0110)
    (N  Nitrogen      14.0067) (O  Oxygen         15.9994) (F  Fluorine     18.9984)
    (Ne Neon          20.1790) (Na Sodium         22.9898) (Mg Magnesium    24.3050)
    (Al Aluminum      26.9815) (Si Silicon        28.0860) (P  Phosphorus   30.9738)
    (S  Sulfur        32.0600) (Cl Chlorine       35.4530) (Ar Argon        39.9480)
    (K  Potassium     39.1020) (Ca Calcium        40.0800) (Sc Scandium     44.9559)
    (Ti Titanium      47.9000) (V  Vanadium       50.9414) (Cr Chromium     51.9960)
    (Mn Manganese     54.9380) (Fe Iron           55.8470) (Co Cobalt       58.9332)
    (Ni Nickel        58.7100) (Cu Copper         63.5460) (Zn Zinc         65.3700)
    (Ga Gallium       69.7200) (Ge Germanium      72.5900) (As Arsenic      74.9216)
    (Se Selenium      78.9600) (Br Bromine        79.9040) (Kr Krypton      83.8000)
    (Rb Rubidium      85.4678) (Sr Strontium      87.6200) (Y  Yttrium      88.9059)
    (Zr Zirconium     91.2200) (Nb Niobium        92.9064) (Mo Molybdenum   95.9400)
    (Tc Technetium    98.9062) (Ru Ruthenium     101.0700) (Rh Rhodium     102.9055)
    (Pd Palladium    106.4000) (Ag Silver        107.8680) (Cd Cadmium     112.4000)
    (In Indium       114.8200) (Sn Tin           118.6900) (Sb Antimony    121.7500)
    (Te Tellurium    127.6000) (I  Iodine        126.9045) (Xe Xenon       131.3000)
    (Cs Caesium      132.9055) (Ba Barium        151.9600) (La Lanthanum   138.9055)
    (Ce Cerium       140.1160) (Pr Praseodymium  140.9077) (Nd Neodymium   144.2420)
    (Pm Promethium   145.0000) (Sm Samarium      150.3600) (Eu Europium    151.9640)
    (Gd Gadolinium   157.2500) (Tb Terbium       158.9254) (Dy Dysprosium  162.5000)
    (Ho Holmium      164.9303) (Er Erbium        167.2590) (Tm Thulium     168.9342)
    (Yb Ytterbium    173.0450) (Lu Lutetium      174.9669) (Hf Hafnium     178.4900)
    (Ta Tantalum     180.9479) (W Tungsten       183.8500) (Re Rhenium     186.2000)
    (Os Osmium       190.2000) (Ir Iridium       192.2200) (Pt Platinum    195.0900)
    (Au Gold         196.9665) (Hg Mercury       200.5900) (Tl Thallium    204.3700)
    (Pb Lead         207.2000) (Bi Bismuth       208.9806) (Po Polonium    210.0000)
    (At Astatine     210.0000) (Rn Radon         222.0000) (Fr Francium    223.0000)
    (Ra Radium       226.0254) (Ac Actinium      227.0000) (Th Thorium     232.0381)
    (Pa Protactinium 231.0359) (U Uranium        238.0289) (Np Neptunium   237.0482)
    (Pu Plutonium    242.0000) (Am Americium     243.0000) (Cm Curium      247.0000)
    (Bk Berkelium    249.0000) (Cf Californium   251.0000) (Es Einsteinium 252.0000)
    (Fm Fermium      257.0000) (Md Mendelevium   258.0000) (No Nobelium    259.0000)
    (Lr Lawrencium   266.0000) (Rf Rutherfordium 267.0000) (Db Dubnium     268.0000)
    (Sg Seaborgium   269.0000) (Bh Bohrium       270.0000) (Hs Hassium     270.0000)
    (Mt Meitnerium   278.0000) (Ds Darmstadtium  281.0000) (Rg Roentgenium 282.0000)
    (Cn Copernicium  285.0000) (Nh Nihonium      286.0000) (Fl Flerovium   289.0000)
    (Mc Moscovium    290.0000) (Lv Livermorium   293.0000) (Ts Tennessine  294.0000)
    (Og Oganesson    294.0000) )) 


;;;Function 1
(define (helper x y)
  (if (eq? (car (car y)) x)
      1
      (+ 1 (helper x (cdr y)))))

(define (atomic-number x)
  (if (eq? (car (car PERIODIC-TABLE)) x)
      1
      (+ 1 (helper x (cdr PERIODIC-TABLE)))))


;;;Function 2
(define (atomic-weight x)
  (car(cddr (assoc x PERIODIC-TABLE))))

;;Function 3
(define (molecular-weight1 x)
  (if (null? x)
      0
      (+ (atomic-weight (car x)) (molecular-weight1 (cdr x)))))

;;;Function 4
(define (molecular-weight2 x)
  (if (null? x)
      0
      (if (equal? (length x) 1)
          (if (number? x)
              0
              (atomic-weight (car x)))
          (if (number? (cadr x))
              (+ (molecular-weight2 (cddr x)) (* (atomic-weight (car x)) (cadr x)))
              (+ (molecular-weight2 (cdr x)) (atomic-weight (car x)))))))

;;;Function 5
(define (molecular-weight3 x)
  (if (null? x)
      'null
      (if (equal? (length x) 1)
          (if (list? (car x))
              (molecular-weight3 (car x))
              (if (number? (car x))
                  0
                  (atomic-weight (car x))))
          (if (list? (car x))
              (if (number? (cadr x))
                  (+ (* (molecular-weight3 (car x)) (cadr x)) (molecular-weight3 (cdr x)))
                  (+ (molecular-weight3 (car x)) (molecular-weight3 (cdr x))))
              (if (number? (car x))
                  (molecular-weight3 (cdr x))
                  (if (number? (cadr x))
                      (+ (* (atomic-weight (car x)) (cadr x)) (molecular-weight3 (cdr x)))
                      (+ (atomic-weight (car x)) (molecular-weight3 (cdr x)))))))))
              




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PART 2

(define ANIMALS 
  '(dog
     (bird (horse () ()) (cat () ()))
     (possum (dog () ()) ())))

;;; Determines if the specified binary tree is empty
(define (empty-tree? tree)
  (null? tree))

;;; Returns the value stored at the root of the binary tree.
(define (root tree)
  (if (empty-tree? tree)
      'ERROR
      (car tree)))

;;; Returns the left subtree of the specified binary tree.
(define (left-subtree tree)
  (if (empty-tree? tree)
      'ERROR
      (cadr tree)))

;;; Returns the right subtree of the specified binary tree.
(define (right-subtree tree)
  (if (empty-tree? tree)
      'ERROR
      (caddr tree)))


;;;Function 6
(define (height x)
  (if (empty-tree? x)
      0
      (+ 1 (max (height (left-subtree x)) (height (right-subtree x))))))

;;;Function 7
(define (contains? x y)
  (cond ((empty-tree? x) #f)
        ((equal? (root x) y) #t)
        ((contains? (right-subtree x) y) #t)
        ((contains? (left-subtree x) y) #t)
        (else #f)))

;;;Function 8 (only works on sorted binary trees)
(define (has-dupes-sorted? x)
  (if (null? x)
      #f
      (if (or (contains? (left-subtree x) (root x)) (contains? (right-subtree x) (root x)))
          #t
          (or (has-dupes? (left-subtree x)) (has-dupes? (right-subtree x))))))

;;;Helpers for unsorted
(define (tree->list x)
  (if (null? x)
      '()
      (cons (cons (root x) (tree->list (left-subtree x))) (tree->list (right-subtree x)))))

(define (list-has-dupes? x)
  (cond ((null? x) #f)
        ((equal? (length x) 1) #f)
        (else (if (list-contains? (cdr x) (car x))
                  #t
                  (list-has-dupes? (cdr x))))))
  

(define (list-contains? x y)
  (cond ((null? x) #f)
        ((equal? (car x) y) #t)
        (else (list-contains? (cdr x) y)))) 


;;;For unsorted binary trees
(define (has-dupes? x)
  (if (has-dupes-sorted? x)
      #t
      (list-has-dupes? (tree->list x))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PART 3

;;; Models a Coin object with two sides: HEADS and TAILS
;;;Updated for Function 10
  (define (Coin head-percent)
    (define num-flips 0)
    (define (apply op)
      (cond ((equal? op 'flip) (or (set! num-flips (+ 1 num-flips))) (if (< (random 100) head-percent) 'HEADS 'TAILS))
            ((equal? op 'percent) head-percent)
            ((equal? op 'num-flips) num-flips)
            (else 'UNDEFINED)))
  apply)

;;;Function 9
(define (check x y)
  (letrec ((loop (lambda (times numHeads)
                   (if (= times 0)
                       (exact->inexact (* (/ numHeads y) 100))
                       (if (equal? (x 'flip) 'HEADS)
                           (begin (loop (- times 1) (+ numHeads 1)))
                           (begin (loop (- times 1) numHeads)))))))
    (loop y 0)))
