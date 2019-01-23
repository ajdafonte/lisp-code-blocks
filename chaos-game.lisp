;;; .-------------------------------------------.
;;; |		  Chaos: Wizard                 |
;;; '-------------------------------------------'

;;Wizard
(defstruct wizard
  life
  movement
  attack
  defense
  creatures
  spells
  id)

;;; .-------------------------------------------.
;;; |		  Chaos: Spells                 |
;;; '-------------------------------------------'

;;ligthning-cold
(defstruct lightning-cold
  (range 4)
  (damage 2)
  (difficulty 30))

;;fire-ball
(defstruct fire-ball
  (range 4)
  (damage 3)
  (difficulty 40))

;;magic-missile
(defstruct magic-missile
  (range 6)
  (damage 1)
  (difficulty 30))

;;sonic-attack
(defstruct sonic-attack
  (range 4)
  (damage 3)
  (difficulty 50))

;;heat-shock
(defstruct heat-shock
  (range 5)
  (damage  3)
  (difficulty  60))

;;psionic-blast
(defstruct psionic-blast
  (range  8)
  (damage  2)
  (difficult  70))

;;death-ray
(defstruct death-ray
  (range  4)
  (damage  6)
  (difficulty  90))

;;; .-------------------------------------------.
;;; |		Chaos: Creatures		|
;;; '-------------------------------------------'
;;; ------ Creature: Bat ------
(defstruct bat
  ;;; Life
  (life 2)
  ;;; Movement
  (movement 3)
  ;;; Attack
  (attack 1)
  ;;; Defense
  (defense 1)
  ;;; Difficulty
  (difficulty 20)
  id)

;;; ------ Creature: Eagle ------
(defstruct eagle
  ;;; Life
  (life 4)
  ;;; Movement
  (movement 3)
  ;;; Attack
  (attack 1)
  ;;; Defense
  (defense 2)
  ;;; Difficulty
  (difficulty 30)
  id)

;;; ------ Creature: Viper ------
(defstruct viper
  ;;; Life
  (life 2)
  ;;; Movement
  (movement 2)
  ;;; Attack
  (attack 2)
  ;;; Defense
  (defense 1)
  ;;; Difficulty
  (difficulty 30)
  id)

;;; ------ Creature: Crocodile ------
(defstruct crocodile
  ;;; Life
  (life 4)
  ;;; Movement
  (movement 1)
  ;;; Attack
  (attack 3)
  ;;; Defense
  (defense 2)
  ;;; Difficulty
  (difficulty 40)
  id)

;;; ------ Creature: Bear ------
(defstruct bear
  ;;; Life
  (life 4)
  ;;; Movement
  (movement 2)
  ;;; Attack
  (attack 2)
  ;;; Defense
  (defense 2)
  ;;; Difficulty
  (difficulty 40)
  id)

;;; ------ Creature: Centaur ------
(defstruct centaur
  ;;; Life
  (life 5)
  ;;; Movement
  (movement 2)
  ;;; Attack
  (attack 3)
  ;;; Defense
  (defense 2)
  ;;; Difficulty
  (difficulty 50)
  id)

;;; ------ Creature: Vampire ------
(defstruct vampire
  ;;; Life
  (life 5)
  ;;; Movement
  (movement 2)
  ;;; Attack
  (attack 4)
  ;;; Defense
  (defense 3)
  ;;; Difficulty
  (difficulty 70)
  id)

;;; ------ Creature: Dragon ------
(defstruct dragon
  ;;; Life
  (life 12)
  ;;; Movement
  (movement 2)
  ;;; Attack
  (attack 5)
  ;;; Defense
  (defense 5)
  ;;; Difficulty
  (difficulty 80)
  id)

;;; .-------------------------------------------.
;;; |		      Chaos: Map                |
;;; '-------------------------------------------'

;;; ------ Map: Struct ------
(defstruct gameMap
  (tab nil)
  (px 8)
  (py 6))


;;; .-------------------------------------------.
;;; |		 Nivel 1: Functions             |
;;; '-------------------------------------------'

;; -------- Wizard ---------

;; novo-mago (vida x movimento x ataque x defesa x nfeiticos -> mago)
(defun new-wizard (life movement attack defense numSpells)
  ;; create wizard spell list
  (let ((spellList (choose-spells numSpells)))
    ;; create wizard
    (make-wizard :life life :movement movement :attack attack :defense defense :spells spellList :id (gensym "W"))))


;; ---------- Spells ----------


;; escolhe-feitiço (int -> lista)
(defun choose-spells (num)
  (let ((spellslist nil)
        (rand nil)
        (tmp nil))
    (dotimes (i num spellslist)
      (setf rand (random 15))
      ;; (format T ">> Random ~a~%" rand)
      (setf tmp (create-spell rand))
      (setf spellslist (cons tmp spellslist))))) 

;; cria feitico (criatura ou ataque) 
(defun create-spell (spell)   
  (cond  ((eq spell 0) (make-lightning-cold))
         ((eq spell 1) (make-fire-ball))
         ((eq spell 2) (make-magic-missile))
         ((eq spell 3) (make-sonic-attack))
         ((eq spell 4) (make-heat-shock))
         ((eq spell 5) (make-psionic-blast))
         ((eq spell 6) (make-death-ray))
	 ((eq spell 7) (make-bat))
         ((eq spell 8) (make-eagle))
         ((eq spell 9) (make-viper))
         ((eq spell 10) (make-crocodile))
         ((eq spell 11) (make-bear))
         ((eq spell 12) (make-centaur))
         ((eq spell 13) (make-vampire))
         ((eq spell 14) (make-dragon))))

;; ---------- Creature ----------

;;  cria-criatura (tipo -> criatura)
(defun create-creature (type)
  (cond  ((eq type 'bat) (make-bat))
         ((eq type 'eagle) (make-eagle))
         ((eq type 'viper) (make-viper))
         ((eq type 'crocodile) (make-crocodile))
         ((eq type 'bear) (make-bear))
         ((eq type 'centaur) (make-centaur))
         ((eq type 'vampire) (make-vampire))
         ((eq type 'dragon) (make-dragon))))

;; verifica se o feitiço é uma criatura ou um ataque
;; T para criaturas, Nil para feitiços
(defun spell-type (type)
  (cond  ((eq type 'bat) T)
         ((eq type 'eagle) T)
         ((eq type 'viper) T)
         ((eq type 'crocodile) T)
         ((eq type 'bear) T)
         ((eq type 'centaur) T)
         ((eq type 'vampire) T)
         ((eq type 'dragon) T)))

;; criaturas-mago (mago -> lista)
;; devolve as criaturas que foram invocadas pelo mago (slot creatures)
(defun wizard-creatures-list (wiz)
  (wizard-creatures wiz))

;; novo-mapa (largura x altura -> mapa)
(defun new-map (width height)
  (make-gameMap :tab (make-array (list width height) ) :px width :py height))

;;escreve (stream x map -> NIL)
(defun write-map (stream gMap)
  (format stream "~a" (view-map (gameMap-tab gMap))))

(defun view-map (tab)
  (dotimes (j (array-dimension tab 0) "'")
    (or (when (= j 0) 
	  (dotimes (i (array-dimension tab 1))
	     (or (when (= i 0)
		   (concatenate 'string (format T ".-----+")))
		 (when (and (> i 0)(< i (1- (array-dimension tab 1))))
			(concatenate 'string (format T "-----+")))
		 (when (= i (1- (array-dimension tab 1)))
		   (concatenate 'string (format T "-----.")))))
	   (dotimes (i (array-dimension tab 1))
	     (or (when (= i 0)
		   (concatenate 'string (format T "~%| ~A |" (aref tab j i))))
		 (when (and (> i 0)(< i (1- (array-dimension tab 1))))
		   (concatenate 'string (format T " ~A |" (aref tab j i))))
		 (when (= i (1- (array-dimension tab 1)))
		   (concatenate 'string (format T " ~A |~%" (aref tab j i))))))))
	(when (and (> j 0) (< j (1- (array-dimension tab 0))))
	  (dotimes (i (array-dimension tab 1))
	     (or (when (= i 0)
		   (concatenate 'string (format T "|-----+")))
		 (when (and (> i 0)(< i (1- (array-dimension tab 1))))
		   (concatenate 'string (format T "-----+")))
		 (when (= i (1- (array-dimension tab 1)))(concatenate 'string (format T "-----|")))))
	   (dotimes (i (array-dimension tab 1))
	     (or (when (= i 0)
		   (concatenate 'string (format T "~%| ~A |" (aref tab j i))))
		 (when (and (> i 0)(< i (1- (array-dimension tab 1))))
		   (concatenate 'string (format T " ~A |" (aref tab j i))))
		 (when (= i (1- (array-dimension tab 1)))
		   (concatenate 'string (format T " ~A |~%" (aref tab j i)))))))
	(when (= j (1- (array-dimension tab 0)))
	  (dotimes (i (array-dimension tab 1))
	     (or (when (= i 0)
		   (concatenate 'string (format T ".-----+")))
		 (when (and (> i 0)(< i (1- (array-dimension tab 1))))
		   (concatenate 'string (format T "-----+")))
		 (when (= i (1- (array-dimension tab 1)))
		   (concatenate 'string (format T "-----.")))))
	   (dotimes (i (array-dimension tab 1))
	     (or (when (= i 0)
		   (concatenate 'string (format T "~%| ~A |" (aref tab j i))))
		 (when (and (> i 0)(< i (1- (array-dimension tab 1))))
		   (concatenate 'string (format T " ~A |" (aref tab j i))))
		 (when (= i (1- (array-dimension tab 1)))
		   (concatenate 'string (format T " ~A |~%" (aref tab j i))))))
	   (dotimes (i (array-dimension tab 1))
	     (or (when (= i 0)
		   (concatenate 'string (format T "'-----+")))
		 (when (and (> i 0)(< i (1- (array-dimension tab 1))))
		   (concatenate 'string (format T "-----+")))
		 (when (= i (1- (array-dimension tab 1)))
		   (concatenate 'string (format T "-----"))))))))

;; mapa-pos (coluna x linha x mapa -> simbolo)
(defun map-pos (col row gMap)
  (aref (gameMap-tab gMap) row col))

;; mapa-remove (coluna x linha x mapa -> simbolo)
(defun map-remove (col row gMap)
  (let ((tab (gameMap-tab gMap)))
	   (or (when (eql nil tab) nil)
	       (when (and (<= row (1- (array-dimension tab 0)))
			  (<= col (1- (array-dimension tab 1)))
			  (not (eql (aref tab row col) nil)))
		 ;; nil é o simbolo que representa posição vazia
                 (setf (aref tab row col) nil)
                 (setf (gameMap-tab gMap) tab)  
		 (eq (aref tab row col) nil)))))

;; mapa-insere (coluna x linha x mapa -> mapa)
(defun map-insert (col row gMap elem)
  (let ((tab (gameMap-tab gMap)))
    (or (when (eql nil tab) nil)
        (when (and (<= row (1- (array-dimension tab 0)))
                   (<= col (1- (array-dimension tab 1)))
                   (eql (aref tab row col) nil))
          ;; nil é o simbolo que representa posição vazia
          (setf (aref tab row col) elem)
          (setf (gameMap-tab gMap) tab)))
    gMap))

;;testa-nivel1

;; Função que insere um wizard no mapa
(defun insert-wizard-on-map (wizard gMap)
  (let ((wz1Px nil)
        (wz1Py nil))
    (loop
      (setq wz1Px (random (array-dimension (gameMap-tab gMap) 0)))
      (setq wz1Py (random (array-dimension (gameMap-tab gMap) 1)))
      (format T "PxRand: ~a~%" wz1Px)
      (format T "PyRand: ~a~%" wz1Py)
      (when (eq (map-pos wz1Py wz1Px gMap) nil)
        (map-insert wz1Py wz1Px gMap (wizard-id wizard))
        (return-from insert-wizard-on-map)))))

;; Função que procura um wizard no mapa
 (defun search-wizard (wiz gMap)
  (let ((x 0)
        (y 0)
        (posWiz nil)
        (tab (gameMap-tab gMap)))
    (dotimes (i (array-dimension tab 0) posWiz)
      (dotimes (j (array-dimension tab 1))
        (let ((elem (aref tab i j)))
          (when (eql elem (wizard-id wiz))
            (setf x i y j)
            (setf posWiz (list x y))))))))

;; invoca uma criatura
(defun creature-registry (wizard spell-pos gMap)
	   (let*((creature (nth  spell-pos (wizard-spells wizard))) 
		(cNAme (type-of creature))
		(cID nil)
		(position (search-wizard wizard gMap))
		(wzdpx (first position))
		(wzdpy (second position)))
	 (cond  ((eq cName 'bat)
		 (setf cID (gensym "B"))
		 (wizard-invoke-creature wizard creature cID)	 
		 (creature-map-insert gMap cID wzdpx wzdpy))
		((eq cName 'eagle)
		 (setf cID (gensym "E"))
		 (wizard-invoke-creature wizard creature cID)
		 (creature-map-insert gMap cID wzdpx wzdpy))
		((eq cName 'viper)
		 (setf cID (gensym "V"))
		 (wizard-invoke-creature wizard creature cID)
		 (creature-map-insert gMap cID wzdpx wzdpy))
		((eq cName 'crocodile)
		 (setf cID (gensym "C"))
		 (wizard-invoke-creature wizard creature cID)
		 (creature-map-insert gMap cID wzdpx wzdpy))
		((eq cName 'bear)
		 (setf cID (gensym "P"))
		 (wizard-invoke-creature wizard creature cID)
		 (creature-map-insert gMap cID wzdpx wzdpy))
		((eq cName 'centaur)
		 (setf cID (gensym "T"))
		 (wizard-invoke-creature wizard creature cID)
		 (creature-map-insert gMap cID wzdpx wzdpy))
		((eq cName 'vampire)
		 (setf cID (gensym "A"))
		 (wizard-invoke-creature wizard creature cID)
		 (creature-map-insert gMap cID wzdpx wzdpy))
		((eq cName 'dragon)
		 (setf cID (gensym "D"))
		 (wizard-invoke-creature wizard creature cID)
		 (creature-map-insert gMap cID wzdpx wzdpy)))))

;; insere criatura no mapa
(defun creature-map-insert (gMap cID wzdpx wzdpy)
  (let ((x_min nil)
	(x_max nil)
	(y_min nil)
	(y_max nil)
	(px nil)
	(py nil))
    (if (> wzdpx 0)
	(setf x_min (1- wzdpx))
	(setf x_min 0))
    (if (< wzdpx (1- (array-dimension (gameMap-tab gMap) 0)))
	(setf x_max (+ 1 wzdpx))
	(setf x_max (1- (array-dimension (gameMap-tab gMap) 0))))
    (if (> wzdpy 0)
	(setf y_min (1- wzdpy))
	(setf y_min 0))
    (if (< wzdpy (1- (array-dimension (gameMap-tab gMap) 1)))
	(setf y_max (+ 1 wzdpy))
	(setf y_max (1- (array-dimension (gameMap-tab gMap) 1))))
    (format T "x_min: ~a~%" x_min)
    (format T "x_max: ~a~%" x_max)
    (format T "y_min: ~a~%" y_min)
    (format T "y_max: ~a~%" y_max)
    (loop
      ;;(setq px (+ x_min (random (1+(- x_max x_min)))))
      ;;(setq py (+ x_min (random (1+(- y_max y_min)))))
      (setq px (random (1+ x_max)))
      (setq py (random (1+ y_max)))
      (format T "Px: ~a~%" px)
      (format T "Py: ~a~%" py)
      (when (and (eql (map-pos py px gMap) nil)
		 (>= px x_min)(<= px x_max)
		 (>= py y_min)(<= py y_max)
		 (not (and (= px wzdpx)(= py wzdpy))))
        (map-insert py px gMap cID)
        (return-from creature-map-insert)))))

;; wizard-spells -> wizard-creatures 
(defun wizard-invoke-creature (wizard creature cID)
  (let((cNAme (type-of creature))
       (cClone nil))
    (cond  ((eq cName 'bat)
	    ;; cria uma cópia do feitiço
	    (setf cClone (copy-bat creature))
	    ;; insere o ID no clone
	    (setf (bat-id cClone) cID)
	    ;; insere o feitiço nas criaturas
	    (setf (wizard-creatures wizard)
		  (cons  cClone (wizard-creatures wizard)))
	    ;; remove da lista de feitiços
	    (setf (wizard-spells wizard)(delete creature (wizard-spells wizard) :count 1)))	   
	   ((eq cName 'eagle)
	    ;; cria uma cópia do feitiço
	    (setf cClone (copy-eagle creature))
	    ;; insere o ID no clone
	    (setf (eagle-id cClone) cID)
	    ;; insere o feitiço nas criaturas
	    (setf (wizard-creatures wizard)
		  (cons  cClone (wizard-creatures wizard)))
	    ;; remove da lista de feitiços
	    (setf (wizard-spells wizard)(delete creature (wizard-spells wizard) :count 1)))
	   ((eq cName 'viper)
	    ;; cria uma cópia do feitiço
	    (setf cClone (copy-viper creature))
	    ;; insere o ID no clone
	    (setf (viper-id cClone) cID)
	    ;; insere o feitiço nas criaturas
	    (setf (wizard-creatures wizard)
		  (cons  cClone (wizard-creatures wizard)))
	    ;; remove da lista de feitiços
	    (setf (wizard-spells wizard)(delete creature (wizard-spells wizard) :count 1)))	   
	   ((eq cName 'crocodile)
	    ;; cria uma cópia do feitiço
	    (setf cClone (copy-crocodile creature))
	    ;; insere o ID no clone
	    (setf (crocodile-id cClone) cID)
	    ;; insere o feitiço nas criaturas
	    (setf (wizard-creatures wizard)
		  (cons  cClone (wizard-creatures wizard)))
	    ;; remove da lista de feitiços
	    (setf (wizard-spells wizard)(delete creature (wizard-spells wizard) :count 1)))
	   ((eq cName 'bear)
	    ;; cria uma cópia do feitiço
	    (setf cClone (copy-bear creature))
	    ;; insere o ID no clone
	    (setf (bear-id cClone) cID)
	    ;; insere o feitiço nas criaturas
	    (setf (wizard-creatures wizard)
		  (cons  cClone (wizard-creatures wizard)))
	    ;; remove da lista de feitiços
	    (setf (wizard-spells wizard)(delete creature (wizard-spells wizard) :count 1)))
	   ((eq cName 'centaur)
	    ;; cria uma cópia do feitiço
	    (setf cClone (copy-centaur creature))
	    ;; insere o ID no clone
	    (setf (centaur-id cClone) cID)
	    ;; insere o feitiço nas criaturas
	    (setf (wizard-creatures wizard)
		  (cons  cClone (wizard-creatures wizard)))
	    ;; remove da lista de feitiços
	    (setf (wizard-spells wizard)(delete creature (wizard-spells wizard) :count 1)))	   
	   ((eq cName 'vampire)
	    ;; cria uma cópia do feitiço
	    (setf cClone (copy-vampire creature))
	    ;; insere o ID no clone
	    (setf (vampire-id cClone) cID)
	    ;; insere o feitiço nas criaturas
	    (setf (wizard-creatures wizard)
		  (cons  cClone (wizard-creatures wizard)))
	    ;; remove da lista de feitiços
	    (setf (wizard-spells wizard)(delete creature (wizard-spells wizard) :count 1)))
	   ((eq cName 'dragon)
	    ;; cria uma cópia do feitiço
	    (setf cClone (copy-dragon creature))
	    ;; insere o ID no clone
	    (setf (dragon-id cClone) cID)
	    ;; insere o feitiço nas criaturas
	    (setf (wizard-creatures wizard)
		  (cons  cClone (wizard-creatures wizard)))
	    ;; remove da lista de feitiços
	    (setf (wizard-spells wizard)(delete creature (wizard-spells wizard) :count 1))))))

;; Testing
;; (setf wizard1 (new-wizard 5 2 2 2 8))
;; (setf gmap (new-map 8 6))
;; (creature-registry wizard1 0 gmap)
;; (write-map T gmap)


;;Testa nivel 1
(defun testa-nivel-1()
  (let((wizard1 (new-wizard 5 2 2 2 8))
       (wizard2 (new-wizard 5 2 2 2 8))
       (gmap (new-map 8 6)))
    (insert-wizard-on-map wizard1 gmap)
    (insert-wizard-on-map wizard2 gmap)
    
    (dotimes (i 7)
      (creature-registry wizard1 i gmap))
    (dotimes (i 7)
      (creature-registry wizard2 i gmap))
    (write-map t gmap)
    ))

;;; .-------------------------------------------.
;;; |		 Nivel 2: Functions             |
;;; '-------------------------------------------'

;; ataque (ataque x defesa -> danos)
(defun attack (att def)
  (let ((rndVal (random 100))
        (attackDificulty (+ 50 (* 10 (- def att)))))
    ;; Validate if was possible to do the attack
    (if (> rndVal attackDificulty)
        ;; attack = damage points
        ;; needs to update the struts values ??
        att        
        NIL)))

;; dada uma lista de feiticos e um nome de feitico (criatura ou ataque) devolvendo o feitico em causa
(defun get-spell (creatureSpell spellList)
  (let ((type nil))
    (dolist (sp spellList)
      (setq type (type-of sp))
      (when (eql type creatureSpell)
	(return sp)))))

;; Testing
(defparameter wiz1 (new-wizard 50 9 8 7 3))
(get-spell 'vampire (wizard-spells wiz1))


;;calcular a dificuldade de um determinado feitico
(defun get-spell-difficulty (spell)
  (cond ((eql (type-of spell) 'bat) (bat-difficulty spell))
	((eql (type-of spell) 'eagle) (eagle-difficulty spell))
	((eql (type-of spell) 'viper) (viper-difficulty spell))
	((eql (type-of spell) 'crocodile) (crocodile-difficulty spell))
	((eql (type-of spell) 'bear) (bear-difficulty spell))
	((eql (type-of spell) 'centaur) (centaur-difficulty spell))
	((eql (type-of spell) 'vampire) (vampire-difficulty spell))
	((eql (type-of spell) 'dragon) (dragon-difficulty spell))
	((eql (type-of spell) 'lightning-cold) (lightning-cold-difficulty spell))
	((eql (type-of spell) 'fire-ball) (fire-ball-difficulty spell))
	((eql (type-of spell) 'magic-missile) (magic-missile-difficulty spell))
	((eql (type-of spell) 'sonic-attack) (sonic-attack-difficulty spell))
	((eql (type-of spell) 'heat-shock) (heat-shock-difficulty spell))
	((eql (type-of spell) 'psionic-blast) (psionic-blast-difficulty spell))
	((eql (type-of spell) 'death-ray) (death-ray-difficulty spell))))

;;Testing
(defparameter creature1 (get-spell 'vampire (wizard-spells wiz1)))
(get-spell-difficulty creature1)

;; invoca uma criatura
(defun creature-registry-2 (wizard creat gameMap)
	   (let* ((cNAme (type-of creat))
		  (cID nil)
		  (position (search-wizard wizard gMap))
		  (wzdpx (first position))
		  (wzdpy (second position)))
	     (cond  ((eq cName 'bat)
		     (setf cID (gensym "B"))
		     (wizard-invoke-creature wizard creat cID)	 
		     (creature-map-insert gMap cID wzdpx wzdpy))
		    ((eq cName 'eagle)
		     (setf cID (gensym "E"))
		     (wizard-invoke-creature wizard creat cID)
		     (creature-map-insert gMap cID wzdpx wzdpy))
		    ((eq cName 'viper)
		     (setf cID (gensym "V"))
		     (wizard-invoke-creature wizard creat cID)
		     (creature-map-insert gMap cID wzdpx wzdpy))
		    ((eq cName 'crocodile)
		     (setf cID (gensym "C"))
		     (wizard-invoke-creature wizard creat cID)
		     (creature-map-insert gMap cID wzdpx wzdpy))
		    ((eq cName 'bear)
		     (setf cID (gensym "P"))
		     (wizard-invoke-creature wizard creat cID)
		     (creature-map-insert gMap cID wzdpx wzdpy))
		    ((eq cName 'centaur)
		     (setf cID (gensym "T"))
		     (wizard-invoke-creature wizard creat cID)
		     (creature-map-insert gMap cID wzdpx wzdpy))
		    ((eq cName 'vampire)
		     (setf cID (gensym "A"))
		     (wizard-invoke-creature wizard creat cID)
		     (creature-map-insert gMap cID wzdpx wzdpy))
		    ((eq cName 'dragon)
		     (setf cID (gensym "D"))
		     (wizard-invoke-creature wizard creat cID)
		     (creature-map-insert gMap cID wzdpx wzdpy)))))

;;Testing
(defparameter spell1 (get-spell 'lightning-cold (wizard-spells wiz1)))
(get-spell-difficulty spell1)
(defparameter gmap (new-map 4 4))



;; lanca-feitico (mago x feitico x mapa -> mapa)
(defun launch-spell (wiz spll gameMap)
  (let ((rndVal (random 100))
	(creat nil)
	(spell nil)
	(spellDifficulty nil))
    ;; validate type of spell (creature or attack) and if exists in wizards list
    (if (spell-type spll)
	;;find + get creture
	(setq creat (get-spell spll (wizard-spells wiz)))
      ;;find + get spell
      (setq spell (get-spell spll (wizard-spells wiz))))
    
    ;; 2 - remove spell in wizard spell list (precisava que as criatura estivessem generalizadas numa estrutura so, em vez de ter so criaturas com varias estruturas)
      (when creat
       (setq spellDifficulty (get-spell-difficulty creat))
       (format T ">>The spell difficulty..:~a~%" spellDifficulty)
       (format T ">>The randomVal..:~a~%" rndVal)
    ;; 3 - validate if spell is to be launched
       (if (> rndVal spellDifficulty)
    	    (creature-registry-2 wiz creat gameMap)
    	    (format T "~a~%" ">> The creation of the creature failed..")))))
            ;; aqui tb preciso de remover o spell da lista na mm - TODO
    
    ;; 2 - remove spell in wizard spell list (precisava que as criatura estivessem generalizadas numa estrutura so, em vez de ter so criaturas com varias estruturas)
  ;;  (cond (creat (setq spellDifficulty (get-spell-difficulty creat))
		 ;; 3 - validate if spell is to be launched
;;		 (if (> rndVal spellDifficulty)
;;		     (creature-registry-2 wiz creat gameMap)
;;		   (format T "~a~%" ">> The creation of the creature failed..")))
;;	  (spell (setq spellDifficulty (get-spell-difficulty creat))
		 ;; 3 - validate if spell is to be launched
;;		 (if (> rndVal spellDifficulty)
;;		     T ;;attack spell - Perguntar ao user as coordenadas
;; Dadas as coordenadas valida se estao dentro do alcance necessario
;; Lanca feitico
;; Caso coordenadas sejam um outro jogador - actualiza struturas...


;;        ;; creature
;;      ;; 1 - validate if spell exists in wizards list - TODO
    ;;    (member spll wizard-spells)
        ;; 2 - remove spell in wizard spell list - TODO
        ;; 3 - validate if spell is to be launched
      ;;  (if (> rndVal <creature-dificulty>)
            ;; 3.1 - Create creature (!! next to wizard) - TODO
        ;;    (create-creature 'spll)
          ;;  (format T "~a~%" ">> The creation of the creature failed..")
        ;;  )
     ;; )
    ;; ))
