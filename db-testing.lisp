(defvar *mydb* nil)

(defun setup-table (db)
  (ignore-errors (dbi:sql "drop table dummy_tbl" :db db))
  (dbi:sql "create table dummy_tbl (k1 number(5), k2 number, data varchar2(100))" :db db)
  (dbi:sql "create unique index dummy_tbl_idx01 on dummy_tbl (k1)" :db db)
  (dbi:sql "create unique index dummy_tbl_idx02 on dummy_tbl (k2)" :db db))


(defun regular-operation (db records operation field &key (rollback? t))
  (unwind-protect
      (let* ((data "hello")
	     (stmt (case operation
			 ((:insert) (case field
					  ((1) "insert into dummy_tbl (data, k1) values ('~a', ~a)")
					  ((2) "insert into dummy_tbl (data, k2) values ('~a', ~a)")))
			 ((:update) (case field
					  ((1) "update dummy_tbl set data = '~a' where k1 = ~a")
					  ((2) "update dummy_tbl set data = '~a' where k2 = ~a"))))))
	(dotimes (i records)
	  (dbi:sql (format nil stmt data i) :db db)))
    (when rollback?
      (dbi:sql "rollback" :db db))))

(defun parameter-operation (db records operation method field &key (rollback? t) (force.index? nil))
  (unwind-protect
      (let* ((data "hello")
	     (bparameter (case method
			       ((:int) (dbi:bind-parameter 1 nil :int nil :db db))
			       ((:long) (dbi:bind-parameter 1 nil :long nil :db db))
			       ((:float) (dbi:bind-parameter 1 nil :float nil :db db))
			       ((:double) (dbi:bind-parameter 1 nil :double nil :db db))))
	     (stmt (case operation
			 ((:insert) (case field
					  ((1) (format nil "insert into dummy_tbl (data, k1) values ('~a', ?)" data))
					  ((2) (format nil "insert into dummy_tbl (data, k2) values ('~a', ?)" data))))
			 ((:update) (case field
					  ((1) (format nil "update ~a dummy_tbl set data = '~a' where k1 = ?" 
						       (if force.index? "/*+ INDEX(dummy_tbl, dummy_tbl_idx01) */" "") data))
					  ((2) (format nil "update ~a dummy_tbl set data = '~a' where k2 = ?" 
						       (if force.index? "/*+ INDEX(dummy_tbl, dummy_tbl_idx02) */" "") data)))))))
	(dbi:prepare-sql stmt :db db)
	(dotimes (i records)
	  (setf (ff:fslot-value bparameter :data) (case method
							((:int :long) i)
							((:float :double) (float i 1.0d0))))
	  (dbi:run-prepared-sql :db db)))
    (when rollback?
      (dbi:sql "rollback" :db db))))




(setf *mydb* (dbi:connect :data-source-name "db0511gora3" :user "bla009database52078" :password "bla009database52078"))
(dbi:set-autocommit nil :db *mydb*)
(setup-table *mydb*)

;; records are not important

(time (regular-operation *mydb* 10000 :insert 1))
;; RA
; cpu time (total)  2.890625 sec user, 0.328125 sec system
; real time  10.624000 sec ( 30.3%)
;; AC
; cpu time (total)  0.015600 sec user, 0.000000 sec system
; real time  13.354000 sec (.1168%)

(time (regular-operation *mydb* 10000 :insert 2))
;; RA
; cpu time (total)  2.640625 sec user, 0.625000 sec system
; real time  9.014000 sec (36.23%)
;; AC
; cpu time (total)  0.405602 sec user, 0.842406 sec system
; real time  14.161000 sec (8.813%)


(time (parameter-operation *mydb* 10000 :insert :int 1))
;; RA
; cpu time (total)  0.875000 sec user, 0.156250 sec system
; real time  2.828000 sec (36.47%)
;; AC
; cpu time (total)  0.015600 sec user, 0.000000 sec system
; real time  10.283000 sec (.1517%)


(time (parameter-operation *mydb* 10000 :insert :int 2))
;; RA
; cpu time (total)  0.656250 sec user, 0.171875 sec system
; real time  1.781000 sec ( 46.5%)
; cpu time (total)  3.203125 sec user, 0.546875 sec system
; real time  16.123000 sec (23.26%)
;; AC
; cpu time (total)  0.046801 sec user, 0.000000 sec system
; real time  10.320000 sec (.4535%)


(time (parameter-operation *mydb* 10000 :insert :long 1))
;; RA
; cpu time (total)  0.859375 sec user, 0.187500 sec system
; real time  3.531000 sec (29.65%)
;; AC
; cpu time (total)  0.062400 sec user, 0.078000 sec system
; real time  10.340000 sec (1.358%)

(time (parameter-operation *mydb* 10000 :insert :long 2))
;; RA
; cpu time (total)  0.718750 sec user, 0.187500 sec system
; real time  4.078000 sec (22.22%)
;; AC
; cpu time (total)  0.062401 sec user, 0.015600 sec system
; real time  10.229000 sec (.7625%)



(time (parameter-operation *mydb* 10000 :insert :float 1))
;; RA
; cpu time (total)  0.375000 sec user, 0.296875 sec system
; real time  1.813000 sec (37.06%)
;; AC
; cpu time (total)  0.062401 sec user, 0.015601 sec system
; real time  10.255000 sec (.7606%)


(time (parameter-operation *mydb* 10000 :insert :float 2))
;; RA
; cpu time (total)  0.468750 sec user, 0.125000 sec system
; real time  1.500000 sec (39.58%)
;; AC
; cpu time (total)  0.062401 sec user, 0.124801 sec system
; real time  10.321000 sec (1.814%)



(time (parameter-operation *mydb* 10000 :insert :double 1))
;; RA
; cpu time (total)  0.328125 sec user, 0.296875 sec system
; real time  4.782000 sec (13.07%)
;; AC
; cpu time (total)  0.046801 sec user, 0.000000 sec system
; real time  10.633000 sec (.4401%)


(time (parameter-operation *mydb* 10000 :insert :double 2))
;; RA
; cpu time (total)  0.359375 sec user, 0.250000 sec system
; real time  1.500000 sec (40.62%)
;; AC
; cpu time (total)  0.031200 sec user, 0.000000 sec system
; real time  10.238000 sec (.3047%)



(time (regular-operation *mydb* 10000 :update 1))
;; RA
; cpu time (total)  2.437500 sec user, 0.484375 sec system
; real time  11.202000 sec (26.08%)
;; AC
; cpu time (total)  0.031200 sec user, 0.015600 sec system
; real time  13.809000 sec (.3389%)


(time (regular-operation *mydb* 10000 :update 2))
;; RA
; cpu time (total)  3.203125 sec user, 0.546875 sec system
; real time  16.123000 sec (23.26%)
;; AC
; cpu time (total)  0.015600 sec user, 0.000000 sec system
; real time  13.977000 sec (.1116%)



(time (parameter-operation *mydb* 10000 :update :int 1))
;; RA
; cpu time (total)  0.250000 sec user, 0.187500 sec system
; real time  1.172000 sec (37.33%)
;; AC
; cpu time (total)  0.078001 sec user, 0.171602 sec system
; real time  10.127000 sec (2.465%)

(time (parameter-operation *mydb* 10000 :update :int 2))
;; RA
; cpu time (total)  0.312500 sec user, 0.265625 sec system
; real time  2.156000 sec (26.81%)
;; AC
; cpu time (total)  0.031200 sec user, 0.140401 sec system
; real time  10.648000 sec (1.612%)


(time (parameter-operation *mydb* 10000 :update :long 1))
;; RA
; cpu time (total)  0.296875 sec user, 0.203125 sec system
; real time  1.109000 sec (45.09%)
;; AC
; cpu time (total)  0.015600 sec user, 0.000000 sec system
; real time  10.060000 sec (.1551%)


(time (parameter-operation *mydb* 10000 :update :long 2))
;; RA
; cpu time (total)  0.328125 sec user, 0.125000 sec system
; real time  2.125000 sec (21.32%)
;; AC
; cpu time (total)  0.015600 sec user, 0.000000 sec system
; real time  10.049000 sec (.1552%)


(time (parameter-operation *mydb* 10000 :update :float 1))
;; RA
; cpu time (total)  0.281250 sec user, 0.140625 sec system
; real time  1.984000 sec (21.26%)
;; AC
; cpu time (total)  0.046800 sec user, 0.000000 sec system
; real time  10.056000 sec (.4654%)


(time (parameter-operation *mydb* 10000 :update :float 2))
;; RA
; cpu time (total)  0.421875 sec user, 0.171875 sec system
; real time  1.969000 sec (30.15%)
;; AC
; cpu time (total)  0.046800 sec user, 0.000000 sec system
; real time  10.062000 sec (.4651%)









;; Test UPDATE, records are important

;; NOTAS:
;; - contagem de time foi tirada com 1000 numeros
;;   exemplo:  (time (regular-operation *mydb* 1000 :update 1))


(progn
  (dbi:sql "truncate table dummy_tbl" :db *mydb*)
  (regular-operation *mydb* 10000 :insert 1 :rollback? nil)
  (dbi:sql "commit" :db *mydb*))


(regular-operation *mydb* 2 :update 1)
;; AC
; cpu time (total)  0.000000 sec user, 0.000000 sec system
; real time  0.032000 sec (  0.0%)
;; RA
; cpu time (total)  0.328125 sec user, 0.031250 sec system
; real time  1.219000 sec (29.48%)
SQL_ID        OPERATION                      OPTIONS                        OBJECT_NAME                    SQL_TEXT
------------- ------------------------------ ------------------------------ ------------------------------ ------------------------------------------------
7mftg1bvd34tx INDEX                          UNIQUE SCAN                    DUMMY_TBL_IDX01                update dummy_tbl set data = 'hello' where k1 = 0


(regular-operation *mydb* 2 :update 2)
;; AC
; cpu time (total)  0.000000 sec user, 0.000000 sec system
; real time  0.004000 sec (  0.0%)
;; RA
; cpu time (total)  0.281250 sec user, 0.031250 sec system
; real time  1.157000 sec (27.01%)
SQL_ID        OPERATION                      OPTIONS                        OBJECT_NAME                    SQL_TEXT
------------- ------------------------------ ------------------------------ ------------------------------ ------------------------------------------------
gvdjfdm22c9f1 INDEX                          UNIQUE SCAN                    DUMMY_TBL_IDX02                update dummy_tbl set data = 'hello' where k2 = 0


(parameter-operation *mydb* 2 :update :int 1)
;; AC
; cpu time (total)  0.000000 sec user, 0.000000 sec system
; real time  0.035000 sec (  0.0%)
;; RA
; cpu time (total)  0.109375 sec user, 0.015625 sec system
; real time  0.203000 sec (61.58%)
SQL_ID        OPERATION                      OPTIONS                        OBJECT_NAME                    SQL_TEXT
------------- ------------------------------ ------------------------------ ------------------------------ ------------------------------------------------
2n7uqh67ay2kf INDEX                          UNIQUE SCAN                    DUMMY_TBL_IDX01                update dummy_tbl set data = 'hello' where k1 = :1


(parameter-operation *mydb* 2 :update :int 2)
;; AC
; cpu time (total)  0.000000 sec user, 0.000000 sec system
; real time  0.005000 sec (  0.0%)
;; RA
; cpu time (total)  0.046875 sec user, 0.031250 sec system
; real time  0.172000 sec (45.42%)
SQL_ID        OPERATION                      OPTIONS                        OBJECT_NAME                    SQL_TEXT
------------- ------------------------------ ------------------------------ ------------------------------ ------------------------------------------------
7pgfmtkprygmq INDEX                          UNIQUE SCAN                    DUMMY_TBL_IDX02                update dummy_tbl set data = 'hello' where k2 = :1


(parameter-operation *mydb* 2 :update :long 1)
;; AC
; cpu time (total)  0.000000 sec user, 0.000000 sec system
; real time  0.025000 sec (  0.0%)
;; RA
; cpu time (total)  0.093750 sec user, 0.015625 sec system
; real time  0.187000 sec (58.49%)


(parameter-operation *mydb* 2 :update :long 2)
;; AC
; cpu time (total)  0.000000 sec user, 0.000000 sec system
; real time  0.004000 sec (  0.0%)
;; RA
; cpu time (total)  0.078125 sec user, 0.031250 sec system
; real time  0.157000 sec (69.67%)


(parameter-operation *mydb* 2 :update :float 1)
;; AC
; cpu time (total)  0.000000 sec user, 0.000000 sec system
; real time  0.073000 sec (  0.0%)
;; RA
; cpu time (total)  0.140625 sec user, 0.015625 sec system
; real time  12.892000 sec (1.212%)
SQL_ID        OPERATION                      OPTIONS                        OBJECT_NAME                    SQL_TEXT
------------- ------------------------------ ------------------------------ ------------------------------ ------------------------------------------------
2n7uqh67ay2kf TABLE ACCESS                   FULL                           DUMMY_TBL                      update dummy_tbl set data = 'hello' where k1 = :1


(parameter-operation *mydb* 2 :update :float 2)
;; AC
; cpu time (total)  0.000000 sec user, 0.000000 sec system
; real time  0.007000 sec (  0.0%)
;; RA
;; REMARKS: campo k2 não tem dados em nenhum registo, Oracle perde menos tempo !
; cpu time (total)  0.156250 sec user, 0.000000 sec system
; real time  1.078000 sec (14.49%)
SQL_ID        OPERATION                      OPTIONS                        OBJECT_NAME                    SQL_TEXT
------------- ------------------------------ ------------------------------ ------------------------------ ------------------------------------------------
7pgfmtkprygmq TABLE ACCESS                   FULL                           DUMMY_TBL                      update dummy_tbl set data = 'hello' where k2 = :1


(parameter-operation *mydb* 2 :update :float 1 :force.index? t)
SQL_ID        OPERATION                      OPTIONS                        OBJECT_NAME                    SQL_TEXT
------------- ------------------------------ ------------------------------ ------------------------------ ------------------------------------------------
2q5qps0s2xk63 TABLE ACCESS                   FULL                           DUMMY_TBL                      update /*+ INDEX(dummy_tbl, dummy_tbl_idx01) */ dummy_tbl set data = 'hello' where k1 = :1


(parameter-operation *mydb* 2 :update :float 2 :force.index? t)
SQL_ID        OPERATION                      OPTIONS                        OBJECT_NAME                    SQL_TEXT
------------- ------------------------------ ------------------------------ ------------------------------ ------------------------------------------------
d9pc63qkh2fvf TABLE ACCESS                   FULL                           DUMMY_TBL                      update /*+ INDEX(dummy_tbl, dummy_tbl_idx02) */ dummy_tbl set data = 'hello' where k2 = :1





;; Consultar informação

conn bla009database/bla009database@ora10g2

set linesize 400
column sql_text format A200

SELECT S.SQL_ID, P.OPERATION, P.OPTIONS, P.OBJECT_NAME, S.SQL_TEXT
FROM V$SQL S, V$SQL_PLAN P
WHERE S.SQL_ID = P.SQL_ID
 AND UPPER(S.SQL_TEXT) LIKE 'UPDATE %DUMMY_TBL%:1'
 AND ((P.OPERATION LIKE '%TABLE%' and P.OPTIONS LIKE '%FULL%') OR
      (P.OPERATION LIKE '%INDEX%'))
UNION
SELECT S.SQL_ID, P.OPERATION, P.OPTIONS, P.OBJECT_NAME, S.SQL_TEXT
FROM V$SQL S, V$SQL_PLAN P
WHERE S.SQL_ID = P.SQL_ID
 AND UPPER(S.SQL_TEXT) LIKE 'UPDATE %DUMMY_TBL%'
 AND UPPER(S.SQL_TEXT) NOT LIKE 'UPDATE %DUMMY_TBL%:1'
 AND ROWNUM=1
 AND ((P.OPERATION LIKE '%TABLE%' and P.OPTIONS LIKE '%FULL%') OR
      (P.OPERATION LIKE '%INDEX%'));
