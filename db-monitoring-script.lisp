;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                 ;;
;;    Monitoring Tool                                                                          ;;
;;                                                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Config. Parameters
;;;


;; list of functionalities
(defparameter *functionalities* 
  (list :registered-sessions "Counting of sessions currently registered"
        :database-locks "Get existing database locks"
        :top-sql "top SQL"
        :released-sessions-in-db "Get released sessions analysing bd"
        :released-sessions-in-files "Get released sessions analysing problems-log"
        ))


;; list of config params
(defparameter *configs*
  (list :most.recent.problems.log (* 60 60 24) ;; represents 1 day
        :time.interval.for.bd 2 ;; Ex: 2 or 48/24 = 2 days,
                                ;;     1/2 or 12/24 = 1/2 days
                                ;;     4/24 = 4 hours
   ))


(defparameter *log.file.dir* "c:/temp/") ;; mandatory ends with /
(defparameter *log.file.name* "monitoring-log")
(defparameter *log.file.path* nil)
(defvar *stop.loop.execution.p* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Tool
;;;

;;; Config & Startup

(defun execute.work ()
  (get.number.sessions.in.DB)
  (get.existing.database.locks_v1)
  (get.top.sql)
  (get.released.sessions.in.bd)
  )
 
(defun work.loop (time.interval)
  (do ()
       (*stop.loop.execution.p*)
    (execute.work)
    (sleep (* time.interval 60)))
  (format t "~%End of loop execution~%"))
 
(defun main (&key (time.interval nil))
  ;; validate if process "monitoring loop" is running
  (unless (some #'(lambda (proc)
                    (string= "monitoring loop" (mp:process-name proc)))
                (ac::all-processes))

    ;; create log-file name
    (set.monitoring.log.path (make.monitoring.log.path))

    ;; write ToC
    (log.to.file :log.keyword :toc)
    
    (if time.interval
        ;; execute work.loop
        (progn
          (setf *stop.loop.execution.p* nil)
          (mp:process-run-function (list :name "monitoring loop"
                                         :bindings (ac::get.bindings.with.common.graphics))
            #'work.loop time.interval)
          (format t "~%To end loop execution, call: (setf «package»:*stop.loop.execution.p* t)~%" ))
        ;; execute work
        (execute.work))))


;;; Utilities

;; Util - generate log-file name
(defun get.monitoring.log.path ()
  *log.file.path*)

(defun set.monitoring.log.path (monitoring.log.path)
  (setf *log.file.path* monitoring.log.path))

(defun make.monitoring.log.path (&key (type "txt"))
  (flet ((my.print.date (date)
	   (format nil "~a~2,1,0,'0@a~2,1,0,'0@a" (ul::date.year date) (ul::date.month date) (ul::date.day date)))
	 (my.print.utime (utime)
	   (format nil "~a~2,1,0,'0@a~2,1,0,'0@a-~2,1,0,'0@a~2,1,0,'0@a~2,1,0,'0@a"
		   (ul::utime.year utime)
		   (ul::utime.month utime)
		   (ul::utime.day utime)
		   (ul::utime.hours utime)
		   (ul::utime.minutes utime)
		   (ul::utime.seconds utime))))
    (let* ((utime (get-universal-time))
	   (filename (string-downcase (format nil "~a-in-~a-~a-at-~a-~a~a"
					      *log.file.name*
					      (ac::local-host)
					      (ul::current.user.name)
					      (my.print.utime utime)
					      (dm::short.name (dm::current.partition))
					      (ac::getpid)))))
      (ac::make.pathname :directory *log.file.dir* :name filename :type type))))


;; Util - Log file

(defun insert.newline (stream &key (num.lines 1))
  (let ((newline.pattern (format nil "~~~a%" num.lines)))
    (format stream newline.pattern)))

(defun insert.separator (stream &key (num 80) (char.sep #\.))
           (let ((sep.pattern (format nil "~~%~~~a,,,'~Ca" num char.sep)))
             (format stream sep.pattern char.sep)))


(defun print.data (stream data data.type pattern)
  ;; get padding format
  (flet ((get.padding (offset direction)
                      (cond ((eq direction :r)
                             (format nil "~~~aa" offset))
                            ((eq direction :l)
                             (format nil "~~~a@a" offset)))))
        (do ((iter 0 (incf iter)))
            ((= iter (length pattern)))
            (let* ((elem.pat (nth iter pattern))
                   (elem.data (nth iter data))
                   (pat.offset (first elem.pat))
           (pat.direction (second elem.pat)))        
              (format stream (get.padding pat.offset pat.direction) elem.data)))
        ;; when printing columns -> separate columns from data using total of pattern offset
        (when (eq data.type :columns)
          (insert.separator str :num (reduce #'+ pattern :key #'first)))))
		    

(defun print.results (str test.name columns data pattern)
  (labels ((no.data.to.print ()
			     (insert.newline str :num.lines 2)	     
			     (print.phrase str ">>>> NO DATA <<<<")
			     (insert.newline str :num.lines 2)
			     )
	   (data.to.print ()
			  ;; 1- print columns
			  (print.data str columns :columns pattern)    
			  (insert.newline str)        
			  ;; 2- print data
			  (dolist (d data)
			    (print.data str d :data pattern)
			    (insert.newline str))))
	  
	  ;; initial separator
	  (insert.separator str :num 150)
	  (insert.newline str :num.lines 3)
	  
          ;; print name test
	  (print.phrase str (format nil ">> Test: ~a" test.name))
	  (insert.newline str :num.lines 3)
          
          ;; validate if has data to print
          (if (zerop (length data))
              (no.data.to.print)
            (data.to.print))
          
          ;; final separator
          (insert.newline str :num.lines 2)
          (insert.separator str :num 150)))

(defun print.phrase (str phrase)
  (format str "~% ~a ~%" phrase))

(defun print.toc (str)
  (insert.separator str :num 50 :char.sep #\*)
  (insert.newline str)
  (format str " Monitoring tool log~%")
  (format str " Functionalities to test:~%")
  (ul::do.plist (prop func.name *functionalities*)
    (format str "  - ~a~%" func.name))
  (insert.separator str :num 50 :char.sep #\*)
  (insert.newline str))

(defun log.to.file (&key (log.keyword :data)
                         test.name
                         columns
			 data
                         pattern
                         phrase)
  (let ((filespec (namestring (get.monitoring.log.path))))
    (with-open-file (str filespec :direction :output :if-does-not-exist :create :if-exists :append)
      (case log.keyword
        (:data (print.results str test.name columns data pattern))
        (:toc  (print.toc str))
        (:phrase (print.phrase str phrase))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Monitoring Functions
;;;

;; list of parameters
(defparameter *patterns*
  (list :registered-sessions '((60 :r) (15 :r) (30 :r) (5 :l))        
        :database-locks '((12 :r) (8 :r) (30 :r) (30 :r) (30 :r) (18 :r) (8 :r) (30 :r) (30 :r) (30 :r) (20 :r) (12 :r))
        :top-sql '((20 :r) (5 :r) (8 :r) (5 :r) (8 :r) (100 :r))
        :released-sessions-in-db '((60 :r) (15 :r) (30 :r) (5 :l))
        :released-sessions-in-files 
        ))

;;; Utilities

;; Util -  Truncate list values indicating positions
;;         Example: > (truncate.value.in.positions '(("ola" 1 1.0) ("adeus" 2 2.0)) '(2))
;;                  (("ola" 1 1) ("adeus" 2 2))
(defun truncate.value.in.positions (list positions)
  (mapcar #'(lambda (elem)
              (dolist (position positions)
                (setf (nth position elem) (truncate (nth position elem))))
              elem)
          list))


;; Util - Exceute SQL query
(defun execute.query (query)
  (let ((control.user.conn (ie::get-control-user-db)))
    (multiple-value-bind (data error.msg columns)
        (odbc::execute-sql query :db control.user.conn :column-names? t)
      (declare (ignore error.msg))      
      (values data columns))))



;;; Monitoring Functions


;; 1 - Get number of sessions registered in DB and their loaded periods
(defun get.number.sessions.in.DB ()
  (when (getf *functionalities* :registered-sessions)
    (let ((str.query "select problem_id, node_id, lp_start_date || '<->' || lp_end_date period, count (*) as num_sess
                      from prob_reg_user
                      group by problem_id, node_id, lp_start_date || '<->' || lp_end_date order by 2 desc;"))
      ;; run the test
      (multiple-value-bind (data columns)
          (execute.query str.query)
        ;; print the test    
        (log.to.file :log.keyword :data
                     :test.name (getf *functionalities* :registered-sessions)
                     :columns columns
		     :data (truncate.value.in.positions data (list 3))
                     :pattern (getf *patterns* :registered-sessions))))))



;; 2 - Get existing database locks

;; get existing database locks (V1)
(defun get.existing.database.locks_v1 ()
  (when (getf *functionalities* :database-locks)
    (let ((str.query "SELECT SESSION_ID,SERIAL#,
                             (SELECT OWNER FROM DBA_OBJECTS WHERE (OBJECT_ID = A.OBJECT_ID)) owner,
                             (SELECT OBJECT_NAME FROM DBA_OBJECTS WHERE (OBJECT_ID = A.OBJECT_ID)) object_name,
                             (SELECT OBJECT_TYPE FROM DBA_OBJECTS WHERE (OBJECT_ID = A.OBJECT_ID)) object_type,
                             NVL(LOCKWAIT,'active') lock_p,
                             DECODE(LOCKED_MODE,0,'None or Request',1,'Null',2,'Row share',3,'Row exclusive',
                                                4,'Share',5,'Share row exclusive',6,'Exclusive','Unknown') lock_type,
                             BLOCKING_SESSION,ORACLE_USERNAME,OSUSER,TERMINAL,C.PROGRAM,C.PROCESS
                      FROM V$LOCKED_OBJECT A,V$SESSION C
                      WHERE (C.SID = A.SESSION_ID)
                      ORDER BY NVL2(LOCKWAIT,0,1) DESC;"))
      ;; run the test
      (multiple-value-bind (data columns)
          (execute.query str.query)         
        ;; print the test    
        (log.to.file :log.keyword :data
                     :test.name (getf *functionalities* :database-locks)
                     :columns columns
		     :data (truncate.value.in.positions data (list 0 1))
                     :pattern (getf *patterns* :database-locks))))))

;; 3 - top SQL
(defun get.top.sql ()  
  (when (getf *functionalities* :top-sql)
    (let ((str.query "select ash.SQL_ID ,
                             sum(decode(ash.session_state,'ON CPU',1,0)) \"CPU\",
                             sum(decode(ash.session_state,'WAITING',1,0)) -
                             sum(decode(ash.session_state,'WAITING', decode(en.wait_class, 'User I/O',1,0),0)) \"WAIT\" ,
                             sum(decode(ash.session_state,'WAITING', decode(en.wait_class, 'User I/O',1,0),0)) \"IO\" ,
                             sum(decode(ash.session_state,'ON CPU',1,1))  \"TOTAL\",
                             substr (s.SQL_TEXT , 1, 100)  \"SQL_TEXT\"
                      from v$active_session_history ash,
                           v$event_name en,
                           sys.v_$sqlarea s
                      where ash.SQL_ID is not NULL  and
                            ash.event# = en.event# (+) and
                            SAMPLE_TIME > sysdate - (1/(24*60)) and
                            s.SQL_ID = ash.SQL_ID
                      group by ash.sql_id, s.SQL_TEXT
                      order by sum(decode(session_state,'ON CPU',1,1))   desc;"))
      ;; run the test
      (multiple-value-bind (data columns)
          (execute.query str.query)
      ;; print the test    
        (log.to.file :log.keyword :data
                     :test.name (getf *functionalities* :top-sql)
		     :columns columns
		     :data (truncate.value.in.positions data (list 1 2 3 4))
                     :pattern (getf *patterns* :top-sql))))))


;;; 4 - sessions that should be not registered

;; 4.1 - monitoring DB with log of actions related with permissions
(defun get.released.sessions.in.bd ()
  (flet ((run.test (str.query)
          (log.to.file :log.keyword :data
                       :test.name (getf *functionalities* :released-sessions-in-db)
                       :results (multiple-value-list (execute.query str.query))
                       :pattern (getf *patterns* :released-sessions-in-db))))
    
    (when (getf *functionalities* :released-sessions-in-db)
      ;; TODO include in query: ACTION_DESCRIPTION
      (let ((str.query (format nil "select date_time
                                    from action_log
                                    where action_name = 'Access to RELEASE.ALL - ADB'
                                    AND date_time >= (select trunc(SYSDATE) - ~a res from dual);"
                             (getf *configs* :time.interval.for.bd))))
        ;; run first test
        (run.test str.query)
        ;; inform user
        (log.to.file :log.keyword :phrase
                     :phrase "Vamos esperar um pouco e para repetir novamente...")
        ;; wait a bit
        (sleep 2)
                
        ;; run second test
        (run.test str.query)
        ))))
  

;; 4.2 - analise problems-log

;; Aux 
;; Think - Delete fn??
(defun reject.file? (file.utime)
  (< file.utime (- (get-universal-time) (getf *configs* :most.recent.problems.log))))

(defun get.released.sessions.in.files ()
    (when (getf *tests* :get.released.sessions.in.files)
    ;; get list of files of problems-log
    (let* ((path.problems.log (ie:dp-default-data-directory :problems))
           (files.problems.log (mapcar #'(lambda (file)
                                           (list file (file-write-date file)))
                                       (directory path.problems.log)))
           (final.files nil))
      (dolist (file files.problems.log final.files)
        (let ((file.path (first file))
              (file.utime (second file)))
          ;; consider only the most recents files
          (unless (reject.file? file.utime)
            (let* ((pattern "unable to establish")
                   (command (format nil "findstr /l /m \"~a\" \"~a\""
                                    pattern
                                    (namestring file.path))))
              ;;(format t "Command: ~s~%" command)
              (excl.osi:with-command-output (var command
                                                 :error-output :output)
                (format t "WCO: ~a~%"  var)
                (push (pathname-name file.path) final.files)))))))))



