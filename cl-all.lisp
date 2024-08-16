#|
exec sbcl \
  --noinform \
  --no-userinit \
  --disable-debugger \
  --load "$0" \
  --eval "(cl-all:toplevel)" \
  --quit \
  --end-toplevel-options "$@"
|#

(require 'sb-posix)
(defpackage #:cl-all
  (:use #:cl)
  (:export
   #:implementation
   #:name
   #:executable
   #:local-executable
   #:lisp-implementations
   #:available-lisp-implementations
   #:run-lisp
   #:eval-in-lisp
   #:run
   #:implementation
   #:output
   #:status
   #:done-p
   #:abcl
   #:allegro
   #:ccl
   #:clasp
   #:clisp
   #:cmucl
   #:ecl
   #:gcl
   #:jscl
   #:mkcl
   #:sbcl
   #:toplevel))
(in-package #:cl-all)

(defvar *ansi* NIL)

(defun starts-with (prefix string)
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defmacro case* (test arg1 &body cases)
  (let ((arg1g (gensym "ARG1")))
    `(let ((,arg1g ,arg1))
       (cond ,@(loop for (arg2 . body) in cases
                     collect (if (eql arg2 T)
                                 `(T ,@body)
                                 `((or ,@(loop for arg in (if (listp arg2) arg2 (list arg2))
                                               collect `(,test ,arg1g ,arg)))
                                   ,@body)))))))

(defun split (split string)
  (let ((parts ()) (buffer (make-string-output-stream)))
    (flet ((maybe-output ()
             (let ((part (get-output-stream-string buffer)))
               (when (string/= part "") (push part parts)))))
      (loop for char across string
            do (if (char= char split)
                   (maybe-output)
                   (write-char char buffer))
            finally (maybe-output))
      (nreverse parts))))

(defun temp-file ()
  (make-pathname
   :name (format NIL "cl-all-~d-~4,'0x" (get-universal-time) (random #xFFFF))
   :type "lisp"
   :defaults
   #+windows (merge-pathnames "AppData/Local/Temp/" (user-homedir-pathname))
   #-windows #p"/tmp/"))

(defun copy-stream-to-stream (input output)
  (let ((buffer (make-string 4096)))
    (loop for read = (read-sequence buffer input)
          while (< 0 read)
          do (write-sequence buffer output :end read))))

(defun create-input-file (&rest args &key (input *standard-input*) (output (temp-file)) print disassemble)
  (etypecase output
    ((or pathname string)
     (with-open-file (stream output :direction :output
                                    :element-type 'character
                                    :if-exists :supersede)
       (apply #'create-input-file :output stream args)
       (pathname output)))
    (stream
     (etypecase input
       (pathname
        (with-open-file (stream input :direction :input
                                      :element-type 'character)
          (apply #'create-input-file :input stream args)))
       (string
        (let ((stream (make-string-input-stream input)))
          (apply #'create-input-file :input stream args)))
       (stream
        (format output "~&(cl:defun cl-user::cl-all-thunk ()~%")
        (copy-stream-to-stream input output)
        (format output ")~%")
        (when disassemble
          (format output "~&(cl:disassemble (cl:compile 'cl-user::cl-all-thunk))~%"))
        (if print
            (format output "~&(cl:format cl:*standard-output* \"~~a\" (cl-user::cl-all-thunk))~%")
            (format output "~&(cl-user::cl-all-thunk)~%"))
        output)))))

(defun executable-paths (&optional (pathvar "PATH"))
  (loop for path in (split #+windows #\; #-windows #\: (sb-posix:getenv pathvar))
        collect (parse-namestring (format NIL "~a~c" path #+windows #\\ #-windows #\/))))

(defun bytes= (array &rest test)
  (loop for comp in test
        for byte across array
        always (= comp byte)))

(defun executable-file-p (path)
  (when (or (pathname-name path)
            (pathname-type path))
    (with-open-file (stream path :direction :input
                                 :element-type '(unsigned-byte 8)
                                 :if-does-not-exist NIL)
      (when stream
        (let ((bytes (make-array 4 :element-type '(unsigned-byte 8)
                                   :initial-element 0)))
          (read-sequence bytes stream)
          (or ;; Windows Executable
           (bytes= bytes #x4D #x5A)
           ;; Windows Portable Executable
           (bytes= bytes #x5A #x4D)
           ;; ELF Executable
           (bytes= bytes #x7F #x45 #x4C #x46)
           ;; Mach-O Executable
           (or (bytes= bytes #xCE #xFA #xED #xFE)  ;; Mach-O Executable i386
               (bytes= bytes #xCF #xFA #xED #xFE)) ;; Mach-O Executable x86_64
           ;; Script with a shebang
           #+unix (bytes= bytes #x23 #x21)))))))

(defun find-executable (name &optional (directories (executable-paths)))
  (flet ((find-single (path)
           (let* ((path (pathname path))
                  (name (pathname-name path))
                  (type (pathname-type path)))
             (loop for directory in directories
                   for path = (directory (make-pathname :name name :type type :defaults directory))
                   do (when (and path (executable-file-p (first path)))
                        (return-from find-executable (first path)))))))
    (if (listp name)
        (mapc #'find-single name)
        (find-single name))
    NIL))

(defun cl-user::ansi (stream code &rest arg)
  (declare (ignore arg))
  (when *ansi*
    (format stream "~c[~dm" #\escape code)))

(defclass implementation ()
  ((name :reader name)
   (executable :reader executable)
   (local-executable)))

(defmethod initialize-instance :after ((lisp implementation) &key)
  (unless (slot-boundp lisp 'name)
    (setf (slot-value lisp 'name) (string-upcase (class-name (class-of lisp)))))
  (unless (slot-boundp lisp 'executable)
    (setf (slot-value lisp 'executable) (string-downcase (name lisp)))))

(defmethod ensure-lisp ((lisp implementation))
  lisp)

(defmethod ensure-lisp ((lisp class))
  (make-instance lisp))

(defmethod ensure-lisp ((lisp symbol))
  (etypecase lisp
    (keyword (ensure-lisp (find-symbol (string lisp) #.*package*)))
    ((not null) (ensure-lisp (find-class lisp)))))

(defmethod name (lisp)
  (name (ensure-lisp lisp)))

(defmethod executable (lisp)
  (executable (ensure-lisp lisp)))

(defmethod local-executable ((lisp implementation))
  (if (slot-boundp lisp 'local-executable)
      (slot-value lisp 'local-executable)
      (setf (slot-value lisp 'local-executable)
            (find-executable (executable lisp)))))

(defmethod local-executable (lisp)
  (local-executable (ensure-lisp lisp)))

(defun lisp-implementations ()
  (sort (mapcar #'class-name (sb-mop:class-direct-subclasses (find-class 'implementation)))
        #'string<))

(defvar *available-lisp-implementations* ())
(defun available-lisp-implementations (&key force)
  (when (or force (null *available-lisp-implementations*))
    (setf *available-lisp-implementations* (remove-if-not #'local-executable (lisp-implementations))))
  *available-lisp-implementations*)

(defclass run ()
  ((implementation :initarg :implementation :accessor implementation)
   (arguments :initarg :arguments :initform () :accessor arguments)
   (output-stream :initform (make-string-output-stream) :accessor output-stream)
   (process :initform NIL :accessor process)))

(defmethod initialize-instance :after ((run run) &key)
  (setf (process run) (sb-ext:run-program (local-executable (implementation run))
                                          (remove NIL (arguments run))
                                          :input NIL
                                          :wait NIL
                                          :output (output-stream run)
                                          :error :output)))

(defmethod print-object ((run run) stream)
  (print-unreadable-object (run stream :type T :identity T)
    (format stream "~a ~a"
            (name (implementation run))
            (status run))))

(defmethod status ((run run))
  (cond ((null (process run)) :PENDING)
        ((sb-ext:process-alive-p (process run)) :RUNNING)
        ((= 0 (sb-ext:process-exit-code (process run))) :DONE)
        (T :FAILED)))

(defmethod done-p ((run run))
  (member (status run) '(:failed :done)))

(defmethod output ((run run))
  (get-output-stream-string (output-stream run)))

(defmethod wait-until-done ((run run))
  (sb-ext:process-wait (process run) T)
  run)

(defmethod run-lisp ((lisp implementation) &rest args)
  (make-instance 'run :implementation lisp :arguments args))

(defmethod run-lisp (lisp &rest args)
  (apply #'run-lisp (ensure-lisp lisp) args))

(defgeneric eval-in-lisp (lisp input &key &allow-other-keys))
(defgeneric eval-wrapper (lisp file &optional destination))
(defgeneric quit-form (lisp code))

(defmethod eval-in-lisp :around ((lisp implementation) _ &key)
  (with-simple-restart (abort "Don't run ~a" lisp)
    (call-next-method)))

(defmethod eval-in-lisp (lisp _ &rest args &key)
  (apply #'eval-in-lisp (ensure-lisp lisp) _ args))

(defmethod eval-in-lisp ((impls list) _ &rest args &key)
  (dolist (impl impls)
    (apply #'eval-in-lisp impl _ args)))

(defmethod eval-in-lisp ((lisp (eql T)) _ &rest args &key)
  (apply #'eval-in-lisp (lisp-implementations) _ args))

(defmethod eval-in-lisp (lisp (string string) &rest args &key)
  (apply #'eval-in-lisp lisp (create-input-file :input string) args))

(defmethod eval-in-lisp (lisp (stream stream) &rest args &key)
  (apply #'eval-in-lisp lisp (create-input-file :input stream) args))

(defmethod eval-wrapper (lisp file &optional destination)
  (format destination "~
(cl:flet ((#1=finish () ~
            (cl:finish-output cl:*standard-output*) ~
            (cl:finish-output cl:*error-output*))) ~
  (cl:handler-case ~
      (cl:progn (cl:load ~s :verbose cl:nil :print cl:nil) ~
        (#1#) ~
        ~a) ~
    (cl:error (e) ~
     (cl:format cl:*error-output* \"~~&~~%ERROR: ~~a\" e) ~
     (#1#) ~
     ~a)))"
          (namestring file) (quit-form lisp 0) (quit-form lisp 1)))

(defclass abcl (implementation) ())

(defmethod quit-form ((lisp abcl) code)
  (format NIL "(ext:quit :status ~d)" code))

(defmethod eval-in-lisp ((lisp abcl) (file pathname) &key with-rc)
  (run-lisp lisp "--noinform"
            (unless with-rc "--noinit")
            "--eval" (eval-wrapper lisp file)))

(defclass allegro (implementation)
  ((name :initform "Allegro")
   (executable :initform '("alisp" "allegro"))))

(defmethod quit-form ((lisp allegro) code)
  (format NIL "(excl:exit ~d :quiet T)" code))

(defmethod eval-in-lisp ((lisp allegro) (file pathname) &key with-rc)
  (run-lisp lisp
            (unless with-rc "--qq")
            (when with-rc "-e")
            (when with-rc "(mapcar #'(lambda (init-file) (when (probe-file init-file) (load init-file))) (remove-duplicates (list (translate-logical-pathname \"sys:siteinit.cl\") (merge-pathnames \".clinit.cl\" (user-homedir-pathname)) (merge-pathnames \"clinit.cl\" (user-homedir-pathname)) (merge-pathnames \".clinit.cl\" *default-pathname-defaults*) (merge-pathnames \"clinit.cl\" *default-pathname-defaults*)) :test #'equal))")
            "-e" (eval-wrapper lisp file)))

(defclass ccl (implementation)
  ((executable :initform '("ccl64" "lx86cl64" "ccl" "lx86cl"))))

(defmethod quit-form ((lisp ccl) code)
  (format NIL "(ccl:quit ~d)" code))

(defmethod eval-in-lisp ((lisp ccl) (file pathname) &key with-rc)
  (Run-lisp lisp "-Q"
            (unless with-rc "-n")
            "-e" (eval-wrapper lisp file)))

(defclass clasp (implementation)
  ((name :initform "Clasp")))

(defmethod quit-form ((lisp clasp) code)
  (format NIL "(si:quit ~d)" code))

(defmethod eval-in-lisp ((lisp clasp) (file pathname) &key with-rc)
  (run-lisp lisp "--noinform" "-N"
            (unless with-rc "-r")
            "-e" (eval-wrapper lisp file)))

(defclass clisp (implementation)
  ((name :initform "CLISP")))

(defmethod quit-form ((lisp clisp) code)
  (format NIL "(ext:quit ~d)" code))

(defmethod eval-in-lisp ((lisp clisp) (file pathname) &key with-rc)
  (run-lisp lisp "-q" "-q" "-ansi"
            (unless with-rc "-norc")
            "-x" (eval-wrapper lisp file)))

(defclass cmucl (implementation)
  ((executable :initform '("cmucl" "lisp"))))

(defmethod quit-form ((lisp cmucl) code)
  (format NIL "(unix:unix-exit ~d)" code))

(defmethod eval-in-lisp ((lisp cmucl) (file pathname) &key with-rc)
  (run-lisp lisp "-quiet"
            (unless with-rc "-noinit")
            "-eval" (eval-wrapper lisp file)))

(defclass ecl (implementation) ())

(defmethod quit-form ((lisp ecl) code)
  (format NIL "(si:quit ~d)" code))

(defmethod eval-in-lisp ((lisp ecl) (file pathname) &key with-rc)
  (run-lisp lisp "-q"
            (unless with-rc "--norc")
            "--eval" (eval-wrapper lisp file)))

(defclass gcl (implementation) ())

(defmethod quit-form ((lisp gcl) code)
  (format NIL "(bye ~d)" code))

(defmethod eval-in-lisp ((lisp gcl) (file pathname) &key)
  (run-lisp lisp "-eval" (eval-wrapper lisp file)))

(defclass jscl (implementation)
  ((executable :initform '("jscl" "jscl-repl"))))

(defmethod quit-form ((lisp jscl) code)
  (format NIL "(#j:process:exit ~d)" code))

(defmethod eval-in-lisp ((lisp jscl) (file pathname) &key)
  (let ((tmp (temp-file)))
    (with-open-file (stream tmp :direction :output :if-exists :supersede)
      (eval-wrapper lisp file stream))
    (run-lisp lisp (namestring tmp))))

(defclass mkcl (implementation) ())

(defmethod quit-form ((lisp mkcl) code)
  (format NIL "(mx-ext:quit :exit-code ~d)" code))

(defmethod eval-in-lisp ((lisp mkcl) (file pathname) &key with-rc)
  (run-lisp lisp "-q"
            (unless with-rc "-norc")
            "-eval" (eval-wrapper lisp file)))

(defclass sbcl (implementation) ())

(defmethod quit-form ((lisp sbcl) code)
  (format NIL "(sb-ext:exit :code ~d)" code))

(defmethod eval-in-lisp ((lisp sbcl) (file pathname) &key with-rc)
  (run-lisp lisp "--disable-ldb"
            "--noinform"
            "--lose-on-corruption"
            (unless with-rc "--no-sysinit")
            (unless with-rc "--no-userinit")
            "--eval" (eval-wrapper lisp file)))

(defvar *help-string* "cl-all, the Common Lisp implementation comparator

Usage:
cl-all (implementation | option | snippet)*

  implementation:
    The given implementation is run. If no implementations are
    explicitly specified, all known and available implementations
    are used. See -l for a list of available implementations.
  
  option:
    -a --ansi          Use ANSI escape codes to mark up the output.

    -d --disassemble   Compiles and prints the disassembly of the
                       expressions.

    -e --eval          Evaluates the given expression.

    -f --file          Uses the given file as input.

    -h --help          Show this usage prompt and exit.

    -l --lisps         List all known and available implementations
                       and exit.

    -n --no-rc         Do not run implementation init files.

    -p --print         Causes the last form's value to be printed.
                       Will also trim extraneous whitespace from the
                       output.

    -x --parallel      Run the implementations in parallel.

    --                 All further arguments are used as tokens to be
                       evaluated, concatenated by spaces.
  
  snippet:
    A lisp snippet to be evaluated.
  
  If no snippet, file, or eval option is given, the standard input is
  used to read forms from. Forms are read until EOF is encountered~%")

(defun parse-args (args)
  (let ((input NIL) (print NIL) (disassemble NIL) (impls ()) (with-rc T) (parallel NIL))
    (loop for arg = (pop args)
          while arg
          do (cond ((starts-with "--" arg)
                    (case* string= arg
                      ("--ansi"
                       (setf *ansi* T))
                      ("--print"
                       (setf print T))
                      ("--file"
                       (setf input (parse-namestring (pop args))))
                      ("--eval"
                       (setf input (format NIL "~@[~a~%~]~a" input (pop args))))
                      ("--no-rc"
                       (setf with-rc NIL))
                      ("--disassemble"
                       (setf disassemble T))
                      ("--parallel"
                       (setf parallel T))
                      ("--"
                       (setf input (format NIL "~@[~a ~]~{~a~^ ~}" input args))
                       (setf args ()))
                      ("--help"
                       (format *error-output* *help-string*)
                       (sb-ext:exit))
                      ("--lisps"
                       (format *error-output* "~{~(~a~)~^ ~}~%" (available-lisp-implementations))
                       (sb-ext:exit))
                      (T
                       (format *error-output* "~&Unknown argument ~s: Ignoring." arg))))
                   ((starts-with "-" arg)
                    (loop for i from 1 below (length arg)
                          do (case (char arg i)
                               (#\a (setf *ansi* T))
                               (#\p (setf print T))
                               (#\f (setf input (parse-namestring (pop args))))
                               (#\e (setf input (format NIL "~@[~a~%~]~a" input (pop args))))
                               (#\n (setf with-rc NIL))
                               (#\d (setf disassemble T))
                               (#\x (setf parallel T))
                               (#\h (format *error-output* *help-string*) (sb-ext:exit))
                               (#\l (format *error-output* "~{~(~a~)~^ ~}~%" (available-lisp-implementations)) (sb-ext:exit))
                               (T (format *error-output* "~&Unknown argument ~s: Ignoring." arg)))))
                   ((find arg (lisp-implementations) :test #'string-equal)
                    (let ((impl (find arg (lisp-implementations) :test #'string-equal)))
                      (unless (local-executable impl)
                        (error "Cannot run on ~a: can't find its executable locally!" arg))
                      (push impl impls)))
                   (T
                    (setf input (format NIL "~@[~a ~]~a" input arg)))))
    (values (create-input-file :input (or input *standard-input*)
                               :print print
                               :disassemble disassemble)
            (or (nreverse impls) (available-lisp-implementations))
            parallel with-rc print disassemble)))

(defun output-run (run print disassemble)
  (let ((out (output run)))
    (ecase (status run)
      ((:pending :running))
      (:done
       (when (or print disassemble)
         (when (and disassemble (not print))
           (terpri))
         (format T "~&~/ansi/~a~/ansi/: ~vt~a~%"
                 1 (name (implementation run)) 0
                 (if (interactive-stream-p *standard-output*) 34 16)
                 (string-trim '(#\Linefeed #\Return #\Space #\Tab) out))))
      (:failed
       (format T "~&~/ansi/~a~/ansi/: ~/ansi/[FAILED]~/ansi/ ~vt~a~%"
               1 (name (implementation run)) 0 31 0
               (if (interactive-stream-p *standard-output*) 34 16)
               (string-trim '(#\Linefeed #\Return #\Space #\Tab) out))))))

(defun toplevel (&optional (args (rest sb-ext:*posix-argv*)))
  (handler-case
      (multiple-value-bind (input impls parallel with-rc print disassemble) (parse-args args)
        (unwind-protect
             (if parallel
                 (let ((runs (loop for impl in impls
                                   collect (eval-in-lisp impl input :with-rc with-rc))))
                   (loop while runs
                         do (loop for run in runs
                                  do (when (done-p run)
                                       (wait-until-done run)
                                       (output-run run print disassemble)
                                       (setf runs (remove run runs))
                                       (return))
                                  finally (sleep 0.01))))
                 (loop for impl in impls
                       for run = (eval-in-lisp impl input :with-rc with-rc)
                       do (wait-until-done run)
                          (output-run run print disassemble)))
          (when (probe-file input)
            (delete-file input)))
        (fresh-line))
    (sb-sys:interactive-interrupt ()
      (format *error-output* "~&Interrupted~%")
      (sb-ext:exit :code 1 :abort T))))
