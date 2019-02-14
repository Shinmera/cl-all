#|
 This file is a part of cl-all
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
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
   #:abcl
   #:allegro
   #:ccl
   #:clasp
   #:clisp
   #:cmucl
   #:ecl
   #:mkcl
   #:sbcl
   #:toplevel))
(in-package #:cl-all)

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
  #p"/tmp/cl-all.lisp")

(defun copy-stream-to-stream (input output)
  (let ((buffer (make-string 4096)))
    (loop for read = (read-sequence buffer input)
          while (< 0 read)
          do (write-sequence buffer output :end read))))

(defun create-input-file (&key (input *standard-input*) (output (temp-file)) print)
  (etypecase output
    ((or pathname string)
     (with-open-file (stream output :direction :output
                                    :element-type 'character
                                    :if-exists :supersede)
       (create-input-file :input input :output stream :print print)
       (pathname output)))
    (stream
     (etypecase input
       (pathname
        (with-open-file (stream input :direction :input
                                      :element-type 'character)
          (create-input-file :input stream :output output :print print)))
       (string
        (let ((stream (make-string-input-stream input)))
          (create-input-file :input stream :output output :print print)))
       (stream
        (when print (write-line "(cl:format T \"~a\" (cl:progn" output))
        (copy-stream-to-stream input output)
        (when print (write-line "))" output))
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
           (bytes= bytes #xFE #xED #xFA #xCE)
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
        (find-single name))))

(defun run (executable &rest args)
  (sb-ext:run-program executable (remove NIL args)
                      :input NIL
                      :output *standard-output*
                      :error *standard-output*))

(defun cl-user::ansi (stream code &rest arg)
  (declare (ignore arg))
  (when (interactive-stream-p stream)
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

(defun available-lisp-implementations ()
  (remove-if-not #'local-executable (lisp-implementations)))

(defmethod run-lisp ((lisp implementation) &rest args)
  (apply #'run (local-executable lisp) args))

(defmethod run-lisp (lisp &rest args)
  (apply #'run-lisp (ensure-lisp lisp) args))

(defgeneric eval-in-lisp (lisp input with-rc))
(defgeneric eval-wrapper (lisp file))
(defgeneric quit-form (lisp code))

(defmethod eval-in-lisp :around ((lisp implementation) _ __)
  (with-simple-restart (abort "Don't run ~a" lisp)
    (call-next-method)))

(defmethod eval-in-lisp (lisp _ __)
  (eval-in-lisp (ensure-lisp lisp) _ __))

(defmethod eval-in-lisp ((impls list) _ __)
  (dolist (impl impls)
    (eval-in-lisp impl _ __)))

(defmethod eval-in-lisp ((lisp (eql T)) _ __)
  (eval-in-lisp (lisp-implementations) _ __))

(defmethod eval-in-lisp (lisp (string string) _)
  (eval-in-lisp lisp (create-input-file :input string) _))

(defmethod eval-in-lisp (lisp (stream stream) _)
  (eval-in-lisp lisp (create-input-file :input stream) _))

(defmethod eval-wrapper (lisp file)
  (format NIL "(flet ((finish () ~
                        (finish-output *standard-output*) ~
                        (finish-output *error-output*))) ~
                 (handler-case ~
                     (progn (load ~s) ~
                       (finish) ~
                       ~a) ~
                   (error (e) ~
                    (format *error-output* \"~&~%ERROR: ~~a\" e) ~
                    (finish)
                    ~a)))"
          (namestring file) (quit-form lisp 0) (quit-form lisp 1)))

(defclass abcl (implementation) ())

(defmethod quit-form ((lisp abcl) code)
  (format NIL "(ext:quit :status ~d)" code))

(defmethod eval-in-lisp ((lisp abcl) (file pathname) with-rc)
  (run-lisp lisp "--noinform" (unless with-rc "--noinit") "--eval" (eval-wrapper lisp file)))

(defclass allegro (implementation)
  ((name :initform "Allegro")
   (executable :initform '("alisp" "allegro"))))

(defmethod quit-form ((lisp allegro) code)
  (format NIL "(excl:exit ~d)" code))

(defmethod eval-in-lisp ((lisp allegro) (file pathname) with-rc)
  ;; FIXME: Allegro seems to run -e /before/ rc files are loaded.
  (run-lisp lisp (unless with-rc "--qq") "-e" (eval-wrapper lisp file)))

(defclass ccl (implementation)
  ((executable :initform '("ccl64" "lx86cl64" "ccl" "lx86cl"))))

(defmethod quit-form ((lisp ccl) code)
  (format NIL "(ccl:quit ~d)" code))

(defmethod eval-in-lisp ((lisp ccl) (file pathname) with-rc)
  (Run-lisp lisp (unless with-rc "-n") "-Q" "-e" (eval-wrapper lisp file)))

(defclass clasp (implementation)
  ((name :initform "Clasp")))

(defmethod quit-form ((lisp clasp) code)
  (format NIL "(si:quit ~d)" code))

(defmethod eval-in-lisp ((lisp clasp) (file pathname) with-rc)
  (run-lisp lisp (unless with-rc "-r") "-N" "-e" (eval-wrapper lisp file)))

(defclass clisp (implementation)
  ((name :initform "CLisp")))

(defmethod quit-form ((lisp clisp) code)
  (format NIL "(ext:quit ~d)" code))

(defmethod eval-in-lisp ((lisp clisp) (file pathname) with-rc)
  (run-lisp lisp "-q" "-q" "-ansi" (unless with-rc "-norc") "-x" (eval-wrapper lisp file)))

(defclass cmucl (implementation) ())

(defmethod quit-form ((lisp cmucl) code)
  (format NIL "(unix:unix-exit ~d)" code))

(defmethod eval-in-lisp ((lisp cmucl) (file pathname) with-rc)
  (run-lisp lisp "-quiet" (unless with-rc "-noinit") "-eval" (eval-wrapper lisp file)))

(defclass ecl (implementation) ())

(defmethod quit-form ((lisp ecl) code)
  (format NIL "(si:quit ~d)" code))

(defmethod eval-in-lisp ((lisp ecl) (file pathname) with-rc)
  (run-lisp lisp (unless with-rc "--norc") "--eval" (eval-wrapper lisp file)))

(defclass mkcl (implementation) ())

(defmethod quit-form ((lisp mkcl) code)
  (format NIL "(mx-ext:quit :exit-code ~d)" code))

(defmethod eval-in-lisp ((lisp mkcl) (file pathname) with-rc)
  (run-lisp lisp (unless with-rc "-norc") "-q" "-eval" (eval-wrapper lisp file)))

(defclass sbcl (implementation) ())

(defmethod quit-form ((lisp sbcl) code)
  (format NIL "(sb-ext:exit :code ~d)" code))

(defmethod eval-in-lisp ((lisp sbcl) (file pathname) with-rc)
  (run-lisp lisp "--disable-ldb" "--lose-on-corruption"
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
    --print -p   Causes the last form's value to be printed.
    --file  -f   Uses the given file as input.
    --eval  -e   Evaluates the given expression.
    --no-rc -n   Do not run implementation init files.
    --lisps -l   Lists all known and available implementations and exit.
    --help  -h   Show this usage prompt and exit.
    --           All further arguments are used as tokens to be evaluated,
                 concatenated by spaces.
  
  snippet:
    A lisp snippet to be evaluated.
  
  If no snippet, file, or eval option is given, the standard input is
  used to read forms from. Forms are read until EOF is encountered (C-d).~%")

(defun toplevel (&optional (args (rest sb-ext:*posix-argv*)))
  (let ((input NIL) (print NIL) (impls ()) (with-rc T))
    (loop for arg = (pop args)
          while arg
          do (cond ((starts-with "-" arg)
                    (case* string= arg
                      (("-p" "--print")
                       (setf print T))
                      (("-f" "--file")
                       (setf input (parse-namestring (pop args))))
                      (("-e" "--eval")
                       (setf input (format NIL "~@[~a~%~]~a" input (pop args))))
                      (("-n" "--no-rc")
                       (setf with-rc NIL))
                      (("--")
                       (setf input (format NIL "~@[~a ~]~{~a~^ ~}" input args))
                       (setf args ()))
                      (("-h" "--help")
                       (format *error-output* *help-string*)
                       (return-from toplevel))
                      (("-l" "--lisps")
                       (format *error-output* "~{~(~a~)~^ ~}~%" (available-lisp-implementations))
                       (return-from toplevel))
                      (T
                       (format *error-output* "~&Unknown argument ~s: Ignoring." arg))))
                   ((find arg (available-lisp-implementations) :test #'string-equal)
                    (push (find arg (available-lisp-implementations) :test #'string-equal) impls))
                   (T
                    (setf input (format NIL "~@[~a ~]~a" input arg)))))
    (loop with input = (create-input-file :input (or input *standard-input*)
                                          :print print)
          for impl in (or (nreverse impls) (available-lisp-implementations))
          do (format T "~& ~/ansi/-->~/ansi/ ~/ansi/~a~/ansi/: ~vt" 33 0 1 (name impl) 0
                     (if (interactive-stream-p *standard-output*) 34 16))
             (force-output)
             (handler-case
                 (eval-in-lisp impl input with-rc)
               (error (e)
                 (format T "~& ~/ansi/[ERR]~/ansi/ ~vt~a" 31 0
                         (if (interactive-stream-p *standard-output*) 34 16) e))))
    (fresh-line)))
