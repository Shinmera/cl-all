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
   #:evaluate-in-lisp
   #:abcl
   #:allegro
   #:ccl
   #:clisp
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
       (create-input-file :input input :output stream :print print)))
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
        (when print (write-line "(print (progn" output))
        (copy-stream-to-stream input output)
        (when print (write-line "))" output))
        output)))))

(defun executable-paths (&optional (pathvar "PATH"))
  (loop for path in (split #+windows #\; #-windows #\: (sb-posix:getenv pathvar))
        collect (parse-namestring (format NIL "~a~c" path #+windows #\\ #-windows #\/))))

(defun find-executable (name &optional (paths (executable-paths)))
  (dolist (path paths)
    (dolist (file (directory (make-pathname :name :wild :type :wild :defaults path)))
      (when (string= (file-namestring file) name)
        (return-from find-executable file)))))

(defun run (executable &rest args)
  (sb-ext:run-program executable args
                      :output *standard-output*
                      :error *error-output*))

(defun cl-user::ansi (stream code &rest arg)
  (declare (ignore #-unix stream #-unix code arg))
  #+unix (format stream "~c[~dm" #\escape code))

(defclass implementation ()
  ((name :reader name)
   (executable :reader executable)
   (local-executable)))

(defmethod initialize-instance :after ((lisp implementation) &key)
  (unless (slot-boundp lisp 'name)
    (setf (slot-value lisp 'name) (string-upcase (class-name (class-of lisp)))))
  (unless (slot-boundp lisp 'executable)
    (setf (slot-value lisp 'executable) (string-downcase (name lisp)))))

(defmethod local-executable ((lisp implementation))
  (if (slot-boundp lisp 'local-executable)
      (slot-value lisp 'local-executable)
      (setf (slot-value lisp 'local-executable)
            (find-executable (executable lisp)))))

(defmethod local-executable ((lisp class))
  (local-executable (make-instance lisp)))

(defmethod local-executable ((lisp symbol))
  (local-executable (find-class lisp)))

(defun lisp-implementations ()
  (sort (mapcar #'class-name (sb-mop:class-direct-subclasses (find-class 'implementation)))
        #'string<))

(defun available-lisp-implementations ()
  (remove-if-not #'local-executable (lisp-implementations)))

(defmethod run-lisp ((lisp implementation) &rest args)
  (let ((executable (local-executable lisp)))
    (when executable
      (format *error-output* "~% ~/ansi/-->~/ansi/ ~/ansi/~a~/ansi/:~%" 33 0 1 (name lisp) 0)
      (apply #'run executable args))))

(defmethod run-lisp ((lisp class) &rest args)
  (apply #'run-lisp (make-instance lisp) args))

(defmethod run-lisp ((lisp symbol) &rest args)
  (apply #'run-lisp (find-class lisp) args))

(defgeneric evaluate-in-lisp (lisp input))

(defmethod evaluate-in-lisp ((lisp class) _)
  (evaluate-in-lisp (make-instance lisp) _))

(defmethod evaluate-in-lisp ((lisp symbol) _)
  (evaluate-in-lisp (find-class lisp) _))

(defmethod evaluate-in-lisp ((impls list) _)
  (dolist (impl impls)
    (evaluate-in-lisp impl _)))

(defmethod evaluate-in-lisp ((lisp (eql T)) _)
  (evaluate-in-lisp (lisp-implementations) _))

(defmethod evaluate-in-lisp (lisp (string string))
  (evaluate-in-lisp lisp (create-input-file :input string)))

(defmethod evaluate-in-lisp (lisp (stream stream))
  (evaluate-in-lisp lisp (create-input-file :input stream)))

(defclass abcl (implementation) ())

(defmethod evaluate-in-lisp ((lisp abcl) (file pathname))
  (run-lisp lisp "--noinform" "--noinit" "--load" (namestring file) "--eval" "(ext:quit)"))

(defclass allegro (implementation)
  ((name :initform "Allegro")
   (executable :initform "alisp")))

(defmethod evaluate-in-lisp ((lisp allegro) (file pathname))
  (run-lisp lisp "-L" (namestring file) "--kill"))

(defclass ccl (implementation)
  ((executable :initform #+x86-64 "ccl64" #+x86 "ccl")))

(defmethod evaluate-in-lisp ((lisp ccl) (file pathname))
  (run-lisp lisp "-n" "-Q" "-l" (namestring file) "-e" "(ccl:quit)"))

(defclass clisp (implementation)
  ((name :initform "CLisp")))

(defmethod evaluate-in-lisp ((lisp clisp) (file pathname))
  (run-lisp lisp "-q" "-q" "-ansi" "-norc" "-x" (format NIL "(progn (load ~s) (ext:quit 0))" (namestring file))))

(defclass ecl (implementation) ())

(defmethod evaluate-in-lisp ((lisp ecl) (file pathname))
  (run-lisp lisp "--norc" "--shell" (namestring file)))

(defclass cmucl (implementation) ())

(defmethod evaluate-in-lisp ((lisp cmucl) (file pathname))
  (run-lisp lisp "-quiet" "-noinit" "-load" (namestring file) "-eval" "(unix:unix-exit 0)"))

(defclass mkcl (implementation) ())

(defmethod evaluate-in-lisp ((lisp mkcl) (file pathname))
  (run-lisp lisp "-norc" "-q" "-load" (namestring file) "-eval" "(mk-ext:quit)"))

(defclass sbcl (implementation) ())

(defmethod evaluate-in-lisp ((lisp sbcl) (file pathname))
  (run-lisp lisp "--script" (namestring file)))

(defun toplevel (&optional (args (rest sb-ext:*posix-argv*)))
  (let ((input NIL) (print NIL) (impls ()))
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
                      (("-l" "--lisps")
                       (format *error-output* "~{~(~a~)~^ ~}~%" (available-lisp-implementations))
                       (return-from toplevel))
                      (("--")
                       (setf input (format NIL "~@[~a ~]~{~a~^ ~}" input args)))
                      (T
                       (format *error-output* "~&Unknown argument ~s: Ignoring." arg))))
                   ((find arg (available-lisp-implementations) :test #'string-equal)
                    (push (find-symbol (string-upcase arg)) impls))
                   (T
                    (setf input (format NIL "~@[~a ~]~a" input arg)))))
    (evaluate-in-lisp (or (nreverse impls) T)
                      (create-input-file :input (or input *standard-input*)
                                         :print print))))
