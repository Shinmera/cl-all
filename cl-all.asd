#|
 This file is a part of cl-all
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem cl-all
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A script to evaluate expressions in multiple lisp implementations."
  :homepage "https://github.com/Shinmera/halftone"
  :serial T
  :components ((:file "cl-all"))
  :depends-on ((:feature :sbcl (:require :sb-posix)))
  :build-operation "program-op"
  :build-pathname "cl-all"
  :entry-point "cl-all:toplevel")
