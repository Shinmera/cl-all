(asdf:defsystem cl-all
  :version "2.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A script to evaluate expressions in multiple lisp implementations."
  :homepage "https://shinmera.com/project/cl-all"
  :serial T
  :components ((:file "cl-all"))
  :depends-on ((:feature :sbcl (:require :sb-posix)))
  :build-operation "program-op"
  :build-pathname "cl-all"
  :entry-point "cl-all:toplevel")
