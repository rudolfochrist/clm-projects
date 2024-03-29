
(require 'uiop)

(defstruct s-system
  project
  name
  source
  dependencies)

(defun create-ql-source-index (srcpath)
  (with-open-file (sources srcpath)
    (loop with index = (make-hash-table :test 'equal)
          for line = (read-line sources nil)
          while line
          do (handler-case
                 (destructuring-bind (project url)
                     (uiop:split-string line)
                   (setf (gethash project index) url))
               (error (condition)
                 (format t "~A" condition)))
          finally (return index))))


(defun create-ql-systems-index (srcpath)
  (with-open-file (systems srcpath)
    (read-line systems)                 ; skip first line of metadata
    (loop with index = (make-hash-table :test 'equal)
          for system = (read-line systems nil)
          while system
          collect (destructuring-bind (project system-file system-name &rest dependencies)
                      (uiop:split-string system)
                    (declare (ignore system-file))
                    (setf (gethash system-name index)
                          (make-s-system :project project :name system-name :dependencies dependencies)))
          finally (return index))))


(defvar *verified-hosts*
  (list "github.com"
        "gitlab.com"
        "gitlab.common-lisp.net"))


(defun verified-host-p (source)
  (and (uiop:string-prefix-p "https://" source)
       (member (subseq source 8 (position #\/ source :start 8)) *verified-hosts* :test #'string=)))


(defun create-s-systems-index (ql-system-index ql-source-index)
  (loop for s-system being the hash-values of ql-system-index
        for source = (gethash (s-system-project s-system) ql-source-index)
        if (null source) do
          (cerror "Ignore" "Found empty source for ~A" (s-system-name s-system))
        else if (verified-host-p source)
               collect
               (progn
                 (setf (s-system-source s-system) source)
                 s-system)))

(defun write-clm-index-file (filename ql-systems-file ql-sources-file)
  (when (and (probe-file ql-systems-file)
             (probe-file ql-sources-file))
    (let* ((ql-system-index (create-ql-systems-index ql-systems-file))
           (ql-source-index (create-ql-source-index ql-sources-file))
           (s-system-index (create-s-systems-index ql-system-index ql-source-index)))
      (with-open-file (index filename :direction :output :if-exists :error)
        (format index "# project system-name source dependencies0..N~%")
        (loop for sys in s-system-index
              do (format index "~A ~A ~A ~{~A ~}~%"
                         (s-system-project sys)
                         (s-system-name sys)
                         (s-system-source sys)
                         (s-system-dependencies sys)))))))

(let ((argv (uiop:command-line-arguments)))
  (if (and argv
           (= 3 (length argv)))
      (write-clm-index-file
       (car argv)
       (cadr argv)
       (caddr argv))
      (format *error-output*
              "Usage: sbcl --script indexer.lisp SYSTEMS-FILENAME QUICKLISP-SYSTEMS-FILE QUICKLISP-SOURCES-FILE")))
