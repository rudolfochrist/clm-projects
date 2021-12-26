
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
                    (setf (gethash project index)
                          (make-s-system :project project :name system-name :dependencies dependencies)))
          finally (return index))))


(defun githubp (source)
  (uiop:string-prefix-p "https://github.com/" source))


(defun gitlabp (source)
  (uiop:string-prefix-p "https://gitlab.com/" source))


(defun create-s-systems-index (ql-system-index ql-source-index)
  (loop for project being the hash-keys of ql-system-index
          using (hash-value s-system)
        for source = (gethash project ql-source-index)
        if (null source) do
          (cerror "Found empty source for ~A" project)
        else if (or (githubp source)
                    (gitlabp source))
               collect
               (progn
                 (setf (s-system-source s-system) source)
                 s-system)))
