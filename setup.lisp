(in-package :mudballs-presenter)

(defvar *package-form* '(in-package :sysdef-user))
(defvar *root-directory* "~/Code/Lisp/mudballs/"
  "The Root directory where the projects can be found.")



;;;;;;;;;;;;;;;;
;; UTILS
(defun last-dir-name (path)
  (car (last (pathname-directory path))))

(defun read-form (path)
  (when (probe-file path)
    (with-open-file (stream path)
      (read stream nil nil))))

(defun get-option (form name)
  (cdr (assoc name form)))

(defun md5sum-file (file)
  (byte-array-to-hex-string (digest-file :md5 file)))

(defun define-system-template-form-p (form)
  (and (consp form) (eql (first form) 'define-system-template))) 

(defun name-argument-is (form name)
  (and (consp form) (sysdef:name= (second form) name)))


;;;;;;;;;;
;; PATHNAME LOOKUP ROUTINES
(defun support-file (root)
  (merge-pathnames (make-pathname :directory '(:relative "support") :name "sysdef-support" :type "lisp")
                   root))


(defun template-path (project)
  (merge-pathnames (make-pathname :name (name-of project) :type "mbs")
                   (path-of project)))


(defun projects-root (root)
  (merge-pathnames (make-pathname :directory '(:relative "projects"))
                   root))

(defun all-projects (root)
  (list-directory (projects-root root)))


(defun project-version-path (name version &optional (root *root-directory*))
  (merge-pathnames (make-pathname :directory `(:relative ,(sysdef::version-string version)))
                   (project-path name root)))

(defun project-path (name &optional (root *root-directory*))
  (merge-pathnames (make-pathname :directory `(:relative ,name))
                   (projects-root root)))




;;;;;;;;;;;;;;;;;;;;;;;;;
;; THE PROJECT CLASS

(defclass project ()
  ((path :initarg :path :initform (error "Path is required.") :reader path-of))
  (:documentation "Represents the concept of a collection of system definitions along
with install files located on disk. The project may have a system definition template
file which can be used with a system-definition-file to produce a merged version
with all of the information."))

(defmethod print-object ((project project) stream)
  (print-unreadable-object (project stream :type t :identity t)
    (princ (name-of project) stream)))

(defmethod versioned-name ((project project) version)
  (format nil "~A.~A" (name-of project)
          (sysdef::version-string version)))

(defun project-exists-p (project-name)
  (directory-exists-p (project-path project-name)))

;; project paths
(defmethod version-exists-p ((project project) version)
  (directory-exists-p (project-version-path (name-of project) version)))

(defmethod project-sysdef-file ((project project) version)
  (make-pathname :name (name-of project) :type "mbd"
                 :defaults (project-version-path (name-of project) version)))

(defmethod project-mudball-file ((project project) version)
  (make-pathname :name (versioned-name project version) :type "mb"
                 :defaults (project-version-path (name-of project) version)))

(defmethod project-support-file ((project project))
  (make-pathname :name "support" :type "lisp"
                 :defaults (project-path (name-of project))))

(defmethod create-release-folder ((project project) version)
  (ensure-directories-exist (project-version-path (name-of project) version)))


(defmethod name-of ((project project))
  (last-dir-name (path-of project)))

(defmethod system-template ((project project))
  "Return the code for the system template. This is found by looking for the first
form which is a define-system-template whose first argument is name= the name of PROJECT."
  (let ((path (template-path project)))
    (if (probe-file path)
        (define-system-template-form (name-of project) path)
        (warn "No default system template for ~S." project))))

(defun define-system-template-form (name path)
  (let ((eof (gensym)))
    (with-open-file (stream path) 
      (loop :for form = (read stream nil eof)
            :until (eql form eof)
            :when (and (define-system-template-form-p form)
                       (name-argument-is form name))
            :do (return form)))))


(defmethod system-definitions ((project project))
  (let ((plain (plain-system-definitions project)))
    (when plain
      `(progn ,@plain))))


(defun plain-system-definitions (project)
  (loop :with template = (system-template project)
        :for form :in (raw-system-definitions project)
        :collect (merge-system-form template form)))

(defun merge-system-form (template form)
  "Returns a new define-system form with the options and superclaess from template and form merged
\(with the forms from template appearing first\)."
  `(define-system ,(second form) ,(remove-duplicates (append (third template) (third form)))
     ,@(cdddr template)
     ,@(cdddr form)))


(defmethod raw-system-definitions (project)
  (loop :for dir :in (version-directories project)
        :for form = (read-form (project-sysdef-file project (last-dir-name dir)))
        :when form
        :collect (add-computed-options project dir form)))

(defun version-directories (project)
  (remove-if-not (lambda (x)
                   (and (directory-pathname-p x)
                        (sysdef::exact-version-spec-p (last-dir-name x))))
                 (list-directory (path-of project))))

(defun has-option (sysdef-form option)
  (get-option (cdddr sysdef-form) option))

(defun add-computed-options (project dir form)
  (merge-system-form
   `(template <name> ()
              ,@(unless (has-option form :version)
                  `((:version ,@(sysdef::coerce-to-version (last-dir-name dir)))))
              ,@(unless (has-option form :md5sum)
                  `((:md5sum ,(compute-md5sum dir project)))))
   form))


(defun compute-md5sum (path project)
  (let* ((version (sysdef::coerce-to-version (last-dir-name path)))
         (versionless-file (make-pathname :name (name-of project) :type "mb"
                                          :defaults path))
         (versioned-file (make-pathname :name (versioned-name project version)
                                        :type "mb"
                                        :defaults path)))
    (cond ((probe-file versioned-file) (md5sum-file versioned-file))
          ((probe-file versionless-file) (md5sum-file versionless-file))
          (t (error "Unable to determine a suitable Mudball file which contains ~S in ~A." project path)))))



;;;;;;
;;; CREATION OF THE SYSTEM DEFINITION FILE
(defun create-sysdef-code (root url contact)
  (with-output-to-string (output)
    (let ((projects (collect-projects root))
          (*package* (find-package :sysdef-user)))
      (format output "~&~S~&~%~%" *package-form*)
      (add-default-headers root output)
      (terpri output)
      (add-support-files projects output)
      (terpri output)
      (format output "(sysdef:with-provider (:url ~S :contact ~S)~&~%" url contact)
      (add-projects projects output) 
      (format output ")~&"))))
  

(defun collect-projects (root)
  (let ((project-paths (all-projects root)))
    (loop :for path :in project-paths
          collect (make-instance 'project :path path))))

(defun add-default-headers (root output)
  (when-let (support-file (probe-file (support-file root)))
    (with-open-file (support support-file :element-type (stream-element-type output))
      (copy-stream support output))))


(defun add-projects (projects output)
  (dolist (project projects)
    (when-let (definitions (system-definitions project))
      (format output "~&~%")
      (pprint definitions output)
      (format output "~%~%" ))))

(defun add-support-files (projects output)
  (dolist (project projects)
    (when-let (support-file (probe-file (project-support-file project)))
      (with-open-file (stream support-file)
        (copy-stream stream output)))))


(define-condition invalid-sysdef-file (warning)
  ((error :reader error-of :initarg :error)))

(defgeneric valid-sysdef-file-p (stream &key errorp)
  
  (:method ((string string) &key (errorp nil))
   (with-input-from-string (input string)
     (valid-sysdef-file-p input :errorp errorp)))
  
  (:method ((stream stream) &key (errorp nil))
   (let ((sysdef::*systems* nil))
     (handler-bind ((error (lambda (c)
                             (unless errorp
                               (warn 'invalid-sysdef-file :error c)
                               (return-from valid-sysdef-file-p nil)))))
       (load stream)
       t))))

(defun release-sysdef-file (output url contact &key (errorp nil))
  (ensure-directories-exist output)
  (let ((new-sysdef-code (create-sysdef-code *root-directory* url contact)))
    (when (valid-sysdef-file-p new-sysdef-code :errorp errorp)
      (with-open-file (outs (make-pathname :version :newest :defaults output)
                            :direction :output :if-exists :rename)
        (write-string new-sysdef-code outs)
        t))))


(defun create-mudballs-release (host url-dir-list contact &key (root *default-pathname-defaults*))
  "Creates a mudballs directory in ROOT from the latest core systems."
  (create-mudball-directory-structure root)
  (copy-default-files root)
  (dolist (core (core-systems))
    (copy-in-system root core))
  (create-sysdef-file root host url-dir-list contact))

(defun extract-tarball (tarball target-dir)
  (gzip-stream:with-open-gzip-file (ins tarball)
    (archive:with-open-archive (archive ins)
      (let ((*default-pathname-defaults* (pathname target-dir)))
        (archive:do-archive-entries (entry archive) 
          (archive:extract-entry archive entry))))))

;(mudballs-presenter::create-mudballs-release "mudballs.com" '("official") "sross@mudballs.com" :root "~/tmp/")

(defun create-sysdef-file (root host url-dir-list contact)
  (release-sysdef-file (merge-pathnames (make-pathname :directory (list* :relative host url-dir-list) :name "mudballs" :type "lisp")
                                        (relative-directory root "mudballs" "system-definitions"))
                       (format nil "http://~A/~A" host (namestring (make-pathname :directory (cons :relative url-dir-list))))
                       contact
                       :errorp t))

(defun latest-version (system-name)
  (first (mb.sysdef:systems-for system-name)))
  
(defun copy-in-system (root system-name)
  (let* ((system (latest-version system-name))
         (system-root (merge-pathnames (make-pathname :directory (list :relative (string-downcase (sysdef:name-of system)) (sysdef::version-string system)))
                                       (relative-directory root "mudballs" "systems")))
         (mudball-file (project-mudball-file (make-instance 'project :path (project-path (string-downcase system-name)))
                                             (sysdef::version-string system))))
    (assert (probe-file mudball-file))
    (ensure-directories-exist system-root)
    (format t "extract ~A to ~A~%" mudball-file system-root)
    (extract-tarball mudball-file system-root)))

(defun core-system-mixins  ()
  (remove-duplicates (mapcar 'sysdef:name-of (sysdef:systems-matching #'(lambda (x) (typep x 'sysdef-user::core-system-mixin))))))

(defun core-systems ()
  (let ((sysdef::*in-find-system* t))
    (labels ((all-dependencies (sys)
               (remove-duplicates (mapcar 'sysdef:component-of (sysdef::needs-of (sysdef:find-system sys)))))

             (process-one (system)
               (cons system (mapcan #'process-one (all-dependencies system)))))
    
      (delete-duplicates (mapcan #'process-one (core-system-mixins))))))

(defun relative-directory (root &rest names)
  (merge-pathnames (make-pathname :directory `(:relative ,@names))
                   root))

(defun create-mudball-directory-structure (root)
  (ensure-directories-exist (relative-directory root "mudballs"))
  (ensure-directories-exist (relative-directory root "mudballs" "documentation"))
  (ensure-directories-exist (relative-directory root "mudballs" "system-definitions"))
  (ensure-directories-exist (relative-directory root "mudballs" "systems"))
  (ensure-directories-exist (relative-directory root "mudballs" "tmp"))
  (ensure-directories-exist (relative-directory root "mudballs" "utils")))
   

(defun copy-default-files (to)
  (copy-file (merge-pathnames (make-pathname :directory '(:relative "files") :name "boot" :type "lisp")
                              *root-directory*)
             (make-pathname :name "boot" :type "lisp"
                            :defaults (relative-directory to "mudballs")))
  (copy-directory
   (relative-directory *root-directory* "files" "mb-artifacts")
   (relative-directory to "mudballs" "documentation" "mb-artifacts"))

  (copy-directory
   (relative-directory *root-directory* "files" "utils")
   (relative-directory to "mudballs" "utils")))


(defun copy-directory (from to)
  (walk-directory from
                  #'(lambda (x)
                      (let ((destination (merge-pathnames (enough-namestring x (truename from))
                                                          to)))
                        (ensure-directories-exist destination)
                        (copy-file x destination)))))

;(release-sysdef-file "/tmp/sysdef.lisp"  "http://mudballs.com/official/" "sean@mudballs.com" :errorp t)


#|
(defparameter *code* (print (create-sysdef-code "~/work/mudballs/" "http://mudballs.com/" "sean@mudballs.com"))
|#

;; EOF
