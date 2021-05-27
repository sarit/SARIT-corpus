":"; exec emacs -Q --no-site-file --script "$0" -- "$@" # -*-emacs-lisp-*-

;;; sarit-preflight.el --- Helpers for checking the SARIT library

;;; Commentary:

;; This is a collection of emacs-lisp functions to help with managing
;; the SARIT library of e-texts
;; (https://github.com/sarit/sarit-corpus).  Currently, it allows you
;; to:

;; - Build the schema for SARIT: ‘sp-make-schema’
;; - Build ODD->RNG for the TEI P5: ‘sp-clean-and-build-tei-p5’
;; - Validate SARIT’s XML documents: ‘sp-rnc-validate-xml-docs’ (uses jing)

;; If you’re not working with Emacs, you can call this file from the
;; command line to perform a set of checks of the SARIT library.
;; Simply run ./tools/bin/sarit-preflight.el from the base directory
;; (where most of the XML files are).

;; Further resources:

;; http://www.tei-c.org/guidelines/p5/using-the-tei-github-repository/
;; ODD overview: http://tei.oucs.ox.ac.uk/Talks/2014-10-odds/talk-04-tagdocs.xml
;; Official (?) ODD intro: http://www.tei-c.org/guidelines/customization/getting-started-with-p5-odds/

(require 'xmltok)
(require 'cl-lib)

;;; Code:

(defcustom sarit-default-dir default-directory
  "The base directory for the SARIT library.

This should be where you cloned the git repository to."
  :group 'sarit-preflight
  :type 'directory)

;; (setq sarit-default-dir (expand-file-name "../../" default-directory))


(defcustom sarit-corpus "saritcorpus.xml"
  "The main teiCorpus file is, relative to ‘sarit-default-dir’."
  :group 'sarit-preflight
  :type 'file)

(defcustom sarit-odd "schemas/odd/sarit.odd"
  "SARIT’s ODD file, relative to ‘sarit-default-dir’."
  :group 'sarit-preflight
  :type 'file)

(defcustom sarit-rnc "schemas/sarit.rnc"
  "SARIT’s RNC file, relative to ‘sarit-default-dir’."
  :group 'sarit-preflight
  :type 'file)

(defcustom sarit-rng "schemas/sarit.rng"
  "SARIT’s RNG file, relative to ‘sarit-default-dir’."
  :group 'sarit-preflight
  :type 'file)

(defcustom sarit-tei-p5-makefile "tools/TEI/P5/Makefile"
  "Makefile of the TEI (P5) Guidelines, relative to ‘sarit-default-dir’."
  :group 'sarit-preflight
  :type 'file)


;; Helper functions

(defun sarit-log-buffer ()
  "Return a log buffer, perhaps creating it."
  (get-buffer-create "* sarit preflight logs *"))

(defun sp-error-and-show-log (message &rest params)
  "Raise an error MESSAGE and show the log.

PARAMS can be anything you want to send along."
  (if noninteractive
      (apply
       #'error
       ;; (string-join `(,message "\n\nDump of current logs" "\n\n%s") "::")
       (mapconcat 'identity `(,message "\n\nDump of current logs" "\n\n%s") "::")
       `(,@params
         ,(with-current-buffer (sarit-log-buffer)
            (or (buffer-string) "[[ end of log ]]"))))
    (with-current-buffer  (sarit-log-buffer)
      (goto-char (point-max))
      (pop-to-buffer (current-buffer))
      (apply #'error message params))))

(defun sp-process-output-filter (proc string)
  "A filter for process PROC to take care of STRING (usually a message)."
  (if noninteractive
      (princ string)
    (when (buffer-live-p (process-buffer proc))
      (with-current-buffer (process-buffer proc)
        (let ((moving (= (point) (process-mark proc))))
          (save-excursion
            ;; Insert the text, advancing the process marker.
            (goto-char (process-mark proc))
            (insert string)
            (set-marker (process-mark proc) (point)))
          (if moving (goto-char (process-mark proc))))))))

(defun sp-process-sentinel-print-out (proc event-type)
  "Listener for process PROC for events of EVENT-TYPE.

The actual EVENT-TYPE is ignored, this sentinel just emits the
message for every status change."
  (message "Status of process “%s”: %s" (process-name proc) event-type))

(defun sp-wait-till-finished (process)
  "Wait until PROCESS has finished, and then complain."
  (while (not (member (process-status process) '(end failed exit) ))
    (sleep-for 1))
  (if (= 0  (process-exit-status process))
      (message "Process “%s” completed without apparent problems" process)
    (error "Process “%s” did not go well, stopping here" process)))

;; Main functions:

(defun sp-check-and-fix-set-up! (&optional verbose)
  "Make sure that all expected files are there.

When VERBOSE is set, list the settings for all configuration directives."
  (when (and
         (file-exists-p sarit-default-dir)
         (file-directory-p sarit-default-dir))
    (let ((default-directory (or sarit-default-dir
				 (progn
				   (warn "sarit-default-dir not set, using current directory")
				   default-directory)))
          (to-check-and-set '(sarit-corpus
                              sarit-odd
			      sarit-tei-p5-makefile)))
      (mapc
       (lambda (x)
         (or
          (and
           (symbol-value x)
           (file-exists-p (expand-file-name (symbol-value x)))
           (file-readable-p (expand-file-name (symbol-value x)))
	   (progn
	     (when verbose
               (message "	- Setting %s to %s" x (expand-file-name (symbol-value x))))
	     (setf x (expand-file-name (symbol-value x)))))
          (error "Could not access file (config option: %s): %s" x (symbol-value x))))
       to-check-and-set)
      ;; some other tests
      (mapc
       (lambda (file)
         (if (file-exists-p (expand-file-name file default-directory))
             (when verbose
               (message "	- File %s found in expected location: %s"
                        file
                        (expand-file-name file default-directory) ))
           (error  "File %s not found in expected location: %s"
                   file
                   (expand-file-name file default-directory))))
       '("tools/TEI/P5/Makefile"))
      (message "\n\n*Setup looks good!*\n\n"))))

(defun sp-p5-start-make (target p5-dir stylesheets-dir prefix-dir)
  "Return a process for ‘make TARGET’ in P5-DIR.

Override STYLESHEETS-DIR and PREFIX-DIR in Makefile."
  (let ((default-directory  p5-dir)
        process)
    (message "Current directory is %s" default-directory)
    (message "Makefile is present: %s" (file-exists-p (expand-file-name "Makefile")))
    (with-temp-buffer
      (call-process "pwd" nil t nil)
      (princ (buffer-string)))
    (setf process
          (start-process
           "sp-p5-make-clean"
           (sarit-log-buffer)
           "make"
           (format "%s" target)
           (format "XSL=%s" stylesheets-dir)
           (format "PREFIX=%s" prefix-dir)))
    (set-process-sentinel process #'sp-process-sentinel-print-out)
    (set-process-filter process #'sp-process-output-filter)
    (unless noninteractive
      (pop-to-buffer (process-buffer process)))
    process))


(defun sp-clean-and-build-tei-p5 ()
  "Build ODD->RNG for the TEI P5.

You need to let-bind ‘sarit-default-dir’ correctly for this
function to work properly."
  (let* ((default-directory
           (file-name-as-directory
            (expand-file-name "tools/TEI/P5" sarit-default-dir)))
         (tei-stylesheet-dir
          (file-name-as-directory
           (expand-file-name "tools/TEI-Stylesheets" sarit-default-dir)))
         (prefix-dir temporary-file-directory)
         (log-buffer (sarit-log-buffer))
         process)
    (mapc
     (lambda (x)
       (message "*** making target “%s”" x)
       (setf process (sp-p5-start-make x default-directory tei-stylesheet-dir prefix-dir))
       (while  (not (member (process-status process) '(end failed exit)))
         (sleep-for 2))
       (or (= 0 (process-exit-status process))
	   (sp-error-and-show-log "Make %s failed for P5" x)))
     '("clean"
       "schemas"
       ;;; This was failing (2019-09-20, for P5_Release_3.5.0)
       ;; "validate"
       ))))

(defun sp-make-schema (&optional odd-file)
  "Produce an RNG and RNC schema from ODD-FILE (default ‘sarit-odd’)."
  (let* ((odd-file (expand-file-name (or odd-file sarit-odd) sarit-default-dir))
         (teitornc-script
          (expand-file-name "tools/TEI-Stylesheets/bin/teitornc" sarit-default-dir))
         (teip5 (expand-file-name "tools/TEI/P5/p5.xml" sarit-default-dir))
         (output-rnc (expand-file-name
                      (format "%s../%s.rnc"
                              (file-name-directory odd-file)
                              (file-name-sans-extension (file-name-nondirectory odd-file)))))
         (output-rng (expand-file-name
                      (format "%s../%s.rng"
                              (file-name-directory odd-file)
                              (file-name-sans-extension (file-name-nondirectory odd-file)))))
         process)
    (mapc
     (lambda (x)
       (unless (file-readable-p x)
         (error "Can’t read %s" teitornc-script)))
     `(,teitornc-script
       ,teip5))
    (message "Converting %s to RNC" odd-file)
    (setf process
          (start-process
           "odd->rnc"
           (sarit-log-buffer)
           "bash"
           teitornc-script
           (format "--localsource=%s" teip5)
           "--odd"
           odd-file
           output-rnc))
    (set-process-sentinel process #'sp-process-sentinel-print-out)
    (set-process-filter process #'sp-process-output-filter)
    (unless noninteractive
      (pop-to-buffer (process-buffer process)))
    (while  (not (member (process-status process) '(end failed exit)))
      (sleep-for 1))
    (unless (file-readable-p output-rnc)
      (sp-error-and-show-log "Could not generate %s" output-rnc))
    (message "Converting %s to RNG" output-rnc)
    (setf process
          (start-process
           "rnc->rng"
           (sarit-log-buffer)
           "java"
           "-jar"
           (expand-file-name "tools/TEI-Stylesheets/lib/trang.jar" sarit-default-dir)
           output-rnc
           output-rng))
    (set-process-sentinel process #'sp-process-sentinel-print-out)
    (set-process-filter process #'sp-process-output-filter)
    (unless noninteractive
      (pop-to-buffer (process-buffer process)))
    (while  (not (member (process-status process) '(end failed exit)))
      (sleep-for 1))
    (message "Converted %s to %s" output-rnc output-rng)
    `(,output-rnc
      ,output-rng)))

(defun sp-find-xml-docs-for-validation (&optional sarit-corpus-file)
  "Get al list of xi:include/@href in SARIT-CORPUS-FILE (default ‘sarit-corpus’)."
  (let (docs)
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name
        (or sarit-corpus-file sarit-corpus)
        sarit-default-dir))
      (goto-char (point-min))
      (while (xmltok-forward)
        (when (and
               xmltok-attributes
               (member xmltok-type '(start-tag empty-element))
               (string-match-p "^xi.*:include$" (xmltok-start-tag-qname)))
          (mapc
           (lambda (att-set)
             (when (string= "href" (xmltok-attribute-local-name att-set))
               (mapc
                (lambda (x) (unless (null x) (push (expand-file-name x sarit-default-dir) docs)))
                (split-string (xmltok-attribute-value att-set)))))
           xmltok-attributes)))
      (nreverse docs))))

;; (sp-find-xml-docs-for-validation)

(defun sp-sarit-corpus-with-xinclude (&optional sarit-corpus-file)
  "Process xinclude directives in SARIT-CORPUS-FILE.

Returns name of file containing the resulting document."
  (let ((sarit-all (expand-file-name (or sarit-corpus-file sarit-corpus) sarit-default-dir))
        (output (make-temp-file "sp-all-docs-" nil ".xml")))
    (with-temp-file output
      (unless (= 0 (call-process
                    "xmllint"
                    nil
                    t
                    nil
                    "--encode"
                    "UTF-8"
                    "--xinclude"
                    sarit-all))
        (error "Could not xmllint %s" sarit-all)))
    output))

;; (sp-find-xml-docs-for-validation)

(defun sp-rnc-validate-xml-docs (&optional schema xml-docs)
  "Use SCHEMA (default ‘sarit-rnc’) to validate XML-DOCS."
  (let ((schema (expand-file-name
                 (or schema sarit-rnc)
                 sarit-default-dir))
        (xml-docs (or xml-docs (sp-find-xml-docs-for-validation)))
        process)
    (setf process
          (apply
           #'start-process
           `("parallel validation"
             ,(sarit-log-buffer)
             "parallel"
             "--verbose"
             "--halt"
             "now,fail=1"
             "java"
             "-jar"
             ,(expand-file-name "./tools/TEI/P5/Utilities/lib/jing.jar" sarit-default-dir)
             "-c"
             ,(expand-file-name schema sarit-default-dir)
             ":::"
             ,@xml-docs)))
    (set-process-sentinel process #'sp-process-sentinel-print-out)
    (set-process-filter process #'sp-process-output-filter)
    (unless noninteractive
      (pop-to-buffer (process-buffer process)))
    process))

;; (sp-rnc-validate-xml-docs)


(defun sp-find-missing-xml-encoding-declaration (xml-docs)
  "Return list of those XML-DOCS that don’t an encoding declaration."
  (cl-delete-if
   #'null
   (mapcar
    (lambda (doc)
      (with-temp-buffer
        (insert-file-contents doc nil 0 2000)
        (goto-char (point-min))
        ;; go to first processing instruction
        (let ((encoding (xmltok-get-declared-encoding-position)))
          (unless (and (not (null encoding))
                       (listp encoding)
                       (string= "UTF-8" (buffer-substring-no-properties (car encoding) (cdr encoding))))
            doc))))
    xml-docs)))


(when noninteractive
  (message "Command line arguments are: %s" argv)
  (cond
   ((<= 2 (length argv))
    (message "Usage: sarit-preflight.el [-h|--help]\n")
    (message "    This script builds the TEI P5 distro, generates the
    SARIT schemas, and validates SARIT’s XML documents against
    the resulting RNC schema.  This file can also be used from
    within Emacs, see the comments at the beginning of the
    file.")
    (message "\nYour current configuration looks like this:\n")
    (sp-check-and-fix-set-up! 'verbose)
    (kill-emacs 0))
   (t
    (let* ((sarit-default-dir
	    (progn (message "sarit-default-dir set to %s"
                            (file-name-as-directory
                             (expand-file-name default-directory)))
		   (file-name-as-directory (expand-file-name default-directory))))
           (corp (expand-file-name sarit-corpus sarit-default-dir))
           (schema (expand-file-name "schemas/sarit.rnc" sarit-default-dir))
           (docs-for-validation
            (sp-find-xml-docs-for-validation corp))
           (missing-encoding (sp-find-missing-xml-encoding-declaration docs-for-validation))
           sarit-all-file
           process)
      (sp-check-and-fix-set-up! 'verbose)
      (if (null missing-encoding)
          (message "Checked encoding declarations, looks good")
        (error "Encoding declaration missing here: %s" missing-encoding))
      (sp-clean-and-build-tei-p5)
      (sp-make-schema  (expand-file-name sarit-odd sarit-default-dir))
      (message "Validating %s docs:" (length docs-for-validation))
      (mapc
       (lambda (doc)
         (message "- %s" doc))
       docs-for-validation)
      (sp-wait-till-finished
       (sp-rnc-validate-xml-docs
        schema
        docs-for-validation))
      (setf sarit-all-file (sp-sarit-corpus-with-xinclude corp))
      (sp-wait-till-finished
       (sp-rnc-validate-xml-docs schema (list sarit-all-file)))
      (delete-file sarit-all-file)))))

(provide 'sarit-preflight)
;;; sarit-preflight.el ends here
