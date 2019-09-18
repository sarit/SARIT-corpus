#! /bin/bash
":"; exec emacs -Q --no-site-file --script "$0" -- "$@" # -*-emacs-lisp-*-


;; Some useful resources:

;; http://www.tei-c.org/guidelines/p5/using-the-tei-github-repository/
;; ODD overview: http://tei.oucs.ox.ac.uk/Talks/2014-10-odds/talk-04-tagdocs.xml
;; Official (?) ODD intro: http://www.tei-c.org/guidelines/customization/getting-started-with-p5-odds/

(require 'xmltok)
(require 'cl-lib)

;; A set of functions that should ease maintenance of the SARIT
;; library.

(defcustom sarit-default-dir nil
  "The base directory for the SARIT library.  This should be
  where you cloned the git repository to."
  :group 'sarit-preflight
  :type 'dir)



(defvar sarit-corpus "saritcorpus.xml"
  "Set name of the main teiCorpus file, relative to ‘sarit-default-dir’.")

(defvar sarit-odd "schemas/odd/sarit.odd"
  "SARIT’s ODD file, relative to ‘sarit-default-dir’.")

(defvar sarit-tei-p5-makefile "tools/TEI/P5/Makefile"
  "Makefile of the TEI (P5) Guidelines, relative to ‘sarit-default-dir’.")


;; Helper functions

(defun sarit-log-buffer ()
  (get-buffer-create "* sarit preflight logs *"))

(defun sp-error-and-show-log (message &rest params)
  (if noninteractive
      (apply
       #'error
       ;; (string-join `(,message "\n\nDump of current logs" "\n\n%s") "::")
       (mapconcat 'identity `(,message "\n\nDump of current logs" "\n\n%s") "::")
       `(,@params
         ,(with-current-buffer (sarit-log-buffer)
            (or (buffer-string) "[[ end of log ]]"))))
    (pop-to-buffer (sarit-log-buffer))
    (apply #'error message params)))

(defun sp-process-output-filter (proc string)
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
  "Make sure that all expected files are there."
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
	   (setf x (expand-file-name (symbol-value x))))
          (error "Could not access file (config option: %s): %s" x (symbol-value x))))
       to-check-and-set)
      (when verbose
	(mapc
	 (lambda (x)
	   (message "Variable %s set to %s" x (expand-file-name (symbol-value x))))
	 to-check-and-set))
      (message "Set up looks good"))))

(defun sp-p5-start-make (target p5-dir stylesheets-dir prefix-dir)
  "Return a process for ‘make TARGET’ in P5-DIR.

Override STYLESHEETS-DIR and PREFIX-DIR in Makefile."
  (let ((default-directory  p5-dir)
        process)
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
  "Build ODD->RNG for the TEI P5."
  (let* ((default-directory (expand-file-name "tools/TEI/P5" sarit-default-dir))
         (tei-stylesheet-dir (expand-file-name "tools/TEI-Stylesheets" sarit-default-dir))
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
     '("clean" "validate"))))

(defun sp-make-schema (odd-file)
  "Produce an RNG and RNC schema from ODD-FILE."
  (let* ((odd-file (expand-file-name odd-file sarit-default-dir))
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

(defun sp-find-xml-docs-for-validation (sarit-corpus-file)
  "Get al list of xi:include/@href in SARIT-CORPUS-FILE."
  (let (docs)
    (with-temp-buffer
      (insert-file-contents (expand-file-name sarit-corpus-file sarit-default-dir))
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

(defun sp-sarit-corpus-with-xinclude (sarit-corpus-file)
  "Process xinclude directives in SARIT-CORPUS-FILE. Returns
  name of file containing the resulting document."
  (let ((sarit-all (expand-file-name sarit-corpus-file sarit-default-dir))
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

(defun sp-rnc-validate-xml-docs (schema xml-docs)
  "Use SCHEMA to validate XML-DOCS."
  (let (process)
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
  (let* ((sarit-default-dir
	  (progn (message "sarit-default-dir set to %s" (expand-file-name default-directory))
		 (expand-file-name default-directory)))
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
    (message "Validating %s docs: %s" (length docs-for-validation) docs-for-validation)
    (sp-wait-till-finished
     (sp-rnc-validate-xml-docs
      schema
      docs-for-validation))
    (setf sarit-all-file (sp-sarit-corpus-with-xinclude corp))
    (sp-wait-till-finished
     (sp-rnc-validate-xml-docs schema (list sarit-all-file)))
    (delete-file sarit-all-file)))

(provide 'sarit-preflight)
