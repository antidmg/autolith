(in-package #:autolith)

;;;; -- Headless Primary-Agent Boundary Tests --

(defvar *headless-primary-test-value* nil
  "The active-image binding changed by the primary boundary regression test.")

(-> headless-primary-tests--request-form
    (&key (:expected-next-sequence (integer 1)) (:prompt string))
    list)
(defun headless-primary-tests--request-form
    (&key (expected-next-sequence 1) (prompt "Improve the active image."))
  "Return one valid primary job form for tests."
  (list :autolith-primary-job
        :version 1
        :id "primary-job-1"
        :conversation-id "realm-primary-proof"
        :expected-next-sequence expected-next-sequence
        :prompt prompt
        :timeout-seconds 30))

(-> headless-primary-tests--installed-set-record-p (t) boolean)
(defun headless-primary-tests--installed-set-record-p (record)
  "Return true when RECORD installs this test's self.set mutation."
  (and (listp record)
       (eq (first record) :mutation)
       (eq (getf (rest record) :kind) :set)
       (eq (getf (rest record) :result) :installed)
       (string= (getf (rest record) :target)
                "*HEADLESS-PRIMARY-TEST-VALUE*")))

(-> run-headless-primary-tests () null)
(defun run-headless-primary-tests ()
  "Test validation, CLI exposure, primary self mutation, and sequence fencing."
  (let ((request
          (primary-job-validate-envelope
           (headless-primary-tests--request-form))))
    (test-assert
     (and
      (string= (primary-job-request-identifier request) "primary-job-1")
      (string= (primary-job-request-conversation-identifier request)
               "realm-primary-proof")
      (= (primary-job-request-expected-next-sequence request) 1))
     "the primary boundary validates its complete version-one envelope"))
  (dolist (form
           (list
            (list :autolith-primary-job
                  :version 1
                  :id "primary-job-1"
                  :conversation-id "realm-primary-proof"
                  :expected-next-sequence 0
                  :prompt "work"
                  :timeout-seconds 30)
            (append (headless-primary-tests--request-form)
                    (list :unexpected t))))
    (test-assert
     (handler-case
         (progn (primary-job-validate-envelope form) nil)
       (run-job-error () t))
     "the primary boundary rejects invalid sequence cursors and unknown fields"))
  (test-assert
   (string= (command-name (main--run-primary-job-command))
            "run-primary-job")
   "the CLI exposes the explicit headless primary-agent command")
  (with-test-configuration (configuration)
    (let* ((provider
             (make-instance
              'scripted-provider
              :configuration configuration
              :results
              (list
               (agent-test-result
                "primary-self-set"
                (list
                 (agent-test-call
                  :call-id "primary-self-set-call"
                  :namespace "self"
                  :name "set"
                  :arguments
                  "{\"symbol\":\"*headless-primary-test-value*\",\"value\":\"42\"}")))
               (agent-test-result
                "primary-self-set-complete"
                (list (agent-test-message "The active image changed."))))))
           (request
             (primary-job-validate-envelope
              (headless-primary-tests--request-form)))
           (status nil)
           (evidence nil)
           (usage nil))
      (unwind-protect
           (test-call-with-function-replacements
            (list
             (list
              'provider-create
              (lambda (active-configuration &key reasoning-summaries-p)
                (declare (ignore active-configuration reasoning-summaries-p))
                provider)))
            (lambda ()
              (multiple-value-setq (status evidence usage)
                (primary-job-execute-with-application
                 configuration request :full-access))
              (test-assert
               (and
                (eq status :succeeded)
                (= *headless-primary-test-value* 42)
                (string= (getf evidence :conversation-id)
                         "realm-primary-proof")
                (> (getf evidence :next-sequence)
                   (getf evidence :start-sequence))
                (= (getf usage :provider-requests) 2)
                (find-if
                 #'headless-primary-tests--installed-set-record-p
                 (getf evidence :records)))
               "the headless boundary runs the primary agent with self.set intact")
              (multiple-value-bind
                    (repeat-status repeat-evidence repeat-usage
                     repeat-category repeat-message)
                  (primary-job-execute-with-application
                   configuration request :full-access)
                (declare (ignore repeat-usage repeat-message))
                (test-assert
                 (and
                  (eq repeat-status :failed)
                  (eq repeat-category :sequence-conflict)
                  (> (getf repeat-evidence :next-sequence)
                     (primary-job-request-expected-next-sequence request))
                  (= *headless-primary-test-value* 42))
                 "the expected sequence rejects a duplicate primary turn"))))
        (setf *headless-primary-test-value* nil))))
  nil)
