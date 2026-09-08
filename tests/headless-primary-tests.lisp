(in-package #:autolith)

;;;; -- Headless Primary-Agent Boundary Tests --

(defvar *headless-primary-test-value* nil
  "The active-image binding changed by the primary boundary regression test.")

(-> headless-primary-tests--request-form
    (&key (:conversation-id string)
          (:expected-next-sequence integer)
          (:expected-generation (option string))
          (:prompt string)
          (:reasoning-effort string)
          (:maximum-output-tokens (integer 1))
          (:maximum-provider-requests (integer 1))
          (:tool-allowlist list))
    list)
(defun headless-primary-tests--request-form
    (&key (conversation-id "realm-primary-proof")
          (expected-next-sequence 1)
          expected-generation
          (prompt "Improve the active image.")
          (reasoning-effort "low")
          (maximum-output-tokens 1024)
          (maximum-provider-requests 4)
          (tool-allowlist '("self.set")))
  "Return one valid version-two primary job form for tests."
  (list :autolith-primary-job
        :version 2
        :id "primary-job-1"
        :conversation-id conversation-id
        :expected-next-sequence expected-next-sequence
        :expected-generation expected-generation
        :prompt prompt
        :reasoning-effort reasoning-effort
        :maximum-output-tokens maximum-output-tokens
        :maximum-provider-requests maximum-provider-requests
        :tool-allowlist tool-allowlist
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
      (= (primary-job-request-expected-next-sequence request) 1)
      (null (primary-job-request-expected-generation request))
      (string= (primary-job-request-reasoning-effort request) "low")
      (= (primary-job-request-maximum-output-tokens request) 1024)
      (= (primary-job-request-maximum-provider-requests request) 4)
      (equal (primary-job-request-tool-allowlist request)
             '("self.set")))
     "the primary boundary validates its complete version-two envelope"))
  (dolist (form
           (list
            (list :autolith-primary-job
                  :version 1
                  :id "primary-job-1"
                  :conversation-id "realm-primary-proof"
                  :expected-next-sequence 1
                  :prompt "work"
                  :timeout-seconds 30)
            (headless-primary-tests--request-form
             :expected-next-sequence 0)
            (headless-primary-tests--request-form
             :tool-allowlist '("self.set" "SELF.SET"))
            (append (headless-primary-tests--request-form)
                    (list :unexpected t))))
    (test-assert
     (handler-case
         (progn (primary-job-validate-envelope form) nil)
       (run-job-error ()
         t))
     "the primary boundary rejects stale versions, invalid cursors, duplicate tools, and unknown fields"))
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
              (let ((*image-state-initialized-p* nil)
                    (*active-image-commit-identifier* nil)
                    (*active-image-history-commit* nil)
                    (*active-image-lineage-identifier* nil))
                (multiple-value-bind
                      (mismatch-status mismatch-evidence mismatch-usage
                       mismatch-category mismatch-message)
                    (primary-job-execute-with-application
                     configuration
                     (primary-job-validate-envelope
                      (headless-primary-tests--request-form
                       :conversation-id "generation-mismatch"
                       :expected-generation "missing-generation"))
                     :full-access)
                  (declare
                   (ignore mismatch-evidence mismatch-message))
                  (test-assert
                   (and
                    (eq mismatch-status :failed)
                    (eq mismatch-category :generation-mismatch)
                    (zerop (getf mismatch-usage :provider-requests)))
                   "generation mismatch fails before a provider request"))
                (multiple-value-bind
                      (tool-status tool-evidence tool-usage
                       tool-category tool-message)
                    (primary-job-execute-with-application
                     configuration
                     (primary-job-validate-envelope
                      (headless-primary-tests--request-form
                       :conversation-id "missing-tool"
                       :tool-allowlist '("missing.tool")))
                     :full-access)
                  (declare (ignore tool-evidence tool-message))
                  (test-assert
                   (and
                    (eq tool-status :failed)
                    (eq tool-category :preflight-failure)
                    (zerop (getf tool-usage :provider-requests)))
                   "an unavailable exact tool fails before a provider request"))
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
                  (equal (getf evidence :generation)
                         '(:expected nil :loaded nil :recovery-used-p nil))
                  (equal (getf evidence :tool-allowlist)
                         '("self.set"))
                  (= (getf (getf evidence :limits)
                           :maximum-output-tokens)
                     1024)
                  (= (getf usage :provider-requests) 2)
                  (find-if
                   #'headless-primary-tests--installed-set-record-p
                   (getf evidence :records)))
                 "the bounded primary agent runs an allowed self.set")
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
                   "the expected sequence rejects a duplicate primary turn")))))
        (setf *headless-primary-test-value* nil))))
  nil)
