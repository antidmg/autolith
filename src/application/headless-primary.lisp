(in-package #:autolith)

;;;; -- Headless Primary-Agent Boundary --

(defparameter *primary-job-maximum-output-tokens* 65536
  "The largest per-request output ceiling accepted by the primary boundary.")

(defparameter *primary-job-maximum-provider-requests* 64
  "The largest provider-request budget accepted by the primary boundary.")

(defparameter *primary-job-maximum-tool-names* 64
  "The largest exact tool allowlist accepted by the primary boundary.")

(defclass primary-job-request nil
  ((identifier
    :initarg :identifier
    :reader primary-job-request-identifier
    :type non-empty-string
    :documentation "The caller's stable job identifier.")
   (conversation-identifier
    :initarg :conversation-identifier
    :reader primary-job-request-conversation-identifier
    :type non-empty-string
    :documentation "The stable primary conversation opened or created for this job.")
   (expected-next-sequence
    :initarg :expected-next-sequence
    :reader primary-job-request-expected-next-sequence
    :type (integer 1)
    :documentation "The exact next durable sequence required before the turn starts.")
   (expected-generation
    :initarg :expected-generation
    :reader primary-job-request-expected-generation
    :type (option non-empty-string)
    :documentation "The exact retained generation required, or NIL for clean source.")
   (prompt
    :initarg :prompt
    :reader primary-job-request-prompt
    :type non-empty-string
    :documentation "The primary agent's user instruction.")
   (reasoning-effort
    :initarg :reasoning-effort
    :reader primary-job-request-reasoning-effort
    :type non-empty-string
    :documentation "The exact reasoning effort selected for this job.")
   (maximum-output-tokens
    :initarg :maximum-output-tokens
    :reader primary-job-request-maximum-output-tokens
    :type (integer 1)
    :documentation "The output-token ceiling applied to every provider request.")
   (maximum-provider-requests
    :initarg :maximum-provider-requests
    :reader primary-job-request-maximum-provider-requests
    :type (integer 1)
    :documentation "The provider-request ceiling applied to the complete turn.")
   (tool-allowlist
    :initarg :tool-allowlist
    :reader primary-job-request-tool-allowlist
    :type list
    :documentation "The exact canonical tools visible and executable during the turn.")
   (timeout-seconds
    :initarg :timeout-seconds
    :reader primary-job-request-timeout-seconds
    :type (integer 1)
    :documentation "The cooperative deadline for the primary turn."))
  (:documentation "One validated version-two non-interactive primary-agent request."))

(-> primary-job--envelope-pairs (t) list)
(defun primary-job--envelope-pairs (form)
  "Validate FORM's version-two envelope shape and return field pairs."
  (unless (and (task--proper-list-p form)
               (eq (first form) :autolith-primary-job))
    (run-job--error
     ':invalid-envelope
     "The input must begin with :AUTOLITH-PRIMARY-JOB."))
  (handler-case
      (task--plist-alist
       (rest form)
       '(:version :id :conversation-id :expected-next-sequence
         :expected-generation :prompt :reasoning-effort
         :maximum-output-tokens :maximum-provider-requests
         :tool-allowlist :timeout-seconds)
       :source ':programmatic)
    (task-agent-definition-error (condition)
      (run-job--error ':invalid-envelope "~A" condition))))

(-> primary-job--tool-allowlist-p (t) boolean)
(defun primary-job--tool-allowlist-p (value)
  "Return true when VALUE is one bounded duplicate-free canonical-name list."
  (and (task--proper-list-p value)
       (plusp (length value))
       (<= (length value) *primary-job-maximum-tool-names*)
       (every (lambda (name)
                (and (non-empty-string-p name)
                     (<= (length name) 128)
                     (position #\. name)))
              value)
       (= (length value)
          (length (remove-duplicates value :test #'string-equal)))))

(-> primary-job-validate-envelope (t) primary-job-request)
(defun primary-job-validate-envelope (form)
  "Validate FORM without opening a conversation or provider."
  (let ((pairs (primary-job--envelope-pairs form)))
    (flet ((required (key)
             (multiple-value-bind (value present-p)
                 (task--alist-value key pairs)
               (unless present-p
                 (run-job--error
                  ':invalid-envelope "Required field ~S is missing." key))
               value)))
      (let ((version (required :version))
            (identifier (required :id))
            (conversation-identifier (required :conversation-id))
            (expected-next-sequence (required :expected-next-sequence))
            (expected-generation (required :expected-generation))
            (prompt (required :prompt))
            (reasoning-effort (required :reasoning-effort))
            (maximum-output-tokens (required :maximum-output-tokens))
            (maximum-provider-requests (required :maximum-provider-requests))
            (tool-allowlist (required :tool-allowlist))
            (timeout-seconds (required :timeout-seconds)))
        (unless (eql version 2)
          (run-job--error
           ':unsupported-version "Unsupported primary job version ~S." version))
        (unless (and (non-empty-string-p identifier)
                     (<= (length identifier) 256))
          (run-job--error
           ':invalid-envelope
           ":ID must be a non-empty string of at most 256 characters."))
        (unless (and (non-empty-string-p conversation-identifier)
                     (<= (length conversation-identifier) 256))
          (run-job--error
           ':invalid-envelope
           ":CONVERSATION-ID must be a non-empty string of at most 256 characters."))
        (unless (typep expected-next-sequence '(integer 1))
          (run-job--error
           ':invalid-envelope
           ":EXPECTED-NEXT-SEQUENCE must be a positive integer."))
        (unless (or (null expected-generation)
                    (and (non-empty-string-p expected-generation)
                         (<= (length expected-generation) 256)))
          (run-job--error
           ':invalid-envelope
           ":EXPECTED-GENERATION must be NIL or a bounded non-empty string."))
        (unless (and (non-empty-string-p prompt)
                     (<= (length prompt)
                         *task-agent-instructions-maximum-characters*))
          (run-job--error
           ':invalid-envelope ":PROMPT must be a bounded non-empty string."))
        (unless (and (non-empty-string-p reasoning-effort)
                     (member reasoning-effort
                             *supported-reasoning-efforts*
                             :test #'string=))
          (run-job--error
           ':invalid-envelope
           ":REASONING-EFFORT must name a supported reasoning level."))
        (unless (and (integerp maximum-output-tokens)
                     (<= 1 maximum-output-tokens
                         *primary-job-maximum-output-tokens*))
          (run-job--error
           ':invalid-envelope
           ":MAXIMUM-OUTPUT-TOKENS is outside the supported range."))
        (unless (and (integerp maximum-provider-requests)
                     (<= 1 maximum-provider-requests
                         *primary-job-maximum-provider-requests*))
          (run-job--error
           ':invalid-envelope
           ":MAXIMUM-PROVIDER-REQUESTS is outside the supported range."))
        (unless (primary-job--tool-allowlist-p tool-allowlist)
          (run-job--error
           ':invalid-envelope
           ":TOOL-ALLOWLIST must contain bounded unique canonical tool names."))
        (unless (and (integerp timeout-seconds)
                     (<= 1 timeout-seconds
                         *run-job-maximum-timeout-seconds*))
          (run-job--error
           ':invalid-envelope
           ":TIMEOUT-SECONDS is outside the supported range."))
        (make-instance
         'primary-job-request
         :identifier identifier
         :conversation-identifier conversation-identifier
         :expected-next-sequence expected-next-sequence
         :expected-generation expected-generation
         :prompt prompt
         :reasoning-effort reasoning-effort
         :maximum-output-tokens maximum-output-tokens
         :maximum-provider-requests maximum-provider-requests
         :tool-allowlist (mapcar #'string-downcase tool-allowlist)
         :timeout-seconds timeout-seconds)))))

(-> primary-job--recover-identifier (t) string)
(defun primary-job--recover-identifier (form)
  "Return FORM's unique bounded identifier, or an empty string when unavailable."
  (if (and (task--proper-list-p form)
           (eq (first form) :autolith-primary-job))
      (let ((identifiers nil))
        (loop for tail = (rest form) then (cddr tail)
              while (and (consp tail) (consp (rest tail)))
              when (and (eq (first tail) :id)
                        (non-empty-string-p (second tail))
                        (<= (length (second tail)) 256))
                do (push (second tail) identifiers))
        (if (= (length identifiers) 1)
            (first identifiers)
            ""))
      ""))

(-> primary-job--effective-configuration
    (configuration primary-job-request)
    configuration)
(defun primary-job--effective-configuration (configuration request)
  "Return CONFIGURATION with REQUEST's bounded inference selection."
  (handler-case
      (configuration-with-reasoning-effort
       configuration
       (primary-job-request-reasoning-effort request))
    (configuration-error (condition)
      (run-job--error ':preflight-failure "~A" condition))))

(-> primary-job--loaded-generation
    (configuration)
    (option non-empty-string))
(defun primary-job--loaded-generation (configuration)
  "Return the retained generation represented by the running image."
  (handler-case
      (let ((commit (image-commit-current configuration)))
        (and commit (image-commit-identifier commit)))
    (image-commit-error (condition)
      (run-job--error ':generation-mismatch "~A" condition))))

(-> primary-job--preflight
    (application primary-job-request (option non-empty-string) boolean)
    null)
(defun primary-job--preflight
    (application request loaded-generation recovery-used-p)
  "Validate generation, prompt extension, and exact tools before model input."
  (when recovery-used-p
    (run-job--error
     ':generation-mismatch
     "A fail-closed primary job cannot run through recovery fallback."))
  (unless (equal loaded-generation
                 (primary-job-request-expected-generation request))
    (run-job--error
     ':generation-mismatch
     "Expected retained generation ~S, but the running image is ~S."
     (primary-job-request-expected-generation request)
     loaded-generation))
  (let* ((registry (application-tool-registry application))
         (available (tool-registry-canonical-names registry))
         (missing
           (remove-if
            (lambda (name)
              (member name available :test #'string=))
            (primary-job-request-tool-allowlist request))))
    (when missing
      (run-job--error
       ':preflight-failure
       "The primary job requested unavailable tools: ~{~A~^, ~}."
       missing)))
  (system-prompt (application-configuration application))
  nil)

(-> primary-job--usage-field (t string) (integer 0))
(defun primary-job--usage-field (usage name)
  "Return nonnegative integer NAME from portable USAGE, defaulting to zero."
  (let ((entry (and (listp usage) (assoc name usage :test #'string=))))
    (if (and entry (typep (second entry) '(integer 0)))
        (second entry)
        0)))

(-> primary-job--usage (list) list)
(defun primary-job--usage (events)
  "Aggregate provider request usage from headless observer EVENTS."
  (let ((input-tokens 0)
        (output-tokens 0)
        (provider-requests 0))
    (dolist (event events)
      (when (eq (getf event :status) :provider-request-completed)
        (let ((usage (getf (getf event :details) :usage)))
          (incf input-tokens
                (primary-job--usage-field usage "input_tokens"))
          (incf output-tokens
                (primary-job--usage-field usage "output_tokens"))
          (incf provider-requests))))
    (list :input-tokens input-tokens
          :output-tokens output-tokens
          :provider-requests provider-requests)))

(-> primary-job--mutation-evidence (configuration (integer 0)) list)
(defun primary-job--mutation-evidence (configuration start-count)
  "Return journal bounds and records appended after START-COUNT."
  (handler-case
      (let* ((records (mutation-journal-read-records configuration))
             (end-count (length records))
             (replaced-p (< end-count start-count)))
        (list :journal-start-count start-count
              :journal-end-count end-count
              :journal-replaced-p replaced-p
              :records (unless replaced-p (nthcdr start-count records))))
    (error (condition)
      (list :journal-start-count start-count
            :journal-end-count nil
            :journal-replaced-p nil
            :records nil
            :read-failure
            (bounded-string condition :limit *run-job-failure-message-limit*)))))

(-> primary-job--evidence
    ((option application) primary-job-request (option (integer 1)) list
     (integer 0) (option non-empty-string) boolean
     &key (:result (option provider-result)))
    list)
(defun primary-job--evidence
    (application request start-sequence events journal-start-count
     loaded-generation recovery-used-p &key result)
  "Return durable conversation, limits, generation, and mutation evidence."
  (let* ((conversation
           (and application
                (slot-boundp application 'conversation)
                (application-conversation application)))
         (configuration
           (and application
                (slot-boundp application 'configuration)
                (application-configuration application))))
    (append
     (when conversation
       (list :conversation-id (conversation-identifier conversation)
             :start-sequence start-sequence
             :next-sequence (conversation-next-sequence conversation)))
     (when result
       (list :response-id (provider-result-response-id result)
             :text (provider-result-assistant-text result)))
     (list
      :generation
      (list :expected (primary-job-request-expected-generation request)
            :loaded loaded-generation
            :recovery-used-p recovery-used-p)
      :limits
      (list :reasoning-effort
            (primary-job-request-reasoning-effort request)
            :maximum-output-tokens
            (primary-job-request-maximum-output-tokens request)
            :maximum-provider-requests
            (primary-job-request-maximum-provider-requests request)
            :timeout-seconds
            (primary-job-request-timeout-seconds request))
      :tool-allowlist
      (copy-list (primary-job-request-tool-allowlist request))
      :events (reverse events))
     (if configuration
         (primary-job--mutation-evidence configuration journal-start-count)
         (list :journal-start-count journal-start-count
               :journal-end-count nil
               :journal-replaced-p nil
               :records nil)))))

(-> primary-job--condition-category (error) keyword)
(defun primary-job--condition-category (condition)
  "Return the stable primary-job failure category for CONDITION."
  (cond
    ((typep condition 'run-job-error)
     (run-job-error-category condition))
    ((typep condition 'provider-error)
     ':provider-failure)
    ((typep condition 'agent-loop-error)
     ':agent-loop)
    (t
     ':primary-failure)))

(-> primary-job-execute-with-application
    (configuration primary-job-request keyword)
    (values keyword list list (option keyword) (option string)))
(defun primary-job-execute-with-application
    (configuration request permission-mode)
  "Execute REQUEST through one preflighted and bounded primary agent turn."
  (let ((application nil)
        (events nil)
        (journal-start-count 0)
        (start-sequence nil)
        (last-request-number nil)
        (loaded-generation nil)
        (recovery-used-p nil)
        (result nil))
    (labels ((observe-status (status details)
               "Retain one portable status event and return the output ceiling."
               (push (list :status status :details details) events)
               (let ((request-number (getf details :request-number)))
                 (when (typep request-number '(integer 1))
                   (setf last-request-number request-number)))
               (and (eq status :provider-request-started)
                    (primary-job-request-maximum-output-tokens request)))
             (evidence ()
               "Return the evidence accumulated before the current boundary."
               (primary-job--evidence
                application request start-sequence events journal-start-count
                loaded-generation recovery-used-p :result result))
             (abort-turn (reason condition)
               "Persist an idempotent turn-aborted boundary when a turn started."
               (when (and application start-sequence)
                 (ignore-errors
                   (conversation-append-turn-aborted
                    (application-conversation application)
                    :turn-start-sequence start-sequence
                    :reason reason
                    :condition-type (write-to-string (type-of condition))
                    :message (princ-to-string condition)
                    :request-number last-request-number)))))
      (unwind-protect
           (handler-case
               (progn
                 (setf configuration
                       (primary-job--effective-configuration
                        configuration request))
                 (setf application
                       (application-create
                        configuration
                        :conversation-id
                        (primary-job-request-conversation-identifier request)
                        :ensure-conversation-p t
                        :permission-mode permission-mode))
                 (setf recovery-used-p
                       (non-empty-string-p (uiop:getenv "AUTOLITH_RECOVERED"))
                       loaded-generation
                       (primary-job--loaded-generation
                        (application-configuration application)))
                 (primary-job--preflight
                  application request loaded-generation recovery-used-p)
                 (setf journal-start-count
                       (length
                        (mutation-journal-read-records
                         (application-configuration application))))
                 (let ((conversation (application-conversation application)))
                   (setf start-sequence
                         (conversation-next-sequence conversation))
                   (unless (= start-sequence
                              (primary-job-request-expected-next-sequence request))
                     (run-job--error
                      ':sequence-conflict
                      "Conversation ~A expected next sequence ~D but is at ~D."
                      (conversation-identifier conversation)
                      (primary-job-request-expected-next-sequence request)
                      start-sequence)))
                 (let ((observer
                         (callback-agent-observer-create
                          :status-callback #'observe-status
                          :command-authorization-callback
                          (run-job-headless-command-authorization-for-instructions
                           application permission-mode
                           (primary-job-request-prompt request))
                          :tool-authorization-callback
                          (run-job-headless-tool-authorization permission-mode))))
                   (let ((*agent-maximum-provider-requests-per-turn*
                           (primary-job-request-maximum-provider-requests
                            request)))
                     (setf result
                           (sb-ext:with-timeout
                               (primary-job-request-timeout-seconds request)
                             (agent-run-user-turn
                              (application-agent application)
                              (primary-job-request-prompt request)
                              :observer observer
                              :tool-allowlist
                              (primary-job-request-tool-allowlist request)
                              :tool-restriction-p t)))))
                 (values ':succeeded (evidence)
                         (primary-job--usage events) nil nil))
             (sb-ext:timeout (condition)
               (abort-turn ':cancelled condition)
               (values ':timed-out (evidence)
                       (primary-job--usage events)
                       ':timeout
                       "The primary job exceeded its wall-clock timeout."))
             (error (condition)
               (abort-turn
                (if (typep condition 'agent-loop-error)
                    ':agent-loop
                    ':application-error)
                condition)
               (values ':failed (evidence)
                       (primary-job--usage events)
                       (primary-job--condition-category condition)
                       (princ-to-string condition))))
        (run-job--close-application application)))))

(-> primary-job-result-envelope
    (string keyword
     &key (:started-at integer) (:finished-at integer)
          (:evidence list) (:usage list)
          (:category (option keyword)) (:message (option string)))
    list)
(defun primary-job-result-envelope
    (identifier status
     &key evidence usage category message started-at finished-at)
  "Return one version-two terminal primary-job result envelope."
  (append
   (list :autolith-primary-job-result
         :version 2
         :id identifier
         :status status
         :evidence evidence)
   (unless (eq status :succeeded)
     (list :failure
           (list :category (or category :unknown)
                 :message
                 (bounded-string
                  (or message "The primary job failed.")
                  :limit *run-job-failure-message-limit*))))
   (list :usage
         (or usage
             '(:input-tokens 0 :output-tokens 0 :provider-requests 0))
         :started-at (run-job--timestamp started-at)
         :finished-at (run-job--timestamp finished-at))))

(-> primary-job-run
    ((or pathname string) (or pathname string) keyword
     &key (:executor function) (:configuration (option configuration)))
    integer)
(defun primary-job-run
    (input-path output-path permission-mode
     &key (executor #'primary-job-execute-with-application) configuration)
  "Run one primary job, atomically write its terminal result, and return an exit code."
  (let ((started-at (get-universal-time))
        (identifier "")
        (form nil))
    (flet ((write-failure (category condition)
             (ignore-errors
               (run-job-write-result-atomically
                output-path
                (primary-job-result-envelope
                 identifier ':failed
                 :started-at started-at
                 :finished-at (get-universal-time)
                 :evidence nil
                 :category category
                 :message (princ-to-string condition))))))
      (handler-case
          (let* ((read-form (run-job-read-file input-path))
                 (request nil))
            (setf form read-form
                  identifier (primary-job--recover-identifier form)
                  request (primary-job-validate-envelope form)
                  identifier (primary-job-request-identifier request))
            (multiple-value-bind (status evidence usage category message)
                (funcall
                 executor
                 (or configuration
                     (configuration-create :defer-provider-validation-p t))
                 request
                 permission-mode)
              (run-job-write-result-atomically
               output-path
               (primary-job-result-envelope
                identifier status
                :started-at started-at
                :finished-at (get-universal-time)
                :evidence evidence
                :usage usage
                :category category
                :message message))
              (if (eq status :succeeded) 0 1)))
        (run-job-error (condition)
          (write-failure (run-job-error-category condition) condition)
          64)
        (error (condition)
          (write-failure ':process-failure condition)
          1)
        (serious-condition (condition)
          (write-failure ':process-failure condition)
          1)))))
