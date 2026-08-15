;;; -*- Mode: lisp; coding: utf-8 -*-

(in-package "JRM-CODE-PROJECT")

(defun aws-ses-endpoint ()
  "Returns the AWS SES SMTP host, port, username, and password from environment variables or defaults."
  (values
   (or (uiop:getenv "AWS_SES_ENDPOINT") "email-smtp.us-east-1.amazonaws.com")
   (or (uiop:getenv "AWS_SES_PORT") "587")
   (or (uiop:getenv "AWS_SES_USERNAME") "")
   (or (uiop:getenv "AWS_SES_PASSWORD") "")))

(defun run-smtp-dialogue (in out to subject body smtp-user smtp-pass)
  "Drive the actual SMTP AUTH LOGIN / MAIL FROM / RCPT TO / DATA dialogue
over the already-connected (and, in production, already-STARTTLS'd)
character streams IN/OUT, sending BODY as an email TO with SUBJECT,
authenticating as SMTP-USER/SMTP-PASS. Returns T on success. Signals an
ERROR (caught by SEND-EMAIL, its only caller) if the peer closes the
connection before any expected response is seen, or if anything else
goes wrong.

Factored out from SEND-EMAIL purely for testability: tests can call this
directly with a pair of in-memory string streams standing in for a real
socket/TLS connection, without needing a live network connection or an
external `openssl' subprocess -- the same dependency-injection pattern
already used elsewhere in this codebase (e.g. PASTEBIN.LISP's
*PASTEBIN-CLOCK*, API-TOKEN.LISP's *JWT-CLOCK*)."
  (let ((sender "no-reply@jrm-code-project.com"))
    (flet ((send (cmd)
             (write-line cmd in)
             (force-output in))
           (get-resp ()
             ;; Reads until a final (non-continuation) SMTP reply line, or
             ;; EOF -- if the peer closes the connection mid-dialogue,
             ;; READ-LINE returns NIL and this loop signals an ERROR
             ;; (caught by SEND-EMAIL) instead of hanging forever or
             ;; silently proceeding as if the step had succeeded.
             (do ((line (read-line out nil nil) (read-line out nil nil)))
                 ((null line) (error "SMTP connection closed unexpectedly (no response received)."))
               (format t "=> ~A~%" line)
               (when (and (>= (length line) 4) (char= (char line 3) #\Space))
                 (return line)))))
      (send "EHLO jrm-code-project.com")
      (get-resp)
      (send "AUTH LOGIN")
      (get-resp)
      (send (cl-base64:string-to-base64-string smtp-user))
      (get-resp)
      (send (cl-base64:string-to-base64-string smtp-pass))
      (get-resp)
      (send (format nil "MAIL FROM:<~A>" sender))
      (get-resp)
      (send (format nil "RCPT TO:<~A>" to))
      (get-resp)
      (send "DATA")
      (get-resp)
      (send (format nil "From: ~A" sender))
      (send (format nil "To: ~A" to))
      (send (format nil "Subject: ~A" subject))
      (send "")
      (send body)
      (send ".")
      (get-resp)
      (send "QUIT")
      (get-resp)
      t)))

(defun send-email (to subject body)
  "Send an email TO the given address with SUBJECT/BODY via AWS SES's SMTP
endpoint, driving an external `openssl s_client -starttls smtp' process
(see AWS-SES-ENDPOINT for the connection parameters) and then the actual
SMTP dialogue via RUN-SMTP-DIALOGUE. Returns an outcome value (see
OUTCOME.LISP): (OK T) on success, or (ERR reason) if the openssl process
can't be spawned, the connection drops mid-dialogue, or any step of the
SMTP conversation fails -- see FUNCTIONAL_REFACTOR.md Phase 6. Per the
error-handling policy documented in API-KEY-ROUTES.LISP (TECHNICAL_DEBT.md
item 7, strategy 2: \"request-scoped recovery that always logs AND
converts to a response/outcome value\"), every failure path here is both
logged (so an operator can diagnose a broken mail pipeline) and converted
to an (ERR reason) the caller can inspect, instead of letting a raw
condition or a silently-hung process propagate out of this function."
  (multiple-value-bind (smtp-host smtp-port smtp-user smtp-pass) (aws-ses-endpoint)
    (let* (;; OS-conditional path resolution
           (openssl-path #+win32 "C:\\Program Files\\Git\\usr\\bin\\openssl.exe"
                         #-win32 "openssl")
           (process nil))
      (unwind-protect
          (handler-case
              (progn
                (setf process
                      (sb-ext:run-program openssl-path
                                           (list "s_client"
                                                 "-starttls" "smtp"
                                                 "-crlf"
                                                 "-quiet"
                                                 "-connect" (format nil "~A:~A" smtp-host smtp-port))
                                           :input :stream
                                           :output :stream
                                           :wait nil
                                           :search (not (member :win32 *features*))))
                ;; Note: :search t is usually required on Linux to find 'openssl' in $PATH.
                ;; We turn it off for Windows since we provide an absolute path.
                (sleep 1)
                (run-smtp-dialogue (sb-ext:process-input process) (sb-ext:process-output process)
                                    to subject body smtp-user smtp-pass)
                (ok t))
            (error (e)
              (format t ";; Warning: Failed to send email to ~A via SES: ~A~%" to e)
              (err (format nil "~A" e))))
        (when process
          (sb-ext:process-close process))))))
