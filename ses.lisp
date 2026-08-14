;;; -*- Mode: lisp; coding: utf-8 -*-

(in-package "JRM-CODE-PROJECT")

(defun aws-ses-endpoint ()
  "Returns the AWS SES SMTP host, port, username, and password from environment variables or defaults."
  (values
   (or (uiop:getenv "AWS_SES_ENDPOINT") "email-smtp.us-east-1.amazonaws.com")
   (or (uiop:getenv "AWS_SES_PORT") "587")
   (or (uiop:getenv "AWS_SES_USERNAME") "")
   (or (uiop:getenv "AWS_SES_PASSWORD") "")))

(defun read-smtp-response (stream)
  "Reads an SMTP response, handling multi-line replies."
  (let ((acc nil))
    (do ((line (read-line stream nil nil) (read-line stream nil nil)))
        ((null line) (nreverse acc))
      (push line acc)
      (when (and (>= (length line) 4) (char= (char line 3) #\Space))
        (return (nreverse acc))))))

(defun read-smtp-response-raw (stream)
  "Reads an SMTP response from a byte stream using read-byte, returning the string."
  (let ((result (make-string-output-stream)))
    (do () (nil) ;; Infinite loop, we explicitly break out below
      (let ((line (make-string-output-stream)))
        
        ;; Read a single line terminated by CRLF or EOF
        (do ((b (read-byte stream nil nil) (read-byte stream nil nil)))
            ((or (null b)
                 (and (char= (code-char b) #\Return)
                      (eql (peek-char nil stream nil nil) #\Linefeed))))
          (write-char (code-char b) line))
        
        ;; Consume the Linefeed
        (when (eql (peek-char nil stream nil nil) #\Linefeed)
          (read-byte stream))
        
        ;; Process the completed line
        (let ((line-str (get-output-stream-string line)))
          (write-string line-str result)
          (write-char #\Newline result)
          
          ;; Check if it's the final line (4th char is a space)
          (when (and (>= (length line-str) 4) 
                     (char= (char line-str 3) #\Space))
            (return (get-output-stream-string result))))))))

(defun send-raw-smtp (stream command-string)
  "Sends a string as bytes with CRLF appended."
  (map nil (lambda (char)
             (write-byte (char-code char) stream))
       command-string)
  (write-byte (char-code #\Return) stream)
  (write-byte (char-code #\Linefeed) stream)
  (force-output stream))

(defun connect-to-ses (host port)
  ;; Open purely as bytes
  (let* ((socket (usocket:socket-connect host port :element-type '(unsigned-byte 8)))
         (raw-stream (usocket:socket-stream socket)))
    
    ;; 1. Read greeting
    (read-smtp-response-raw raw-stream)
    
    ;; 2. Send EHLO
    (send-raw-smtp raw-stream "EHLO jrm-code-project.com")
    (read-smtp-response-raw raw-stream)
    
    ;; 3. Send STARTTLS
    (send-raw-smtp raw-stream "STARTTLS")
    (read-smtp-response-raw raw-stream)
    
    ;; Force flush before handing to OpenSSL
    (force-output raw-stream)
    
    ;; 4. Wrap in TLS
    (let* ((tls-stream (cl+ssl:make-ssl-client-stream 
                        raw-stream 
                        :hostname host))
           ;; 5. Now it's safe to use flexi-streams for the actual email payload
           (tls-flex (flexi-streams:make-flexi-stream tls-stream :external-format :utf-8)))
      
      ;; 6. Re-introduce over secure channel
      (format tls-flex "EHLO jrm-code-project.com~C~C" #\Return #\Linefeed)
      (force-output tls-flex)
      (read-smtp-response tls-flex) ;; We can use the original character-based read-smtp-response here
      
      tls-flex)))

(defun send-email (to subject body)
  (multiple-value-bind (smtp-host smtp-port smtp-user smtp-pass) (aws-ses-endpoint)
    (let* ((sender "[REDACTED_NO_REPLY_EMAIL]")
           
           ;; OS-conditional path resolution
           (openssl-path #+win32 "C:\\Program Files\\Git\\usr\\bin\\openssl.exe"
                         #-win32 "openssl")
           
           ;; Spawn the openssl process
           (process (sb-ext:run-program openssl-path 
                                        (list "s_client" 
                                              "-starttls" "smtp" 
                                              "-crlf" 
                                              "-quiet" 
                                              "-connect" (format nil "~A:~A" smtp-host smtp-port))
                                        :input :stream 
                                        :output :stream 
                                        :wait nil 
                                        :search (not (member :win32 *features*))))) 
      ;; Note: :search t is usually required on Linux to find 'openssl' in $PATH.
      ;; We turn it off for Windows since we provide an absolute path.
      
      (unwind-protect
           (let ((in (sb-ext:process-input process))
                 (out (sb-ext:process-output process)))
            
             (flet ((send (cmd)
                      (write-line cmd in)
                      (force-output in))

                    (get-resp ()
                      (do ((line (read-line out nil nil) (read-line out nil nil)))
                          ((null line))
                        (format t "=> ~A~%" line)
                        (when (and (>= (length line) 4) (char= (char line 3) #\Space))
                          (return)))))
              
               (sleep 1)
              
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
               (get-resp)))
        
        (when process 
          (sb-ext:process-close process))))))
