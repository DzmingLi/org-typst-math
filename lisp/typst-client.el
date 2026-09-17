;;; typst-client.el --- Persistent Typst conversion service -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "30.2"))
;; Keywords: processes, tex

;;; Commentary:
;; Shared stdio JSON-RPC client.  A connection is reused per document directory.
;; No Org or preview dependencies belong here.

;;; Code:
(require 'jsonrpc)
(require 'subr-x)

(defgroup typst-client nil "Persistent Typst conversion." :group 'applications)

(defcustom typst-client-command '("org-typst-math-helper")
  "Helper program and arguments, passed directly to `make-process'."
  :type '(repeat string))

(defcustom typst-client-timeout 30
  "Seconds to wait for a conversion."
  :type 'number)

(defvar typst-client--connections (make-hash-table :test #'equal))

(defun typst-client--connection (root)
  "Return the live connection for ROOT, starting one if necessary."
  (let* ((root (file-name-as-directory (file-truename root)))
         (connection (gethash root typst-client--connections)))
    (unless (and connection (jsonrpc-running-p connection))
      (let* ((default-directory root)
             (name (concat "typst-" (substring (md5 root) 0 8))))
        (setq connection
              (jsonrpc-process-connection
               :name name
               :process (make-process
                         :name name :command typst-client-command
                         :connection-type 'pipe :noquery t
                         :stderr (get-buffer-create (format "*%s stderr*" name)))))
        (condition-case err
            (let ((hello (jsonrpc-request connection :initialize
                                          '(:protocolVersion 1)
                                          :timeout typst-client-timeout)))
              (unless (eq (plist-get hello :protocolVersion) 1)
                (error "Unsupported Typst helper protocol"))
              (puthash root connection typst-client--connections))
          (error (jsonrpc-shutdown connection)
                 (signal (car err) (cdr err))))))
    connection))

(defun typst-client-convert (root preamble items &optional target)
  "Convert ITEMS to TARGET (default mathml) in ROOT with PREAMBLE.
ITEMS is a list of plists with :id, :source and JSON boolean :display.
Return the helper result, including per-formula diagnostics."
  (let ((connection (typst-client--connection root)))
    (condition-case err
        (jsonrpc-request connection :math/convert
                         (list :root (expand-file-name root)
                               :preamble preamble :items (vconcat items)
                               :target (or target "mathml"))
                         :timeout typst-client-timeout)
      (quit (jsonrpc-shutdown connection) (signal 'quit nil))
      (error (jsonrpc-shutdown connection) (signal (car err) (cdr err))))))

(defun typst-client-convert-async (root preamble items success error &optional target)
  "Convert ITEMS asynchronously; call SUCCESS or ERROR with the result.
ROOT, PREAMBLE and TARGET have the same meaning as `typst-client-convert'."
  (jsonrpc-async-request
   (typst-client--connection root) :math/convert
   (list :root (expand-file-name root) :preamble preamble :items (vconcat items)
         :target (or target "mathml"))
   :success-fn success :error-fn error :timeout typst-client-timeout
   :timeout-fn (lambda () (funcall error '(:message "Typst conversion timed out")))))

(defun typst-client-stop ()
  "Stop all helper connections; the next request starts them again."
  (interactive)
  (maphash (lambda (_ connection)
             (when (jsonrpc-running-p connection)
               (jsonrpc-notify connection :exit nil)
               (jsonrpc-shutdown connection)))
           typst-client--connections)
  (clrhash typst-client--connections))

(provide 'typst-client)
;;; typst-client.el ends here
