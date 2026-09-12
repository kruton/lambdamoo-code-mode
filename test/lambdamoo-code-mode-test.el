;;; lambdamoo-code-mode-test.el --- Tests for lambdamoo-code-mode.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'lambdamoo-code-mode)

(defmacro lambdamoo-code-test--with-server (binding &rest body)
  "Bind BINDING to a dispatch-capable Eglot server while running BODY."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((process (make-pipe-process :name "lambdamoo-code-test" :noquery t)))
     (unwind-protect
         (let ((,binding (make-instance 'lambdamoo-code-server
                                        :name "lambdamoo-code-test"
                                        :process process)))
           ,@body)
       (when (process-live-p process)
         (delete-process process)))))

(defconst lambdamoo-code-test--propfind-response
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<D:multistatus xmlns:D=\"DAV:\" xmlns:M=\"urn:moo:webdav\">
  <D:response>
    <D:href>/dav/owned/525/verb/</D:href>
    <D:propstat><D:prop><D:resourcetype><D:collection/></D:resourcetype></D:prop></D:propstat>
  </D:response>
  <D:response>
    <D:href>/dav/owned/525/verb/subdir/</D:href>
    <D:propstat><D:prop><D:resourcetype><D:collection/></D:resourcetype></D:prop></D:propstat>
  </D:response>
  <D:response>
    <D:href>https://example.com/dav/owned/525/verb/check%5Fauthorization</D:href>
    <D:propstat><D:prop>
      <M:owner>#267</M:owner>
      <M:permissions>rxd</M:permissions>
      <M:names>check_authorization auth_check</M:names>
      <M:arguments>{\"this\", \"none\", \"this\"}</M:arguments>
    </D:prop></D:propstat>
  </D:response>
  <D:response>
    <D:href>/outside/not-a-child</D:href>
    <D:propstat><D:prop/></D:propstat>
  </D:response>
  <D:response>
    <D:href>https://attacker.example/dav/owned/525/verb/not-a-child</D:href>
    <D:propstat><D:prop/></D:propstat>
  </D:response>
  <D:response>
    <D:href>https://[invalid/dav/owned/525/verb/not-a-child</D:href>
    <D:propstat><D:prop/></D:propstat>
  </D:response>
</D:multistatus>"
  "Representative LambdaMOO WebDAV directory response.")

(ert-deftest lambdamoo-code-parses-and-maps-uris ()
  (let ((lambdamoo-code-connections
         '(("waterpoint" . "https://waterpoint.example/dav/"))))
    (should (equal (lambdamoo-code--parse-uri
                    "moo://waterpoint/object/18/verb/explode")
                   '("waterpoint" . "object/18/verb/explode")))
    (should (equal (lambdamoo-code--http-url
                    "moo://waterpoint/object/18/verb/explode")
                   "https://waterpoint.example/dav/object/18/verb/explode"))))

(ert-deftest lambdamoo-code-builds-verb-uri-from-simpleedit-reference ()
  (should
   (equal (lambdamoo-code-verb-uri
           "waterpoint" "#123:my_verb this none this")
          "moo://waterpoint/object/123/verb/my_verb"))
  (should-error (lambdamoo-code-verb-uri "waterpoint" "not-a-reference")
                :type 'user-error)
  (should-error (lambdamoo-code-verb-uri "bad/authority" "#123:my_verb")
                :type 'user-error))

(ert-deftest lambdamoo-code-opens-verb-from-simpleedit-reference ()
  (let (opened)
    (cl-letf (((symbol-function 'find-file)
               (lambda (filename &rest _)
                 (setq opened filename))))
      (lambdamoo-code-open-verb
       "waterpoint" "#123:my_verb this none this")
      (should (equal opened
                     "moo://waterpoint/object/123/verb/my_verb")))))

(ert-deftest lambdamoo-code-decodes-crlf-response-body ()
  (should (equal (lambdamoo-code--decode-response-body
                  (encode-coding-string "first\r\nsecond\r\n" 'utf-8))
                 "first\nsecond\n")))

(ert-deftest lambdamoo-code-authentication-uses-endpoint-host ()
  (let ((lambdamoo-code-connections
         '(("waterpoint" . "https://dav.example:8443/dav/")))
        searches)
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest spec)
                 (push spec searches)
                 (when (equal (plist-get spec :host) "dav.example")
                   '((:user "wizard" :secret "swordfish"))))))
      (should
       (equal (lambdamoo-code--authentication-header
               "moo://waterpoint/owned/")
              (cons "Authorization"
                    (concat "Basic "
                            (base64-encode-string "wizard:swordfish" t)))))
      (should (equal (mapcar (lambda (spec) (plist-get spec :host))
                             (nreverse searches))
                     '("dav.example:8443" "dav.example")))
      (should (equal (plist-get (car searches) :port) "https")))))

(ert-deftest lambdamoo-code-authentication-falls-back-to-host-and-port ()
  (let ((lambdamoo-code-connections
         '(("waterpoint" . "https://dav.example/dav/")))
        searched-hosts)
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest spec)
                 (let ((host (plist-get spec :host)))
                   (push host searched-hosts)
                   (when (equal host "dav.example:443")
                     '((:user "wizard" :secret (lambda () "secret"))))))))
      (should (lambdamoo-code--authentication-header
               "moo://waterpoint/owned/"))
      (should (equal searched-hosts '("dav.example:443"))))))

(ert-deftest lambdamoo-code-lists-webdav-directory-with-moo-properties ()
  (let ((lambdamoo-code-connections
         '(("waterpoint" . "https://example.com/dav/")))
        request)
    (cl-letf (((symbol-function 'lambdamoo-code--request)
               (lambda (uri &rest args)
                 (setq request (cons uri args))
                 lambdamoo-code-test--propfind-response)))
      (let ((entries
             (lambdamoo-code-list-directory
              "moo://waterpoint/owned/525/verb/")))
        (should (equal request
                       '("moo://waterpoint/owned/525/verb/"
                         :method "PROPFIND" :headers (("Depth" . "1")))))
        (should (equal (mapcar (lambda (entry) (plist-get entry :name)) entries)
                       '("subdir/" "check_authorization")))
        (should (plist-get (car entries) :directory))
        (let ((verb (cadr entries)))
          (should (equal (plist-get verb :uri)
                         "moo://waterpoint/owned/525/verb/check_authorization"))
          (should (equal (plist-get verb :owner) "#267"))
          (should (equal (plist-get verb :permissions) "rxd"))
          (should (equal (plist-get verb :names)
                         "check_authorization auth_check"))
          (should (equal (plist-get verb :arguments)
                         "{\"this\", \"none\", \"this\"}")))))))

(ert-deftest lambdamoo-code-rejects-malformed-propfind-response ()
  (let ((lambdamoo-code-connections
         '(("waterpoint" . "https://example.com/dav/"))))
    (cl-letf (((symbol-function 'lambdamoo-code--request)
               (lambda (&rest _) "<not-multistatus/>")))
      (should-error
       (lambdamoo-code-list-directory "moo://waterpoint/object/")
       :type 'lambdamoo-code-file-error))))

(ert-deftest lambdamoo-code-directory-operations-and-dired-rendering ()
  (let ((entries
         '((:name "look" :uri "moo://waterpoint/object/1/verb/look"
            :directory nil :owner "#1" :permissions "rxd" :names "look"
            :arguments "{this, none, this}")
           (:name "tools/" :uri "moo://waterpoint/object/1/verb/tools/"
            :directory t :owner "#2" :permissions "r" :names "utilities"
            :arguments ""))))
    (cl-letf (((symbol-function 'lambdamoo-code-list-directory)
               (lambda (_) entries)))
      (should (equal (directory-files "moo://waterpoint/object/1/verb/")
                     '("." ".." "look" "tools/")))
      (should (equal
               (mapcar #'car
                       (directory-files-and-attributes
                        "moo://waterpoint/object/1/verb/" nil "\\`[lt]"))
               '("look" "tools/")))
      (should (equal
               (file-name-all-completions
                "to" "moo://waterpoint/object/1/verb/")
               '("tools/")))
      (with-temp-buffer
        (lambdamoo-code--insert-directory
         "moo://waterpoint/object/1/verb/" "-al" nil t)
        (should (string-match-p "#1[ ]+rxd.*{this, none, this}.*look"
                                (buffer-string)))
        (should (string-match-p "#2[ ]+r.*(utilities).*tools/"
                                (buffer-string)))
        (goto-char (point-min))
        (search-forward "look")
        (should (get-text-property (1- (point)) 'dired-filename))))))

(ert-deftest lambdamoo-code-opens-directory-in-dired ()
  (require 'dired)
  (cl-letf (((symbol-function 'lambdamoo-code-list-directory)
             (lambda (_)
               '((:name "look" :uri "moo://waterpoint/object/1/verb/look"
                  :directory nil :owner "#1" :permissions "rxd"
                  :names "look" :arguments "{this, none, this}")))))
    (let ((buffer (dired-noselect "moo://waterpoint/object/1/verb/")))
      (unwind-protect
          (with-current-buffer buffer
            (should (derived-mode-p 'dired-mode))
            (goto-char (point-min))
            (search-forward "look")
            (should (equal (dired-get-filename 'no-dir t) "look")))
        (kill-buffer buffer)))))

(ert-deftest lambdamoo-code-find-file-recognizes-normalized-directory ()
  (require 'dired)
  (let ((uri "moo://waterpoint/owned/"))
    (should (file-directory-p uri))
    (should (file-directory-p (directory-file-name uri)))
    (cl-letf (((symbol-function 'lambdamoo-code-list-directory)
               (lambda (_) nil)))
      (let ((buffer (find-file-noselect uri)))
        (unwind-protect
            (with-current-buffer buffer
              (should (derived-mode-p 'dired-mode)))
          (kill-buffer buffer))))))

(ert-deftest lambdamoo-code-mode-uses-local-workspace-directory ()
  (let ((lambdamoo-code-workspace-directory temporary-file-directory)
        (lambdamoo-code-mode-hook nil))
    (with-temp-buffer
      (setq default-directory "moo://waterpoint/object/18/verb/")
      (lambdamoo-code-mode)
      (should (equal default-directory
                     (file-name-as-directory
                      (expand-file-name temporary-file-directory)))))))

(ert-deftest lambdamoo-code-mode-falls-back-from-missing-workspace-directory ()
  (let ((lambdamoo-code-workspace-directory
         (expand-file-name "lambdamoo-code-does-not-exist" temporary-file-directory))
        (lambdamoo-code-mode-hook nil))
    (with-temp-buffer
      (lambdamoo-code-mode)
      (should (equal default-directory
                     (file-name-as-directory temporary-file-directory))))))

(ert-deftest lambdamoo-code-mode-is-a-programming-mode ()
  (let ((lambdamoo-code-mode-hook nil))
    (with-temp-buffer
      (lambdamoo-code-mode)
      (should (derived-mode-p 'prog-mode))
      (should (eq indent-line-function #'lambdamoo-code-indent-line))
      (should (local-variable-p 'auto-save-visited-predicate))
      (should-not (funcall auto-save-visited-predicate)))))

(ert-deftest lambdamoo-code-indents-block-statements ()
  (let ((lambdamoo-code-mode-hook nil))
    (with-temp-buffer
      (insert "if (ready)\nreturn 1;\nelse\nreturn 0;\nendif\n")
      (lambdamoo-code-mode)
      (indent-region (point-min) (point-max))
      (should (equal (buffer-string)
                     "if (ready)\n  return 1;\nelse\n  return 0;\nendif\n")))))

(ert-deftest lambdamoo-code-fontifies-language-constructs ()
  (let ((lambdamoo-code-mode-hook nil))
    (with-temp-buffer
      (insert "if (#-42 == target)\nreturn E_NONE;\nendif\n")
      (lambdamoo-code-mode)
      (font-lock-ensure)
      (goto-char (point-min))
      (should (eq (get-text-property (point) 'face) 'font-lock-keyword-face))
      (search-forward "#-42")
      (should (eq (get-text-property (1- (point)) 'face)
                  'font-lock-constant-face))
      (search-forward "E_NONE")
      (should (eq (get-text-property (1- (point)) 'face)
                  'font-lock-constant-face)))))

(ert-deftest lambdamoo-code-eglot-setup-enables-semantic-tokens-when-managed ()
  (let (eglot-started semantic-tokens-enabled font-lock-flushed
                      font-lock-ensured
                      (managed t))
    (with-temp-buffer
      (setq-local eglot-managed-mode-hook nil)
      (cl-letf (((symbol-function 'eglot-ensure)
                 (lambda () (setq eglot-started t)))
                ((symbol-function 'eglot-managed-p)
                 (lambda () managed))
                ((symbol-function 'eglot-semantic-tokens-mode)
                 (lambda (value)
                   (setq eglot-semantic-tokens-mode (> value 0))
                   (push value semantic-tokens-enabled)))
                ((symbol-function 'font-lock-flush)
                 (lambda (&rest _) (setq font-lock-flushed t)))
                ((symbol-function 'font-lock-ensure)
                 (lambda (&rest _) (setq font-lock-ensured t))))
        (lambdamoo-code--eglot-setup)
        (should eglot-started)
        (should-not semantic-tokens-enabled)
        (run-hooks 'eglot-managed-mode-hook)
        (should (equal semantic-tokens-enabled '(1)))
        (should font-lock-flushed)
        (should font-lock-ensured)
        (setq managed nil)
        (run-hooks 'eglot-managed-mode-hook)
        (should (equal semantic-tokens-enabled '(-1 1)))))))

(ert-deftest lambdamoo-code-file-probes-surface-remote-errors ()
  (cl-letf (((symbol-function 'lambdamoo-code-read-uri)
             (lambda (uri)
               (signal 'lambdamoo-code-file-error
                       (list "WebDAV GET returned HTTP 404" uri)))))
    (let ((error-data
           (should-error
            (file-exists-p "moo://waterpoint/object/999999/verb/missing")
            :type 'lambdamoo-code-file-error)))
      (should (string-match-p "HTTP 404"
                              (error-message-string error-data)))
      (should (string-match-p "moo://waterpoint"
                              (error-message-string error-data))))))

(ert-deftest lambdamoo-code-ignores-auxiliary-file-probes ()
  (cl-letf (((symbol-function 'lambdamoo-code-read-uri)
             (lambda (_) (ert-fail "Auxiliary probe performed WebDAV GET"))))
    (should-not
     (file-exists-p
      "moo://waterpoint/object/18/verb/RCS/explode,v"))
    (should-not
     (file-exists-p
      "moo://waterpoint/object/18/verb/explode,v"))))

(ert-deftest lambdamoo-code-rejects-direct-auxiliary-file-reads-locally ()
  (cl-letf (((symbol-function 'lambdamoo-code--request-bytes)
             (lambda (&rest _) (ert-fail "Auxiliary read performed WebDAV GET"))))
    (with-temp-buffer
      (should-error
       (insert-file-contents
        "moo://waterpoint/object/267/verb/RCS/sha1")
       :type 'file-missing)
      (should-error
       (insert-file-contents
        "moo://waterpoint/object/267/verb/sha1,v")
       :type 'file-missing)
      (should-error
       (insert-file-contents
        "moo://waterpoint/object/267/verb/SCCS/s.sha1")
       :type 'file-missing))))

(ert-deftest lambdamoo-code-command-line-normalization-preserves-uri ()
  (let ((uri "moo://waterpoint/object/18/verb/explode"))
    (should (file-name-absolute-p uri))
    (should (equal (substitute-in-file-name uri) uri))
    (should (equal (command-line-normalize-file-name uri) uri))
    (should (equal (command-line-normalize-file-name "some//local-file")
                   "some/local-file"))))

(ert-deftest lambdamoo-code-read-document-prefers-open-buffer ()
  (let ((uri "moo://waterpoint/object/18/verb/explode"))
    (with-temp-buffer
      (setq buffer-file-name uri)
      (insert "unsaved source")
      (cl-letf (((symbol-function 'lambdamoo-code-read-uri)
                 (lambda (_) (ert-fail "WebDAV should not be read"))))
        (lambdamoo-code-test--with-server server
          (should
           (equal (eglot-handle-request
                   server 'lambdamoo/readDocument :uri uri)
                  '(:text "unsaved source"))))))))

(ert-deftest lambdamoo-code-read-document-fetches-closed-uri ()
  (cl-letf (((symbol-function 'lambdamoo-code-read-uri)
             (lambda (uri)
               (should (equal uri "moo://waterpoint/object/1/verb/look"))
               "remote source")))
    (lambdamoo-code-test--with-server server
      (should
       (equal (eglot-handle-request
               server 'lambdamoo/readDocument
               :uri "moo://waterpoint/object/1/verb/look")
              '(:text "remote source"))))))

(ert-deftest lambdamoo-code-insert-file-contents-can-visit ()
  (cl-letf (((symbol-function 'lambdamoo-code--request-bytes)
             (lambda (_) "remote source")))
    (with-temp-buffer
      (should (equal (lambdamoo-code--insert-file-contents
                      "moo://waterpoint/object/1/verb/look" t)
                     '("moo://waterpoint/object/1/verb/look" 13)))
      (should (equal (buffer-string) "remote source"))
      (should (equal buffer-file-name
                     "moo://waterpoint/object/1/verb/look"))
      (should-not (buffer-modified-p)))))

(ert-deftest lambdamoo-code-partial-read-uses-file-byte-offsets ()
  (cl-letf (((symbol-function 'lambdamoo-code--request-bytes)
             (lambda (_) (encode-coding-string "aé\r\nb" 'utf-8))))
    (with-temp-buffer
      ;; Bytes 1..5 contain the two-byte character followed by CRLF.
      (should (equal (lambdamoo-code--insert-file-contents
                      "moo://waterpoint/object/1/verb/look" nil 1 5)
                     '("moo://waterpoint/object/1/verb/look" 2)))
      (should (equal (buffer-string) "é\n")))))

(ert-deftest lambdamoo-code-write-region-nil-bounds-write-whole-buffer ()
  (let (written)
    (cl-letf (((symbol-function 'lambdamoo-code-write-uri)
               (lambda (uri contents) (setq written (list uri contents)))))
      (with-temp-buffer
        (insert "entire buffer")
        (lambdamoo-code--write-region
         nil nil "moo://waterpoint/object/267/verb/sha1")
        (should (equal written
                       '("moo://waterpoint/object/267/verb/sha1"
                         "entire buffer")))))))

(ert-deftest lambdamoo-code-auto-save-file-is-local ()
  (let ((lambdamoo-code-workspace-directory temporary-file-directory))
    (with-temp-buffer
      (setq buffer-file-name "moo://waterpoint/object/267/verb/sha1")
      (let ((auto-save-name (make-auto-save-file-name)))
        (should (string-prefix-p
                 (file-name-as-directory temporary-file-directory)
                 auto-save-name))
        (should-not (string-prefix-p "moo://" auto-save-name))))))

(ert-deftest lambdamoo-code-file-regular-p-rejects-auxiliary-paths ()
  (cl-letf (((symbol-function 'lambdamoo-code-read-uri)
             (lambda (_) "source")))
    (should (file-regular-p
             "moo://waterpoint/object/18/verb/explode"))
    (should-not (file-regular-p
                 "moo://waterpoint/object/18/verb/explode,v"))
    (should-not (file-regular-p
                 "moo://waterpoint/object/18/verb/RCS/explode,v"))))

(ert-deftest lambdamoo-code-dirty-buffer-defers-canonicalization ()
  (with-temp-buffer
    (setq buffer-file-name "moo://waterpoint/owned/525/verb/look")
    (insert "changed")
    (set-buffer-modified-p t)
    (lambdamoo-code-test--with-server server
      (eglot-handle-notification
       server 'lambdamoo/canonicalizeDocument
       :uri buffer-file-name
       :canonicalUri "moo://waterpoint/object/525/verb/look"))
    (should (equal buffer-file-name "moo://waterpoint/owned/525/verb/look"))
    (should (equal lambdamoo-code--pending-canonical-uri
                   "moo://waterpoint/object/525/verb/look"))))

(ert-deftest lambdamoo-code-canonicalization-transfers-etag ()
  (let ((lambdamoo-code--etags (make-hash-table :test #'equal))
        (auto-mode-alist nil)
        (alias "moo://waterpoint/owned/525/verb/look")
        (canonical "moo://waterpoint/object/525/verb/look"))
    (puthash alias "\"version-1\"" lambdamoo-code--etags)
    (cl-letf (((symbol-function 'lambdamoo-code-read-uri)
               (lambda (_) "source")))
      (with-temp-buffer
        (setq buffer-file-name alias)
        (lambdamoo-code--apply-canonical-uri canonical)
        (should (equal buffer-file-name canonical))
        (should (equal (gethash canonical lambdamoo-code--etags)
                       "\"version-1\""))
        (should-not (gethash alias lambdamoo-code--etags))))))

(ert-deftest lambdamoo-code-deferred-canonicalization-transfers-latest-etag ()
  (let ((lambdamoo-code--etags (make-hash-table :test #'equal))
        (auto-mode-alist nil)
        (alias "moo://waterpoint/owned/525/verb/look")
        (canonical "moo://waterpoint/object/525/verb/look"))
    (cl-letf (((symbol-function 'lambdamoo-code-read-uri)
               (lambda (_) "source")))
      (with-temp-buffer
        (setq buffer-file-name alias
              lambdamoo-code--pending-canonical-uri canonical)
        (puthash alias "\"after-save\"" lambdamoo-code--etags)
        (set-buffer-modified-p nil)
        (lambdamoo-code--apply-pending-canonical-uri)
        (should (equal (gethash canonical lambdamoo-code--etags)
                       "\"after-save\""))
        (should-not (gethash alias lambdamoo-code--etags))))))

(ert-deftest lambdamoo-code-canonicalization-reopens-lsp-document ()
  (let ((lambdamoo-code--etags (make-hash-table :test #'equal))
        (auto-mode-alist nil)
        (alias "moo://waterpoint/owned/525/verb/look")
        (canonical "moo://waterpoint/object/525/verb/look")
        events)
    (cl-letf (((symbol-function 'lambdamoo-code-read-uri)
               (lambda (_) "source"))
              ((symbol-function 'eglot-managed-p) (lambda () t))
              ((symbol-function 'eglot--TextDocumentIdentifier)
               (lambda () (cdr eglot--TextDocumentIdentifier-cache)))
              ((symbol-function 'eglot--signal-textDocument/didClose)
               (lambda ()
                 (push (list 'close
                             (car eglot--TextDocumentIdentifier-cache))
                       events)))
              ((symbol-function 'eglot--signal-textDocument/didOpen)
               (lambda () (push (list 'open buffer-file-name) events))))
      (with-temp-buffer
        (setq buffer-file-name alias
              eglot--TextDocumentIdentifier-cache
              (cons alias `(:uri ,alias)))
        (lambdamoo-code--apply-canonical-uri canonical)
        (should (equal (nreverse events)
                       `((close ,alias) (open ,canonical))))
        (should-not eglot--TextDocumentIdentifier-cache)))))

;;; lambdamoo-code-mode-test.el ends here
