;;; lambdamoo-code-mode.el --- Edit LambdaMOO verb code with Eglot -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Kenny Root
;; SPDX-License-Identifier: MIT
;; Author: Kenny Root
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (eglot "1.20"))
;; Keywords: languages, tools
;; URL: https://github.com/kruton/lambdamoo-code-mode/

;;; Commentary:

;; Provides a major mode for editing LambdaMOO verb code, a `moo://' file-name
;; handler, and the remote-document protocol used by moo-lsp-rs.  This is a
;; source-code editing package, not an interactive MOO client.  WebDAV
;; credentials remain in Emacs and can be supplied by auth-source (for
;; example, ~/.authinfo.gpg).

;;; Code:

(require 'cl-lib)
(require 'auth-source)
(require 'eglot)
(require 'jsonrpc)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'url-util)
(require 'xml)

(declare-function eglot-semantic-tokens-mode "eglot" (&optional arg))
(declare-function dired-insert-set-properties "dired" (beg end))

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(define-error 'lambdamoo-code-file-error
  "LambdaMOO remote file error" 'file-error)

(defgroup lambdamoo-code nil
  "LambdaMOO editing over WebDAV."
  :group 'languages)

(defcustom lambdamoo-code-connections nil
  "Mapping from `moo://' authorities to WebDAV endpoint URLs.

Each entry has the form (AUTHORITY . ENDPOINT), for example:

  ((\"codepoint\" . \"https://codepoint.example/dav/\"))"
  :type '(alist :key-type string :value-type string)
  :group 'lambdamoo-code)

(defcustom lambdamoo-code-request-timeout 15
  "Maximum number of seconds to wait for a WebDAV request."
  :type 'number
  :group 'lambdamoo-code)

(defcustom lambdamoo-code-workspace-directory user-emacs-directory
  "Existing local directory Eglot uses for remote LambdaMOO buffers.

The LambdaMOO document URI remains `moo://'; this only prevents Emacs and
Eglot from treating the URI's virtual parent as a local project directory."
  :type 'directory
  :group 'lambdamoo-code)

(defvar lambdamoo-code--etags (make-hash-table :test #'equal))
(defvar-local lambdamoo-code--pending-canonical-uri nil)

(defcustom lambdamoo-code-indent-offset 2
  "Number of spaces per indentation level in LambdaMOO verb code."
  :type 'integer
  :safe #'integerp
  :group 'lambdamoo-code)

(defconst lambdamoo-code--block-openers
  '("if" "for" "while" "fork" "try")
  "Statements that begin indented blocks.")

(defconst lambdamoo-code--block-middles
  '("else" "elseif" "except" "finally")
  "Statements that close one block section and begin another.")

(defconst lambdamoo-code--block-closers
  '("endif" "endfor" "endwhile" "endfork" "endtry")
  "Statements that end indented blocks.")

(defconst lambdamoo-code-font-lock-keywords
  `((,(regexp-opt '("ANY" "break" "continue" "else" "elseif" "endfor"
                    "endfork" "endif" "endtry" "endwhile" "except"
                    "finally" "for" "fork" "if" "in" "return" "try"
                    "while")
                  'symbols)
     . font-lock-keyword-face)
    (,(regexp-opt '("E_ARGS" "E_DIV" "E_FLOAT" "E_INVARG" "E_INVIND"
                    "E_MAXREC" "E_NACC" "E_NONE" "E_PERM" "E_PROPNF"
                    "E_QUOTA" "E_RANGE" "E_RECMOVE" "E_TYPE" "E_VARNF"
                    "E_VERBNF")
                  'symbols)
     . font-lock-constant-face)
    ("#-?[0-9]+" . font-lock-constant-face)
    ("\\_<[0-9]+\\(?:\\.[0-9]+\\)?\\_>" . font-lock-number-face)
    ("\\_<[[:alpha:]_][[:alnum:]_]*\\_>" . font-lock-variable-name-face))
  "Fallback font-lock rules derived from tree-sitter-lambdamoo.")

(defvar lambdamoo-code-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    table)
  "Syntax table used by `lambdamoo-code-mode'.")

(defun lambdamoo-code--line-keyword ()
  "Return the current line's initial lowercase keyword, if any."
  (save-excursion
    (back-to-indentation)
    (when (looking-at "\\([[:alpha:]]+\\)\\_>")
      (downcase (match-string-no-properties 1)))))

(defun lambdamoo-code-indent-line ()
  "Indent the current line as LambdaMOO verb code."
  (interactive)
  (let* ((column (current-column))
         (at-indentation (<= column (current-indentation)))
         (current-keyword (lambdamoo-code--line-keyword))
         (closing (or (member current-keyword lambdamoo-code--block-closers)
                      (member current-keyword lambdamoo-code--block-middles)))
         (indent
          (save-excursion
            (let ((found nil))
              (while (and (not found) (= (forward-line -1) 0))
                (unless (looking-at-p "^[[:space:]]*$")
                  (setq found t)))
              (if (not found)
                  0
                (let* ((previous-indent (current-indentation))
                       (previous-keyword (lambdamoo-code--line-keyword))
                       (opening (or (member previous-keyword
                                            lambdamoo-code--block-openers)
                                    (member previous-keyword
                                            lambdamoo-code--block-middles))))
                  (max 0 (+ previous-indent
                            (if opening lambdamoo-code-indent-offset 0)
                            (if closing (- lambdamoo-code-indent-offset) 0)))))))))
    (indent-line-to indent)
    (unless at-indentation
      (move-to-column (max indent column)))))

(defun lambdamoo-code--local-workspace-directory ()
  "Return an existing local directory for workspace and temporary files."
  (let ((configured (expand-file-name lambdamoo-code-workspace-directory)))
    (file-name-as-directory
     (if (file-directory-p configured) configured temporary-file-directory))))

(defun lambdamoo-code--auto-save-visited-p ()
  "Return nil so automatic saves never write back to a MOO."
  nil)

(define-derived-mode lambdamoo-code-mode prog-mode "LambdaMOO Code"
  "Major mode for editing LambdaMOO verb code."
  (setq-local default-directory (lambdamoo-code--local-workspace-directory))
  (setq-local comment-start "\"")
  (setq-local comment-end "\";")
  (setq-local comment-start-skip "\"+[[:space:]]*")
  (setq-local font-lock-defaults '(lambdamoo-code-font-lock-keywords nil t))
  (setq-local indent-line-function #'lambdamoo-code-indent-line)
  ;; Ordinary auto-save data is redirected to a local recovery file by the
  ;; file-name handler.  Also exclude these buffers from
  ;; `auto-save-visited-mode', which would otherwise perform a real WebDAV PUT.
  (setq-local auto-save-visited-predicate
              #'lambdamoo-code--auto-save-visited-p))

(defclass lambdamoo-code-server (eglot-lsp-server) ())

(defun lambdamoo-code--parse-uri (uri)
  "Return (AUTHORITY . PATH) for URI, or nil when it is invalid."
  (when (string-match "\\`moo://\\([^/]+\\)/\\(.*\\)\\'" uri)
    (cons (match-string 1 uri) (match-string 2 uri))))

(defun lambdamoo-code-verb-uri (authority reference)
  "Return a `moo://' URI for AUTHORITY and SimpleEdit REFERENCE.

REFERENCE begins with an object and verb specification such as
`#123:my_verb'.  Any trailing MCP SimpleEdit arguments, as in
`#123:my_verb this none this', are ignored."
  (unless (and (stringp authority)
               (string-match-p "\\`[^/[:space:]]+\\'" authority))
    (user-error "Invalid LambdaMOO authority: %s" authority))
  (unless (and (stringp reference)
               (string-match "\\`#\\([0-9]+\\):\\([^[:space:]]+\\)" reference))
    (user-error "Invalid LambdaMOO verb reference: %s" reference))
  (let ((object (match-string 1 reference))
        (verb (match-string 2 reference)))
    (format "moo://%s/object/%s/verb/%s" authority object verb)))

;;;###autoload
(defun lambdamoo-code-open-verb (authority reference)
  "Open a LambdaMOO verb from AUTHORITY and SimpleEdit REFERENCE.

For example, authority `waterpoint' and reference
`#123:my_verb this none this' open
`moo://waterpoint/object/123/verb/my_verb'."
  (interactive
   (list (completing-read "LambdaMOO authority: "
                          (mapcar #'car lambdamoo-code-connections)
                          nil nil)
         (read-string "MCP SimpleEdit verb reference: ")))
  (find-file (lambdamoo-code-verb-uri authority reference)))

(defun lambdamoo-code--endpoint (authority)
  "Return the configured WebDAV endpoint for AUTHORITY."
  (or (cdr (assoc-string authority lambdamoo-code-connections t))
      (error "No LambdaMOO connection configured for %s" authority)))

(defun lambdamoo-code--http-url (uri)
  "Translate URI to its configured WebDAV URL."
  (pcase-let ((`(,authority . ,path)
               (or (lambdamoo-code--parse-uri uri)
                   (error "Invalid LambdaMOO URI: %s" uri))))
    (concat (replace-regexp-in-string "/+\\'" "" (lambdamoo-code--endpoint authority))
            "/" path)))

(defun lambdamoo-code--response-header (name)
  "Return HTTP response header NAME from the current URL buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (re-search-forward
             (concat "^" (regexp-quote name) ":[ \t]*\\([^\r\n]+\\)")
             url-http-end-of-headers t)
        (string-trim (match-string 1))))))

(defun lambdamoo-code--authentication-header (uri)
  "Return a Basic Authorization header for URI from `auth-source'."
  (let* ((url (url-generic-parse-url (lambdamoo-code--http-url uri)))
         (host (url-host url))
         (port (url-port url))
         (service (url-type url))
         (host-and-port (and host port (format "%s:%d" host port)))
         (match
          (cl-loop for candidate in (delete-dups (delq nil (list host-and-port host)))
                   thereis (car (auth-source-search
                                 :host candidate :port service :max 1
                                 :require '(:user :secret))))))
    (when match
      (let ((user (plist-get match :user))
            (secret (plist-get match :secret)))
        (when (functionp secret)
          (setq secret (funcall secret)))
        (when (and user secret)
          (cons "Authorization"
                (concat "Basic "
                        (base64-encode-string
                         (format "%s:%s" user secret) t))))))))

(defun lambdamoo-code--decode-response-body (body)
  "Decode UTF-8 BODY and normalize CRLF line endings for Emacs buffers."
  (replace-regexp-in-string
   "\r\n" "\n" (decode-coding-string body 'utf-8) t t))

(cl-defun lambdamoo-code--request-bytes (uri &key (method "GET") data headers)
  "Request URI over WebDAV and return its raw response body.

METHOD, DATA, and HEADERS are bound as the corresponding URL request
variables.  Non-2xx responses signal an error."
  (let* ((headers (copy-tree headers))
         (headers (if (assoc-string "Authorization" headers t)
                      headers
                    (if-let ((authorization
                              (lambdamoo-code--authentication-header uri)))
                        (cons authorization headers)
                      headers)))
         (url-request-method method)
         (url-request-data data)
         (url-request-extra-headers headers)
         (buffer (url-retrieve-synchronously
                  (lambdamoo-code--http-url uri) t t lambdamoo-code-request-timeout)))
    (unless buffer
      (signal 'lambdamoo-code-file-error
              (list (format "WebDAV %s timed out after %s seconds"
                            method lambdamoo-code-request-timeout)
                    uri)))
    (unwind-protect
        (with-current-buffer buffer
          (unless (and url-http-response-status
                       (<= 200 url-http-response-status)
                       (< url-http-response-status 300))
            (signal 'lambdamoo-code-file-error
                    (list (format "WebDAV %s returned HTTP %s"
                                  method
                                  (or url-http-response-status "unknown"))
                          uri)))
          (when-let ((etag (lambdamoo-code--response-header "ETag")))
            (puthash uri etag lambdamoo-code--etags))
          (goto-char url-http-end-of-headers)
          (buffer-substring-no-properties (point) (point-max)))
      (kill-buffer buffer))))

(cl-defun lambdamoo-code--request (uri &key (method "GET") data headers)
  "Request URI over WebDAV and return its decoded response body.

METHOD, DATA, and HEADERS are passed to `lambdamoo-code--request-bytes'."
  (lambdamoo-code--decode-response-body
   (lambdamoo-code--request-bytes
    uri :method method :data data :headers headers)))

(defun lambdamoo-code-read-uri (uri)
  "Read and return the UTF-8 contents of URI."
  (lambdamoo-code--request uri))

(defun lambdamoo-code-write-uri (uri contents)
  "Write CONTENTS as UTF-8 to URI using an ETag precondition when available."
  (let ((headers '(("Content-Type" . "text/plain; charset=utf-8"))))
    (when-let ((etag (gethash uri lambdamoo-code--etags)))
      (push (cons "If-Match" etag) headers))
    (lambdamoo-code--request
     uri :method "PUT" :data (encode-coding-string contents 'utf-8) :headers headers)))

(defun lambdamoo-code--xml-local-name (node)
  "Return NODE's XML local name as a string."
  (when (consp node)
    (replace-regexp-in-string ".*:" "" (symbol-name (car node)))))

(defun lambdamoo-code--xml-children (node name)
  "Return NODE's child elements whose local name is NAME."
  (cl-remove-if-not
   (lambda (child)
     (and (consp child)
          (equal (lambdamoo-code--xml-local-name child) name)))
   (xml-node-children node)))

(defun lambdamoo-code--xml-child (node name)
  "Return NODE's first child element whose local name is NAME."
  (car (lambdamoo-code--xml-children node name)))

(defun lambdamoo-code--xml-text (node)
  "Return the concatenated text below XML NODE."
  (when node
    (string-trim
     (mapconcat
      (lambda (child)
        (cond ((stringp child) child)
              ((consp child) (or (lambdamoo-code--xml-text child) ""))
              (t "")))
      (xml-node-children node) ""))))

(defun lambdamoo-code--xml-property (response name)
  "Return local-name property NAME from a WebDAV RESPONSE node."
  (cl-loop for propstat in (lambdamoo-code--xml-children response "propstat")
           for prop = (lambdamoo-code--xml-child propstat "prop")
           when prop
           thereis (cl-loop for child in (xml-node-children prop)
                            when (and (consp child)
                                      (equal (lambdamoo-code--xml-local-name child)
                                             name))
                            return child)))

(defun lambdamoo-code--decode-href (href endpoint-url)
  "Decode a WebDAV HREF for ENDPOINT-URL and return its UTF-8 path.

Absolute hrefs from a different origin are rejected."
  (let* ((absolute (string-match-p "\\`https?://" href))
         (href-url (and absolute (ignore-errors (url-generic-parse-url href))))
         (same-origin
          (or (not absolute)
              (and href-url
                   (equal (url-type href-url) (url-type endpoint-url))
                   (equal (downcase (or (url-host href-url) ""))
                          (downcase (or (url-host endpoint-url) "")))
                   (equal (url-port href-url) (url-port endpoint-url)))))
         (path (and same-origin
                    (if absolute (url-filename href-url) href))))
    (when path
      (setq path (replace-regexp-in-string "[?#].*\\'" "" path))
      (decode-coding-string (url-unhex-string path) 'utf-8))))

(defun lambdamoo-code--directory-entry
    (response authority endpoint-url endpoint-base request-path)
  "Convert WebDAV RESPONSE to a directory entry plist.

AUTHORITY names the MOO connection.  ENDPOINT-BASE and REQUEST-PATH are
normalized HTTP paths used to reject the directory itself and foreign hrefs."
  (when-let* ((href-node (lambdamoo-code--xml-child response "href"))
              (href (lambdamoo-code--xml-text href-node))
              (href-path (lambdamoo-code--decode-href href endpoint-url)))
    (let* ((collection-node (lambdamoo-code--xml-property response "resourcetype"))
           (directory (or (string-suffix-p "/" href-path)
                          (lambdamoo-code--xml-child collection-node "collection")))
           (normalized-href (directory-file-name href-path))
           (normalized-request (directory-file-name request-path)))
      (when (and (not (equal normalized-href normalized-request))
                 (string-prefix-p endpoint-base href-path))
        (let* ((relative (string-remove-prefix endpoint-base href-path))
               (relative (string-remove-prefix "/" relative))
               (relative (if directory
                             (file-name-as-directory relative)
                           relative))
               (name (file-name-nondirectory (directory-file-name relative)))
               (name (if directory (concat name "/") name)))
          (list :name name
                :uri (concat "moo://" authority "/" relative)
                :directory (and directory t)
                :owner (lambdamoo-code--xml-text
                        (lambdamoo-code--xml-property response "owner"))
                :permissions (lambdamoo-code--xml-text
                              (lambdamoo-code--xml-property response "permissions"))
                :names (lambdamoo-code--xml-text
                        (lambdamoo-code--xml-property response "names"))
                :arguments (lambdamoo-code--xml-text
                            (lambdamoo-code--xml-property response "arguments"))))))))

(defun lambdamoo-code-list-directory (uri)
  "Return WebDAV child entry plists for directory URI."
  (pcase-let* ((`(,authority . ,path)
                (or (lambdamoo-code--parse-uri uri)
                    (error "Invalid LambdaMOO URI: %s" uri)))
               (endpoint (lambdamoo-code--endpoint authority))
               (endpoint-url (url-generic-parse-url endpoint))
               (endpoint-path (or (url-filename endpoint-url) "/"))
               (endpoint-base (file-name-as-directory endpoint-path))
               (request-path (concat endpoint-base path))
               (body (lambdamoo-code--request
                      uri :method "PROPFIND" :headers '(("Depth" . "1")))))
    (condition-case error-data
        (with-temp-buffer
          (insert body)
          (let* ((document (xml-parse-region (point-min) (point-max)))
                 (root (cl-find-if
                        (lambda (node)
                          (equal (lambdamoo-code--xml-local-name node)
                                 "multistatus"))
                        document)))
            (unless root
              (error "WebDAV response lacks DAV:multistatus"))
            (delq nil
                  (mapcar
                   (lambda (response)
                     (lambdamoo-code--directory-entry
                      response authority endpoint-url endpoint-base request-path))
                   (lambdamoo-code--xml-children root "response")))))
      (error
       (signal 'lambdamoo-code-file-error
               (list (format "Invalid WebDAV PROPFIND response: %s"
                             (error-message-string error-data))
                     uri))))))

(defun lambdamoo-code--buffer-for-uri (uri)
  "Return the live buffer visiting URI, if any."
  (cl-find-if
   (lambda (buffer)
     (with-current-buffer buffer
       (equal buffer-file-name uri)))
   (buffer-list)))

(defun lambdamoo-code--buffer-text (buffer)
  "Return all text in BUFFER without properties."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun lambdamoo-code--source-uri-p (uri)
  "Return non-nil when URI names a LambdaMOO verb source resource."
  (and (stringp uri)
       (string-match-p "/verb/[^/]+\\'" uri)
       (not (string-suffix-p ",v" uri))))

(defun lambdamoo-code--directory-uri-p (uri)
  "Return non-nil when URI names a LambdaMOO collection.

Emacs removes a trailing slash before some `file-directory-p' probes, so
non-source MOO paths must also be recognized in that normalized form."
  (and (stringp uri)
       (string-match-p "\\`moo://[^/]+\\(?:/.*\\)?\\'" uri)
       (not (lambdamoo-code--source-uri-p uri))
       (not (lambdamoo-code--auxiliary-uri-p uri))))

(defun lambdamoo-code--auxiliary-uri-p (uri)
  "Return non-nil when URI is an auxiliary version-control path."
  (and (stringp uri)
       (or (string-match-p "/\\(?:RCS\\|SCCS\\)/" uri)
           (string-suffix-p ",v" uri))))

(defun lambdamoo-code--insert-file-contents (filename &optional visit beg end replace)
  "Implement `insert-file-contents' for LambdaMOO FILENAME."
  (when (lambdamoo-code--auxiliary-uri-p filename)
    (signal 'file-missing
            (list "Opening input file" "No such file or directory" filename)))
  (let* ((bytes (lambdamoo-code--request-bytes filename))
         (beg (or beg 0))
         (end (or end (string-bytes bytes)))
         (selected (lambdamoo-code--decode-response-body
                    (substring bytes beg end))))
    (when replace
      (delete-region (point-min) (point-max)))
    (insert selected)
    (when visit
      ;; A file-name handler owns the VISIT contract: install the visited
      ;; name, record a timestamp, and leave the freshly read buffer clean.
      (setq buffer-file-name filename)
      (set-visited-file-modtime (current-time))
      (set-buffer-modified-p nil))
    (list filename (length selected))))

(defun lambdamoo-code--write-region (start end filename &optional append visit _lockname _mustbenew)
  "Implement `write-region' for LambdaMOO FILENAME."
  (when append
    (error "Appending to LambdaMOO resources is unsupported"))
  (let ((contents (if (stringp start)
                      start
                    (save-restriction
                      (when (null start)
                        (widen)
                        (setq start (point-min)
                              end (point-max)))
                      (buffer-substring-no-properties start end)))))
    (lambdamoo-code-write-uri filename contents)
    (when (or (eq visit t) (stringp visit))
      (set-buffer-modified-p nil))
    nil))

(defun lambdamoo-code--make-auto-save-file-name ()
  "Return a collision-resistant local auto-save name for the current MOO file."
  (expand-file-name
   (format "#lambdamoo-code-%s#"
           (secure-hash 'sha1 (or buffer-file-name (buffer-name))))
   (lambdamoo-code--local-workspace-directory)))

(defun lambdamoo-code--file-attributes (filename)
  "Return synthetic regular-file attributes for FILENAME."
  ;; Do not turn transport, authentication, or HTTP failures into nil.  A nil
  ;; result tells `find-file' that this is a valid new file and leads to a
  ;; misleading empty buffer.  These resources are server-backed, so surface
  ;; the original failure instead.
  (let ((size (string-bytes (lambdamoo-code-read-uri filename)))
        (now (current-time)))
    (list nil 1 (user-login-name) (user-login-name)
          now now now size "-rw-rw-rw-" nil 0 0)))

(defun lambdamoo-code--directory-attributes ()
  "Return synthetic attributes for a virtual LambdaMOO directory."
  (let ((now (current-time)))
    (list t 1 (user-login-name) (user-login-name)
          now now now 0 "drwxrwxrwx" nil 0 0)))

(defun lambdamoo-code--entry-attributes (entry)
  "Return file attributes synthesized from directory ENTRY."
  (let ((now (current-time))
        (directory (plist-get entry :directory))
        (owner (or (plist-get entry :owner) ""))
        (permissions (or (plist-get entry :permissions) "")))
    (list (and directory t) 1 owner permissions now now now 0
          (if directory "drwxrwxrwx" "-rw-rw-rw-") nil 0 0)))

(defun lambdamoo-code--directory-files (directory &optional full match nosort count)
  "Implement `directory-files' for LambdaMOO DIRECTORY."
  (let* ((directory (file-name-as-directory directory))
         (entries (lambdamoo-code-list-directory directory))
         (names (append '("." "..")
                        (mapcar (lambda (entry) (plist-get entry :name)) entries)))
         (names (if match
                    (cl-remove-if-not (lambda (name) (string-match-p match name)) names)
                  names))
         (names (if nosort names (sort names #'string-lessp)))
         (names (if count (cl-subseq names 0 (min count (length names))) names)))
    (if full
        (mapcar (lambda (name) (concat directory name)) names)
      names)))

(defun lambdamoo-code--directory-files-and-attributes
    (directory &optional full match nosort _id-format count)
  "Implement `directory-files-and-attributes' for LambdaMOO DIRECTORY."
  (let* ((directory (file-name-as-directory directory))
         (entries (append '((:name "." :directory t)
                            (:name ".." :directory t))
                          (lambdamoo-code-list-directory directory)))
         (entries (if match
                      (cl-remove-if-not
                       (lambda (entry) (string-match-p match (plist-get entry :name)))
                       entries)
                    entries))
         (entries (if nosort
                      entries
                    (sort entries (lambda (left right)
                                    (string-lessp (plist-get left :name)
                                                  (plist-get right :name))))))
         (entries (if count
                      (cl-subseq entries 0 (min count (length entries)))
                    entries)))
    (mapcar
     (lambda (entry)
       (let ((name (plist-get entry :name)))
         (cons (if full (concat directory name) name)
               (lambdamoo-code--entry-attributes entry))))
     entries)))

(defun lambdamoo-code--file-name-all-completions (file directory)
  "Return all LambdaMOO names in DIRECTORY beginning with FILE."
  (let ((case-fold-search completion-ignore-case))
    (cl-remove-if-not
     (lambda (name) (string-prefix-p file name completion-ignore-case))
     (mapcar (lambda (entry) (plist-get entry :name))
             (lambdamoo-code-list-directory
              (file-name-as-directory directory))))))

(defun lambdamoo-code--file-name-completion (file directory &optional predicate)
  "Complete LambdaMOO FILE in DIRECTORY, respecting PREDICATE."
  (let ((candidates (lambdamoo-code--file-name-all-completions file directory)))
    (when predicate
      (setq candidates
            (cl-remove-if-not
             (lambda (name) (funcall predicate (concat directory name)))
             candidates)))
    (try-completion file (mapcar #'list candidates))))

(defun lambdamoo-code--insert-directory
    (file _switches &optional _wildcard full-directory-p)
  "Insert a Dired-compatible WebDAV listing for FILE."
  (let ((entries (sort (lambdamoo-code-list-directory
                        (file-name-as-directory file))
                       (lambda (left right)
                         (string-lessp (plist-get left :name)
                                       (plist-get right :name)))))
        (timestamp (format-time-string "%b %e %H:%M")))
    (when full-directory-p
      (insert (format "total %d\n" (length entries))))
    (dolist (entry entries)
      (let* ((directory (plist-get entry :directory))
             (name (plist-get entry :name))
             (base-name (string-remove-suffix "/" name))
             (owner (or (plist-get entry :owner) ""))
             (permissions (or (plist-get entry :permissions) ""))
             (arguments (or (plist-get entry :arguments) ""))
             (names (or (plist-get entry :names) ""))
             (aliases (if (or (string-empty-p names) (equal names base-name))
                          ""
                        (format "(%s)" names))))
        (insert (format "  %s 1 %-12s %-6s %8d %s  %-24s %-20s "
                        (if directory "drwxrwxrwx" "-rw-rw-rw-")
                        owner permissions 0 timestamp arguments aliases))
        (insert (propertize name 'dired-filename t))
        (insert "\n")))
    nil))

(defun lambdamoo-code--dired-insert-directory
    (original directory switches &optional file-list wildcard header)
  "Call ORIGINAL or insert a WebDAV listing for MOO DIRECTORY.

FILE-LIST and WILDCARD are accepted for compatibility with
`dired-insert-directory'."
  (if (not (and (stringp directory) (string-prefix-p "moo://" directory)))
      (funcall original directory switches file-list wildcard header)
    (let ((start (point)))
      (when header
        (insert "  " (directory-file-name directory) ":\n"))
      (let ((content-start (point)))
        ;; LambdaMOO directories are read-only, so requests to relist selected
        ;; entries can safely refresh the complete directory.
        (lambdamoo-code--insert-directory directory switches wildcard t)
        (dired-insert-set-properties content-start (point)))
      (unless (save-excursion (goto-char start) (looking-at-p "  "))
        (indent-rigidly start (point) 2)))))

(defun lambdamoo-code--file-handler (operation &rest args)
  "Handle file OPERATION with ARGS for `moo://' resources."
  (let ((inhibit-file-name-handlers
         (cons 'lambdamoo-code--file-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (pcase operation
      ('insert-file-contents (apply #'lambdamoo-code--insert-file-contents args))
      ('insert-file-contents-literally
       (apply #'lambdamoo-code--insert-file-contents args))
      ('write-region (apply #'lambdamoo-code--write-region args))
      ('make-auto-save-file-name (lambdamoo-code--make-auto-save-file-name))
      ('insert-directory (apply #'lambdamoo-code--insert-directory args))
      ('directory-files (apply #'lambdamoo-code--directory-files args))
      ('directory-files-and-attributes
       (apply #'lambdamoo-code--directory-files-and-attributes args))
      ('file-name-all-completions
       (apply #'lambdamoo-code--file-name-all-completions args))
      ('file-name-completion
       (apply #'lambdamoo-code--file-name-completion args))
      ('file-exists-p
       (cond ((lambdamoo-code--directory-uri-p (car args)) t)
             ((lambdamoo-code--source-uri-p (car args))
              (and (lambdamoo-code--file-attributes (car args)) t))
             (t nil)))
      ('file-readable-p
       (cond ((lambdamoo-code--directory-uri-p (car args)) t)
             ((lambdamoo-code--source-uri-p (car args))
              (and (lambdamoo-code--file-attributes (car args)) t))
             (t nil)))
      ('file-writable-p (and (lambdamoo-code--parse-uri (car args)) t))
      ('file-attributes
       (cond ((lambdamoo-code--directory-uri-p (car args))
              (lambdamoo-code--directory-attributes))
             ((lambdamoo-code--source-uri-p (car args))
              (lambdamoo-code--file-attributes (car args)))
             (t nil)))
      ('file-regular-p
       (and (lambdamoo-code--source-uri-p (car args))
            (lambdamoo-code--file-attributes (car args))
            t))
      ('file-directory-p (and (lambdamoo-code--directory-uri-p (car args)) t))
      ('file-truename (car args))
      ('expand-file-name (car args))
      ('abbreviate-file-name (car args))
      ('file-remote-p nil)
      ('verify-visited-file-modtime t)
      ('set-visited-file-modtime t)
      ('get-file-buffer (lambdamoo-code--buffer-for-uri (car args)))
      ('file-name-directory
       (when (string-match "\\`\\(moo://.*/\\)[^/]*\\'" (car args))
         (match-string 1 (car args))))
      ('file-name-nondirectory
       (if (string-match "/\\([^/]*\\)\\'" (car args))
           (match-string 1 (car args))
         (car args)))
      (_ (apply operation args)))))

(defun lambdamoo-code--apply-canonical-uri (uri)
  "Change the current buffer's visited name to canonical URI without moving it."
  (let* ((old-uri buffer-file-name)
         (etag (and old-uri (gethash old-uri lambdamoo-code--etags)))
         (managed (eglot-managed-p))
         (old-identifier-cache
          (when managed
            (eglot--TextDocumentIdentifier)
            eglot--TextDocumentIdentifier-cache)))
    (set-visited-file-name uri t nil)
    (when etag
      ;; `set-visited-file-name' may probe URI and obtain a newer ETag.  Retain
      ;; that value when present; otherwise carry the alias's precondition over.
      (unless (gethash uri lambdamoo-code--etags)
        (puthash uri etag lambdamoo-code--etags))
      (unless (equal old-uri uri)
        (remhash old-uri lambdamoo-code--etags)))
    (when (and managed (not (equal old-uri uri)))
      ;; Eglot caches the URI used by didOpen.  Close that alias before
      ;; clearing the cache, then open the canonical URI as a new LSP document.
      (let ((eglot--TextDocumentIdentifier-cache old-identifier-cache))
        (eglot--signal-textDocument/didClose))
      (setq eglot--TextDocumentIdentifier-cache nil)
      (eglot--signal-textDocument/didOpen)))
  (setq lambdamoo-code--pending-canonical-uri nil))

(defun lambdamoo-code--apply-pending-canonical-uri ()
  "Apply a canonical URI deferred while the current buffer was dirty."
  (when (and lambdamoo-code--pending-canonical-uri
             (not (buffer-modified-p)))
    (lambdamoo-code--apply-canonical-uri lambdamoo-code--pending-canonical-uri)))

(cl-defmethod eglot-handle-request
  ((_server lambdamoo-code-server)
   (_method (eql lambdamoo/readDocument))
   &key uri &allow-other-keys)
  (unless (and (stringp uri) (string-prefix-p "moo://" uri))
    (jsonrpc-error "Expected a moo: URI"))
  `(:text ,(if-let ((buffer (lambdamoo-code--buffer-for-uri uri)))
               (lambdamoo-code--buffer-text buffer)
             (lambdamoo-code-read-uri uri))))

(cl-defmethod eglot-handle-notification
  ((_server lambdamoo-code-server)
   (_method (eql lambdamoo/canonicalizeDocument))
   &key uri canonicalUri &allow-other-keys)
  (when-let ((buffer (and (stringp uri)
                          (stringp canonicalUri)
                          (lambdamoo-code--buffer-for-uri uri))))
    (with-current-buffer buffer
      (if (buffer-modified-p)
          (setq lambdamoo-code--pending-canonical-uri canonicalUri)
        (lambdamoo-code--apply-canonical-uri canonicalUri)))))

(defun lambdamoo-code--normalize-command-line-file (original file)
  "Preserve `moo://' FILE names, otherwise call ORIGINAL.

Without this advice, `command-line-normalize-file-name' collapses the URI's
double slash before the normal file-name handler can see it."
  (if (and (stringp file) (string-prefix-p "moo://" file))
      file
    (funcall original file)))

(defun lambdamoo-code--file-name-absolute-p (original filename)
  "Treat MOO FILENAME as absolute, otherwise call ORIGINAL."
  (if (and (stringp filename) (string-prefix-p "moo://" filename))
      t
    (funcall original filename)))

(defun lambdamoo-code--substitute-in-file-name (original filename)
  "Preserve MOO FILENAME, otherwise call ORIGINAL.

The standard implementation treats a double slash as a request to discard
everything before it.  That would turn `moo://authority/path' into
`/authority/path'."
  (if (and (stringp filename) (string-prefix-p "moo://" filename))
      filename
    (funcall original filename)))

(defun lambdamoo-code--enable-semantic-tokens ()
  "Keep semantic-token fontification in sync with Eglot management."
  (cond
   ((not (eglot-managed-p))
    (when (bound-and-true-p eglot-semantic-tokens-mode)
      (eglot-semantic-tokens-mode -1)))
   ((fboundp 'eglot-semantic-tokens-mode)
      (progn
        (eglot-semantic-tokens-mode 1)
        ;; The major-mode hook may already have completed its initial font-lock
        ;; pass by the time Eglot connects.  Refontify now that semantic tokens
        ;; are available instead of waiting for the next buffer change.
        (font-lock-flush)
        (font-lock-ensure)))
   (t
    (display-warning
      'lambdamoo-code
      "This Eglot version lacks semantic-token highlighting; install a current Eglot release"
      :warning))))

(defun lambdamoo-code--eglot-setup ()
  "Start Eglot and arrange semantic-token fontification after connection."
  (add-hook 'eglot-managed-mode-hook
            #'lambdamoo-code--enable-semantic-tokens nil t)
  (eglot-ensure))

;;;###autoload
(defun lambdamoo-code-setup ()
  "Install LambdaMOO file handling, mode selection, and Eglot integration."
  (interactive)
  (add-to-list 'file-name-handler-alist '("\\`moo://" . lambdamoo-code--file-handler))
  (add-to-list 'auto-mode-alist '("\\`moo://.*/verb/" . lambdamoo-code-mode))
  (add-to-list 'auto-mode-alist '("\\.moo\\'" . lambdamoo-code-mode))
  (advice-add 'command-line-normalize-file-name :around
              #'lambdamoo-code--normalize-command-line-file)
  (advice-add 'file-name-absolute-p :around
              #'lambdamoo-code--file-name-absolute-p)
  (advice-add 'substitute-in-file-name :around
              #'lambdamoo-code--substitute-in-file-name)
  (with-eval-after-load 'dired
    (advice-add 'dired-insert-directory :around
                #'lambdamoo-code--dired-insert-directory))
  (add-to-list
   'eglot-server-programs
   '(lambdamoo-code-mode
     . (lambdamoo-code-server
        "moo-lsp-rs"
        :initializationOptions (:lambdamoo (:remoteDocuments 1)))))
  (add-hook 'lambdamoo-code-mode-hook #'lambdamoo-code--eglot-setup)
  (add-hook 'lambdamoo-code-mode-hook #'lambdamoo-code--install-buffer-hooks))

(defun lambdamoo-code--install-buffer-hooks ()
  "Install buffer-local hooks used by canonical URI replacement."
  (add-hook 'after-save-hook #'lambdamoo-code--apply-pending-canonical-uri nil t)
  (add-hook 'after-revert-hook #'lambdamoo-code--apply-pending-canonical-uri nil t))

(lambdamoo-code-setup)

(provide 'lambdamoo-code-mode)

;;; lambdamoo-code-mode.el ends here
