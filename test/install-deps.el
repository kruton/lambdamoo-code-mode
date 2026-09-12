;;; install-deps.el --- Install test dependencies -*- lexical-binding: t; -*-

(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/"))
      package-install-upgrade-built-in t)
(package-initialize)
(package-refresh-contents)

(unless (assq 'eglot package-archive-contents)
  (error "Eglot is unavailable from GNU ELPA"))

;; Let Package.el select the newest descriptor.  In newer Package.el versions,
;; an entry in `package-archive-contents' contains a list of descriptors rather
;; than a single descriptor, so passing its cdr directly is not portable.
(package-install 'eglot)
(message "Installed current GNU ELPA Eglot")

;;; install-deps.el ends here
