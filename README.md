# lambdamoo-code-mode

An Emacs major mode for editing LambdaMOO verb code. This is a source-code
editor integration, not an interactive MOO client. It can edit code via WebDAV
by using `moo://` URLs.

## Requirements

Emacs 29.1 or newer, Eglot 1.20 or newer, and a `moo-lsp-rs` 0.3.0 or newer.
The Eglot bundled with some Emacs releases lacks semantic-token highlighting;
install the current GNU ELPA release with `M-x package-refresh-contents`
followed by `M-x package-install RET eglot RET`.

Install a [`moo-lsp-rs`](https://github.com/kruton/moo-lsp-rs) release binary,
or build it from source, and ensure the `moo-lsp-rs` executable is on `PATH`.

## Installation

Install `lambdamoo-code-mode` directly from its
[Git repository](https://github.com/kruton/lambdamoo-code-mode/).

### With package-vc

On Emacs 29 or newer, run:

```text
M-x package-vc-install RET https://github.com/kruton/lambdamoo-code-mode RET
```

Then add this to your Emacs init file:

```elisp
(require 'lambdamoo-code-mode)
```

### With git

Alternatively, clone the repository into a directory of your choice:

```sh
git clone https://github.com/kruton/lambdamoo-code-mode.git \
  ~/.emacs.d/lisp/lambdamoo-code-mode
```

Then add the checkout to `load-path` from your Emacs init file:

```elisp
(add-to-list 'load-path
             (expand-file-name "lisp/lambdamoo-code-mode" user-emacs-directory))
(require 'lambdamoo-code-mode)
```

## Configuration

To enable editing MOO code via WebDAV (on MOOs that support it), you must provide a map from `moo://AUTHORITY/` to `https://HOST/SUBDIR`. For example:

```elisp
(setq lambdamoo-code-connections
      '(("codepoint" . "https://codepoint.example/dav/")))
```

Store WebDAV credentials using
[`auth-source`](https://emacsdocs.org/docs/emacs/Authentication), for
example in `~/.authinfo.gpg`:

```text
machine codepoint.example login USER password PASSWORD
```

Then visit an exact verb URI:

```elisp
(find-file "moo://codepoint/object/18/verb/explode")
```

You can also open code by providing the AUTHORITY and VERB. For an MCP
2.1 SimpleEdit reference, use `M-x lambdamoo-code-open-verb`. Given
authority `codepoint` and reference `#123:my_verb this none this`, it
opens:

```text
moo://codepoint/object/123/verb/my_verb
```

Open a trailing-slash URI to browse a WebDAV collection in Dired:

```elisp
(find-file "moo://codepoint/object/18/verb/")
```

Directory rows include the MOO owner, permissions, verb argument specification,
and aliases when they differ from the resource filename. Standard Dired keys
such as `RET` to visit an entry, `^` to visit its parent, and `g` to refresh are
supported. Directory mutation commands such as rename, delete, and create are
not implemented.

Normal Emacs auto-save recovery data is stored in a local temporary file; it
is never sent to the MOO. The mode also opts its buffers out of
`auto-save-visited-mode`. Only an explicit save writes verb code back through
WebDAV.

The package starts Eglot for verb buffers and implements the two client hooks
used by `moo-lsp-rs`:

- `lambdamoo/readDocument` returns an open buffer snapshot or fetches the URI
  through WebDAV.
- `lambdamoo/canonicalizeDocument` changes an alias such as `/owned/525/...`
  to its canonical `/object/525/...` visited name. Dirty buffers defer the
  change until after saving or reverting.

## Tests

```sh
make test
```

GitHub Actions runs the tests and warning-free byte compilation on Emacs 29.1,
Emacs 30.2, and an Emacs development snapshot for every push and pull request.
A weekly scheduled run installs the latest Eglot and its dependencies from GNU
ELPA to detect upstream compatibility problems.
