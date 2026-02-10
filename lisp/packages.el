;;; packages.el --- package.el setup -*- lexical-binding: t; -*-

(require 'package)

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
		("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(defun wf/ensure-package (pkg)
  "Install PKG unless already installed."
  (unless (package-installed-p pkg)
    (package-install pkg)))

(provide 'packages)
