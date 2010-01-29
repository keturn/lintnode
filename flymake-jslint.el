;; adapted from http://www.emacswiki.org/emacs/FlymakeJavaScript
;;
;; Installation:
;;
;; Put this in your load-path, then add the following to your .emacs.
;; You substitude espresso-mode-hook for javascript-mode-hook if you
;; use espresso.
;;
;;     (require 'flymake-jslint)
;;     (add-hook 'javascript-mode-hook
;;         (lambda () (flymake-mode t)))
;;
;; Do M-x customize-group flymake-jslint to customize paths and port.
;;
;; Run M-x lintnode-start once to start the server before invoking flymake.

(require 'flymake)

(defcustom lintnode-node-program "node"
  "The program name to invoke node.js."
  :type 'string
  :group 'flymake-jslint)

(defcustom lintnode-location "~/.emacs.d/lintnode"
  "The directory lintnode's app.js may be found in."
  :type 'string
  :group 'flymake-jslint)

(defcustom lintnode-port 3003
  "The port the lintnode server runs on."
  :type 'integer
  :group 'flymake-jslint)

(defun lintnode-start ()
  "Start the lintnode server.
Uses `lintnode-node-program' and `lintnode-location'."
  (interactive)
  (start-process "lintnode-server" "*lintnode*"
                 lintnode-node-program (concat lintnode-location "/app.js")
                 "--port" (number-to-string lintnode-port)))

(defun flymake-jslint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-inplace))
         (local-file (file-relative-name
		      temp-file
		      (file-name-directory buffer-file-name)))
         (jslint-url (format "http://127.0.0.1:%d/jslint" lintnode-port)))
    (list "curl" (list "--form" (format "source=<%s" local-file)
                       "--form" (format "filename=%s" local-file)
                       ;; FIXME: For some reason squid hates this curl invocation.
                       "--proxy" ""
                       jslint-url))))

(setq flymake-allowed-file-name-masks
      (cons '(".+\\.js$"
	      flymake-jslint-init
	      flymake-simple-cleanup
	      flymake-get-real-file-name)
	    flymake-allowed-file-name-masks))

(setq flymake-err-line-patterns 
      (cons '("^Lint at line \\([[:digit:]]+\\) character \\([[:digit:]]+\\): \\(.+\\)$"  
	      nil 1 2 3)
	    flymake-err-line-patterns))

(provide 'flymake-jslint)
