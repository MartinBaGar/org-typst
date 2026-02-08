;;; org-typst.el --- Robust Typst previews for Org mode -*- lexical-binding: t; -*-

;; Author: Martin Bari Garnier <martbari.g@gmail.com>
;; Maintainer: Martin Bari Garnier <martbari.g@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, 3D, blender
;; URL: https://github.com/MartinBaGar/blender.el

;;; Commentary:

;; blender.el provides integration between Emacs and Blender. It allows
;; you to send Python code from Emacs directly to Blender for execution,
;; control the Blender Python environment, and automate workflows.
;; Note: This is designed to work with Emacs running in WSL and Blender running on Windows.
;; Ensure Blender paths are converted using `wslpath -w` for compatibility.

;;; Code:
(defgroup org-typst nil
  "Typst integration for Org mode."
  :group 'tools
  :prefix "org-typst-")

(require 'org)
(require 'sha1)
(require 'svg)
(require 'cl-lib)

;; =============================================================================
;; 1. CONFIGURATION
;; =============================================================================

(defcustom org-typst-compiler-bin "typst"
  "Path to the Typst executable."
  :type 'string :group 'org-typst)

(defcustom org-typst-preamble
  "#set page(width: auto, height: auto, margin: 2pt)
#set text(font: \"New Computer Modern\", size: 12pt)
"
  "Preamble prepended to every fragment."
  :type 'string :group 'org-typst)

(defcustom org-typst-scale 1.5 "Scaling factor for buffer images." :type 'float)
(defcustom org-typst-echo-scale 2.0 "Scaling factor for echo area." :type 'float)
(defcustom org-typst-idle-delay 0.5 "Seconds of idleness before preview." :type 'float)

(defcustom org-typst-enable-echo-preview t
  "If non-nil, show a preview in the echo area when editing a fragment."
  :type 'boolean)

(defvar org-typst-cache-dir (expand-file-name "org-typst-cache/" user-emacs-directory))

;; =============================================================================
;; 2. THE COMPILER ENGINE (Leak-Free Version)
;; =============================================================================

(defun org-typst--collect-context ()
  "Harvest #+TYPST:, #+begin_src typst, and #+begin_export typst."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((context "")
            (case-fold-search t))
        ;; 1. #+TYPST: lines
        (while (re-search-forward "^[ \t]*#\\+TYPST:[ \t]*\\(.*\\)$" nil t)
          (setq context (concat context (match-string-no-properties 1) "\n")))
        ;; 2. Export/Src Blocks
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*#\\+begin_\\(?:export\\|src\\)[ \t]+typst" nil t)
          (let ((start (line-beginning-position 2))
                (end (save-excursion (re-search-forward "^[ \t]*#\\+end_" nil t)
                                     (match-beginning 0))))
            (when (and start end (> end start))
              (setq context (concat context (buffer-substring-no-properties start end) "\n")))))
        context))))

(defun org-typst--compile (code context callback)
  "Compile CODE with CONTEXT asynchronously using in-memory buffers.
CALLBACK is called with the path to the generated image when done."
  (let* ((hash (sha1 (concat org-typst-preamble context code)))
         (image-path (expand-file-name (concat hash ".svg") org-typst-cache-dir)))
    
    (unless (file-directory-p org-typst-cache-dir) (make-directory org-typst-cache-dir t))

    (if (file-exists-p image-path)
        (funcall callback image-path)
      
      (let* ((temp-file (make-temp-file "org-typst-" nil ".typ"))
             ;; FIX: Use a hidden buffer for stderr instead of a file
             (err-buf (generate-new-buffer " *org-typst-stderr*"))
             (full-source (concat org-typst-preamble "\n" context "\n" code))
             (proc (make-process
                    :name "org-typst-worker"
                    :buffer nil
                    :stderr err-buf ;; Pipe errors to buffer
                    :command (list org-typst-compiler-bin "compile" temp-file image-path)
                    :sentinel #'org-typst--sentinel)))
        
        (with-temp-file temp-file (insert full-source))
        (process-put proc 'image-path image-path)
        (process-put proc 'temp-file temp-file)
        (process-put proc 'err-buf err-buf) ;; Store buffer ref
        (process-put proc 'full-source full-source)
        (process-put proc 'callback callback)))))

(defun org-typst--sentinel (proc event)
  "Handle compilation for PROC after finished EVENT.
Clean up buffers and notify on error."
  (let ((image-path (process-get proc 'image-path))
        (temp-file (process-get proc 'temp-file))
        (err-buf (process-get proc 'err-buf))
        (callback (process-get proc 'callback)))
    
    (when (string-match-p "finished" event)
      (if (file-exists-p image-path)
          (funcall callback image-path)
        
        ;; ERROR HANDLING
        (when (buffer-live-p err-buf)
          (let ((error-msg (with-current-buffer err-buf (buffer-string))))
            (unless (string-empty-p error-msg)
              ;; 1. Write to *Org-Typst Error*
              (with-current-buffer (get-buffer-create "*Org-Typst Error*")
                (erase-buffer)
                (insert "=== TYPST COMPILATION FAILED ===\n")
                (insert error-msg)
                (insert "\n--- SOURCE CODE ---\n")
                (insert (process-get proc 'full-source)))
              ;; 2. Notify User
              (message "Org-Typst: Compilation failed! See *Org-Typst Error* buffer."))))))
    
    ;; CLEANUP: Crucial to prevent buffer leaks
    (when (file-exists-p temp-file) (delete-file temp-file))
    (when (buffer-live-p err-buf) (kill-buffer err-buf))))

;; =============================================================================
;; 3. THE INTERFACE
;; =============================================================================

(defvar-local org-typst--active-overlays nil)

(defun org-typst--display-overlay (beg end image-path)
  "Display an SVG image overlay from IMAGE-PATH between BEG and END.
Overlay is removed when the text is modified and has property ORG-TYPST-OVERLAY."
  (org-typst--clear-region beg end)
  (let ((ov (make-overlay beg end nil nil nil))
        (img (create-image image-path 'svg nil :scale org-typst-scale)))
    (overlay-put ov 'display img)
    (overlay-put ov 'org-typst-overlay t)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'modification-hooks '(org-typst--on-modification))))

(defun org-typst--on-modification (ov after &rest _args)
  "Delete overlay OV when text is modified (AFTER is nil)."
  (unless after (delete-overlay ov)))

(defun org-typst--clear-region (beg end)
  "Remove all ORG-TYPST-OVERLAY overlays between BEG and END."
  (remove-overlays beg end 'org-typst-overlay t))

(defun org-typst--echo-preview (image-path)
  "Display IMAGE-PATH as an SVG in the echo area with org-typst-echo-scale."
  (let ((img (create-image image-path 'svg nil :scale org-typst-echo-scale)))
    (message "%s" (propertize " " 'display img))))

;; =============================================================================
;; 4. THE LOOP
;; =============================================================================

(defvar org-typst-timer nil)
(defconst org-typst-regex "\\(\\$\\([^$]+\\)\\$\\)")

(defun org-typst-scan ()
  (interactive)
  (when (eq major-mode 'org-mode)
    (let ((beg (window-start)) (end (window-end)) (pt (point)))
      (save-excursion
        (goto-char beg)
        (while (re-search-forward org-typst-regex end t)
          (let ((m-beg (match-beginning 1))
                (m-end (match-end 1)))
            
            (let ((code (buffer-substring-no-properties m-beg m-end))
                  (cursor-inside (and (>= pt m-beg) (<= pt m-end))))
              
              (when cursor-inside (org-typst--clear-region m-beg m-end))

              (let ((context (org-typst--collect-context)))
                (if cursor-inside
                    (when org-typst-enable-echo-preview
                      (lexical-let ((p-code code) (p-ctx context))
                        (org-typst--compile p-code p-ctx #'org-typst--echo-preview)))

                  (unless (seq-some (lambda (ov) (overlay-get ov 'org-typst-overlay)) (overlays-at m-beg))
                    (lexical-let ((c-beg m-beg) (c-end m-end) (c-buf (current-buffer)) (c-ctx context))
                      (org-typst--compile code c-ctx
                                          (lambda (path)
                                            (when (buffer-live-p c-buf)
                                              (with-current-buffer c-buf
                                                (org-typst--display-overlay c-beg c-end path))))))))))))))))

(defun org-typst-schedule ()
  "Schedule a scan of Org Typst buffer after idle delay."
  (when org-typst-timer (cancel-timer org-typst-timer))
  (setq org-typst-timer (run-with-idle-timer org-typst-idle-delay nil #'org-typst-scan)))

(define-minor-mode org-typst-mode
  "Toggle live Typst previews."
  :lighter " TypstLive"
  (if org-typst-mode
      (progn (add-hook 'post-command-hook #'org-typst-schedule nil t) (org-typst-scan))
    (remove-hook 'post-command-hook #'org-typst-schedule t)
    (org-typst--clear-region (point-min) (point-max))))

(provide 'org-typst)

;;; org-typst.el ends here
