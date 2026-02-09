;;; org-typst.el --- Robust Typst previews for Org mode -*- lexical-binding: t; -*-

;; Author: Martin Bari Garnier
;; Version: 2.0.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))
;; Keywords: tools, org, latex, typst, preview
;; URL: https://github.com/MartinBaGar/org-typst

;;; Commentary:
;;
;; This package provides inline Typst equation rendering in Org mode buffers.
;;
;; Two modes of operation:
;; - org-typst-mode: Manual control via commands (C-c C-x C-t to toggle preview)
;; - org-typst-live-mode: Full automation (auto-render on idle, auto-hide on edit)
;;
;; Usage:
;;   M-x org-typst-mode          Enable manual mode
;;   M-x org-typst-live-mode     Enable automatic rendering
;;   C-c C-x C-t                 Toggle preview at cursor
;;   C-c C-x C-a                 Preview all equations in buffer
;;   C-c C-x C-c                 Clear all previews
;;
;; Equations are written as: $x^2 + y^2 = z^2$
;;
;; Global Typst definitions can be added via:
;;   #+TYPST: #let myvar = 42
;;   #+begin_src typst
;;   #let myfunc(x) = x^2
;;   #+end_src

;;; Code:

(require 'org)
(require 'sha1)
(require 'svg)
(require 'seq)

(defgroup org-typst nil
  "Typst integration for Org mode."
  :group 'tools
  :prefix "org-typst-")

;; =============================================================================
;; 1. CONFIGURATION
;; =============================================================================

(defcustom org-typst-executable "typst"
  "Path to the Typst executable."
  :type 'string
  :group 'org-typst)

(defcustom org-typst-font-family "New Computer Modern"
  "Font family for Typst fragments."
  :type 'string
  :group 'org-typst)

(defcustom org-typst-font-size 12
  "Font size in points for Typst fragments."
  :type 'integer
  :group 'org-typst)

(defcustom org-typst-margin 2
  "Margin in points for Typst page layout."
  :type 'integer
  :group 'org-typst)

(defcustom org-typst-scale 1.5
  "SVG scaling factor for display."
  :type 'float
  :group 'org-typst)

(defcustom org-typst-preview-delay 0.5
  "Idle delay in seconds before rendering in live mode."
  :type 'float
  :group 'org-typst)

(defvar org-typst-cache-directory
  (expand-file-name "org-typst-cache/" user-emacs-directory)
  "Directory for storing compiled SVG fragments.")

;; =============================================================================
;; 2. INTERNAL STATE
;; =============================================================================

(defvar-local org-typst--context-cache nil
  "Cached global Typst context (definitions, imports).")

(defvar-local org-typst--context-cache-tick nil
  "Buffer modification tick when context was last cached.")

(defvar-local org-typst--active-processes nil
  "List of active compilation processes for this buffer.")

(defvar-local org-typst--timer nil
  "Idle timer for automatic scanning in live mode.")

(defconst org-typst--math-regex "\\$\\([^$\n]+\\)\\$"
  "Regex to match inline Typst math fragments.
Captures content between single dollar signs, excluding newlines.")

;; =============================================================================
;; 3. UTILITY FUNCTIONS
;; =============================================================================

(defun org-typst--validate-setup ()
  "Check if Typst executable is available.
Signal an error if not found."
  (unless (executable-find org-typst-executable)
    (user-error "Typst executable not found: %s. Please install Typst or customize `org-typst-executable'" 
                org-typst-executable)))

(defun org-typst--definition-line-p (string)
  "Return non-nil if STRING contains a Typst definition.
Definitions are lines starting with #let, #set, #import, #show, or #include."
  (string-match-p "^[ \t]*#\\(let\\|set\\|import\\|show\\|include\\)" string))

(defun org-typst--get-preamble ()
  "Generate Typst preamble with current settings.
Returns a string containing page and text formatting directives."
  (format "#set page(width: auto, height: auto, margin: %dpt)
#set text(font: \"%s\", size: %dpt)
"
          org-typst-margin
          org-typst-font-family
          org-typst-font-size))

;; =============================================================================
;; 4. CONTEXT HARVESTING (with caching)
;; =============================================================================

(defun org-typst--harvest-context ()
  "Scan buffer for global Typst definitions.
Returns a string containing all definitions from #+TYPST: lines
and typst source blocks. Uses caching to avoid redundant scans."
  (let ((current-tick (buffer-modified-tick)))
    (unless (and org-typst--context-cache
                 (eq org-typst--context-cache-tick current-tick))
      (setq org-typst--context-cache (org-typst--harvest-context-uncached)
            org-typst--context-cache-tick current-tick))
    org-typst--context-cache))

(defun org-typst--harvest-context-uncached ()
  "Actually scan the buffer for global definitions.
Called by `org-typst--harvest-context' when cache is invalid."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((context "")
            (case-fold-search t))
        
        ;; A. Harvest #+TYPST: lines
        (while (re-search-forward "^[ \t]*#\\+TYPST:[ \t]*\\(.*\\)$" nil t)
          (let ((content (match-string-no-properties 1)))
            (when (org-typst--definition-line-p content)
              (setq context (concat context content "\n")))))
        
        ;; B. Harvest typst source blocks
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*#\\+begin_\\(?:export\\|src\\)[ \t]+typst" nil t)
          (let ((start (line-beginning-position 2))
                (end (save-excursion 
                       (re-search-forward "^[ \t]*#\\+end_" nil t) 
                       (match-beginning 0))))
            (when (and start end (> end start))
              (let ((block (buffer-substring-no-properties start end)))
                (when (org-typst--definition-line-p block)
                  (setq context (concat context block "\n")))))))
        
        context))))

;; =============================================================================
;; 5. ASYNC COMPILATION ENGINE
;; =============================================================================

(defun org-typst--compile-fragment (code context buffer beg end)
  "Compile CODE asynchronously and display result in BUFFER at BEG to END.
CONTEXT contains global definitions to prepend.
Creates an async process that compiles to SVG and displays via overlay."
  (let* ((full-source (concat (org-typst--get-preamble) "\n" context "\n" code))
         (hash (sha1 full-source))
         (image-path (expand-file-name (concat hash ".svg") org-typst-cache-directory)))
    
    (unless (file-directory-p org-typst-cache-directory)
      (make-directory org-typst-cache-directory t))

    ;; If already compiled, display immediately
    (if (file-exists-p image-path)
        (org-typst--draw-image-callback buffer beg end image-path code)
      
      ;; Otherwise, compile asynchronously
      (let* ((temp-file (make-temp-file "org-typst-" nil ".typ"))
             (err-buf (generate-new-buffer " *org-typst-stderr*"))
             (proc (make-process
                    :name "org-typst-worker"
                    :buffer nil
                    :stderr err-buf
                    :command (list org-typst-executable "compile" temp-file image-path)
                    :sentinel #'org-typst--compilation-sentinel)))
        
        (with-temp-file temp-file (insert full-source))
        
        ;; Store compilation metadata
        (process-put proc 'image-path image-path)
        (process-put proc 'temp-file temp-file)
        (process-put proc 'err-buf err-buf)
        (process-put proc 'source-code full-source)
        (process-put proc 'original-text code)
        (process-put proc 'target-buffer buffer)
        (process-put proc 'target-beg beg)
        (process-put proc 'target-end end)
        
        ;; Track active processes
        (with-current-buffer buffer
          (push proc org-typst--active-processes))))))

(defun org-typst--compilation-sentinel (proc event)
  "Handle completion of compilation process PROC with EVENT status.
If successful, displays the compiled SVG. If failed, shows error message."
  (let ((image-path (process-get proc 'image-path))
        (temp-file (process-get proc 'temp-file))
        (err-buf (process-get proc 'err-buf))
        (buffer (process-get proc 'target-buffer))
        (beg (process-get proc 'target-beg))
        (end (process-get proc 'target-end))
        (original-text (process-get proc 'original-text)))
    
    ;; Remove from active process list
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq org-typst--active-processes 
              (delq proc org-typst--active-processes))))
    
    (when (string-match-p "finished" event)
      (if (file-exists-p image-path)
          (org-typst--draw-image-callback buffer beg end image-path original-text)
        
        ;; Compilation failed - show error
        (when (buffer-live-p err-buf)
          (let ((error-msg (with-current-buffer err-buf (buffer-string))))
            (unless (string-empty-p error-msg)
              (org-typst--show-error error-msg (process-get proc 'source-code)))))))
    
    ;; Cleanup
    (when (file-exists-p temp-file) 
      (delete-file temp-file))
    (when (buffer-live-p err-buf) 
      (kill-buffer err-buf))))

(defun org-typst--show-error (error-msg source)
  "Display compilation ERROR-MSG and SOURCE in dedicated buffer."
  (with-current-buffer (get-buffer-create "*Org-Typst Error*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "=== COMPILATION FAILED ===\n\n")
      (insert error-msg)
      (insert "\n\n=== SOURCE ===\n")
      (insert source)
      (goto-char (point-min))
      (special-mode))
    (display-buffer (current-buffer)))
  (message "Org-Typst: Compilation failed. See *Org-Typst Error* buffer."))

;; =============================================================================
;; 6. DISPLAY MANAGEMENT
;; =============================================================================

(defun org-typst--draw-image-callback (buffer beg end image-path original-text)
  "Display IMAGE-PATH as overlay in BUFFER from BEG to END.
ORIGINAL-TEXT is used to verify the region hasn't changed.
Called asynchronously when compilation completes."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; Safety: check buffer hasn't been modified beyond our region
      (when (and (<= end (point-max))
                 (string-equal (buffer-substring-no-properties beg end) original-text))
        
        (remove-overlays beg end 'org-typst-overlay t)
        (let ((ov (make-overlay beg end nil nil nil))
              (img (create-image image-path 'svg nil :scale org-typst-scale)))
          (overlay-put ov 'display img)
          (overlay-put ov 'org-typst-overlay t)
          (overlay-put ov 'evaporate t))))))

(defun org-typst--clear-overlays-in-region (beg end)
  "Remove all Typst preview overlays between BEG and END."
  (remove-overlays beg end 'org-typst-overlay t))

;; =============================================================================
;; 7. PROCESS MANAGEMENT
;; =============================================================================

(defun org-typst--kill-all-processes ()
  "Kill all active compilation processes in current buffer."
  (dolist (proc org-typst--active-processes)
    (when (process-live-p proc)
      (delete-process proc)))
  (setq org-typst--active-processes nil))

;; =============================================================================
;; 8. PUBLIC COMMANDS
;; =============================================================================

(defun org-typst-toggle-current ()
  "Toggle preview of equation at cursor.
If preview exists, reveal source text.
If source text visible, render preview.
Works even when cursor is positioned on a dollar sign."
  (interactive)
  (let ((pt (point))
        (found nil)
        (overlay-here (seq-find (lambda (ov) (overlay-get ov 'org-typst-overlay))
                                (overlays-at (point)))))
    
    (if overlay-here
        ;; Remove overlay to reveal text
        (progn
          (delete-overlay overlay-here)
          (message "Org-Typst: Text revealed."))
      
      ;; Search for equation containing cursor
      (save-excursion
        (goto-char (line-beginning-position))
        (while (and (not found)
                    (re-search-forward org-typst--math-regex (line-end-position) t))
          (let ((m-beg (match-beginning 0))  ; ← CHANGED FROM 1 TO 0
                (m-end (match-end 0)))        ; ← CHANGED FROM 1 TO 0
            (when (and (>= pt m-beg) (<= pt m-end))
              (setq found t)
              (message "Org-Typst: Rendering...")
              (org-typst--compile-fragment 
               (buffer-substring-no-properties m-beg m-end)
               (org-typst--harvest-context)
               (current-buffer)
               m-beg
               m-end)))))
      
      (unless found
        (message "Cursor not inside a valid equation.")))))

(defun org-typst-preview-all ()
  "Preview all equations in entire buffer.
Only renders equations that don't already have previews."
  (interactive)
  (org-typst--scan-region (point-min) (point-max))
  (message "Org-Typst: Scanning buffer..."))

(defun org-typst-preview-visible ()
  "Preview all equations in visible window.
Only renders equations that don't already have previews."
  (interactive)
  (org-typst--scan-region (window-start) (window-end))
  (message "Org-Typst: Scanning visible area..."))

(defun org-typst-clear-all ()
  "Clear all preview overlays in buffer."
  (interactive)
  (org-typst--clear-overlays-in-region (point-min) (point-max))
  (message "Org-Typst: Cleared all previews."))

;; =============================================================================
;; 9. AUTOMATION (Scanner & Cursor Logic)
;; =============================================================================

(defun org-typst--scan-region (start end)
  "Scan region from START to END for equations needing previews.
Skips equations where cursor is currently positioned (to allow editing).
Only compiles equations that don't already have overlays."
  (when (eq major-mode 'org-mode)
    (let ((pt (point)))
      (save-excursion
        (goto-char start)
        (while (re-search-forward org-typst--math-regex end t)
          (let ((m-beg (match-beginning 0))  ; Includes opening $
                (m-end (match-end 0)))        ; Includes closing $
            
            ;; Skip if cursor is inside (allow editing)
            (unless (and (>= pt m-beg) (<= pt m-end))
              
              ;; Only compile if no overlay exists
              (unless (seq-some (lambda (ov) (overlay-get ov 'org-typst-overlay))
                                (overlays-at m-beg))
                (org-typst--compile-fragment 
                 (buffer-substring-no-properties m-beg m-end)
                 (org-typst--harvest-context)
                 (current-buffer)
                 m-beg
                 m-end)))))))))

(defun org-typst--check-cursor ()
  "Remove preview overlay if cursor is on top of it.
Allows immediate editing when cursor enters an equation.
Only active in live mode."
  (when (and (eq major-mode 'org-mode) org-typst-live-mode)
    (dolist (ov (overlays-at (point)))
      (when (overlay-get ov 'org-typst-overlay)
        (delete-overlay ov)))))

(defun org-typst--schedule-scan ()
  "Schedule a delayed scan of visible area.
Used in live mode to avoid excessive recompilation."
  (when org-typst--timer 
    (cancel-timer org-typst--timer))
  (setq org-typst--timer 
        (run-with-idle-timer org-typst-preview-delay nil 
                             (lambda ()
                               (when (buffer-live-p (current-buffer))
                                 (with-current-buffer (current-buffer)
                                   (org-typst--scan-region (window-start) (window-end))))))))

;; =============================================================================
;; 10. MINOR MODES
;; =============================================================================

;;;###autoload
(define-minor-mode org-typst-live-mode
  "Toggle automatic Typst preview rendering.
When enabled:
- Equations are automatically rendered after idle delay
- Previews hide automatically when cursor enters them
- Useful for live document editing

Requires `org-typst-mode' to be active."
  :lighter " TypstLive"
  (if org-typst-live-mode
      (progn
        ;; Ensure base mode is active
        (unless org-typst-mode
          (org-typst-mode 1))
        
        ;; Enable auto-render on idle
        (add-hook 'post-command-hook #'org-typst--schedule-scan nil t)
        
        ;; Enable auto-hide on cursor
        (add-hook 'post-command-hook #'org-typst--check-cursor nil t)
        
        (message "Org-Typst Live: Auto-render and auto-hide enabled."))
    
    ;; Disable automation
    (remove-hook 'post-command-hook #'org-typst--schedule-scan t)
    (remove-hook 'post-command-hook #'org-typst--check-cursor t)
    (when org-typst--timer 
      (cancel-timer org-typst--timer)
      (setq org-typst--timer nil))
    
    (message "Org-Typst Live: Automation disabled.")))

;;;###autoload
(define-minor-mode org-typst-mode
  "Toggle Typst preview support in Org mode.
Enables manual commands for rendering Typst equations:
- \\[org-typst-toggle-current]: Toggle preview at cursor
- \\[org-typst-preview-all]: Preview all equations
- \\[org-typst-clear-all]: Clear all previews

For automatic rendering, enable `org-typst-live-mode'."
  :lighter " Typst"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-x C-t") #'org-typst-toggle-current)
            (define-key map (kbd "C-c C-x C-a") #'org-typst-preview-all)
            (define-key map (kbd "C-c C-x C-c") #'org-typst-clear-all)
            map)
  
  (if org-typst-mode
      (progn
        ;; Validate setup on activation
        (org-typst--validate-setup)
        (message "Org-Typst enabled. Toggle preview: C-c C-x C-t | Auto mode: M-x org-typst-live-mode"))
    
    ;; Cleanup on deactivation
    (org-typst-live-mode -1)
    (org-typst--kill-all-processes)
    (org-typst-clear-all)
    (setq org-typst--context-cache nil
          org-typst--context-cache-tick nil)))

(provide 'org-typst)

;;; org-typst.el ends here
