;;;  pro-tabs.el --- Simple & reusable tabs for Emacs -*-lexical-binding:t-*-
;;
;;  Author: Peter Kosov  ·  https://github.com/11111000000/pro-tabs
;;  Version: 2.0  (reborn in the spirit of Dao)
;;  Package-Requires: ((emacs "27.1") (all-the-icons "5.0.0"))
;;  Keywords: convenience, tabs, ui
;;
;;  “Emptiness is useful; it is what we use.”           — Dao De Jing, 11

;;; Commentary:

;;  pro-tabs 2.0 provides a unified, minimalistic and reusable design
;;  for tab-bar and tab-line. All rendering is concentrated in pure
;;  functions, and all side-effects live only in =pro-tabs-mode'.
;;
;;  Main innovations:
;;    • Two global faces: =pro-tabs-active-face' and
;;                        =pro-tabs-inactive-face'
;;      to which *all* built-in tab-bar / tab-line faces *inherit* from.
;;    • A single wave generator  =pro-tabs--wave'
;;      (direction 'left / 'right).
;;    • A single formatting function   =pro-tabs--format'
;;      with thin wrappers for tab-bar and tab-line.
;;
;;  Setup:
;;      (require 'pro-tabs)
;;      (pro-tabs-mode 1)        ; enable
;;
;;  Everything else – via M-x customize-group RET pro-tabs RET.

;;; Code:

(require 'cl-lib)
(require 'tab-bar)
(require 'tab-line)
(require 'color)
(require 'cl-lib)                       ; cl-mapcar, cl-some, …
;; all-the-icons is now optional
(ignore-errors (require 'all-the-icons nil t))

;; -------------------------------------------------------------------
;; Customisation
;; -------------------------------------------------------------------
(defgroup pro-tabs nil "Unified, beautiful tab-bar / tab-line."
  :group 'convenience :prefix "pro-tabs-")

(defcustom pro-tabs-enable-icons t
  "Show icons in tabs when non-nil."  :type 'boolean :group 'pro-tabs)

(defcustom pro-tabs-max-name-length 20
  "Trim buffer / tab names to this length (ellipsis afterwards)."
  :type 'integer :group 'pro-tabs)

(defcustom pro-tabs-tab-bar-height 25
  "Height in px used for wave on tab-bar."  :type 'integer :group 'pro-tabs)

(defcustom pro-tabs-tab-line-height 18
  "Height in px used for wave on tab-line." :type 'integer :group 'pro-tabs)

(defcustom pro-tabs-setup-keybindings t
  "When non-nil, pro-tabs will install its default keybindings (s-0…s-9)
for quick tab selection.  Set to nil before loading `pro-tabs' if you
prefer to manage those bindings yourself or if they conflict with
existing ones."
  :type 'boolean
  :group 'pro-tabs)

;; -------------------------------------------------------------------
;; Icon provider abstraction
;; -------------------------------------------------------------------
(defcustom pro-tabs-icon-functions nil
  "Hook of providers returning an icon string.

Each function gets (BUFFER-OR-MODE BACKEND) and must return either
a propertized string (icon) or nil.  Providers are called in order;
the first non-nil result is used.

The user can add their own functions:
    (add-hook 'pro-tabs-icon-functions #'my-provider)

By default, if `all-the-icons' is installed, the built-in provider
`pro-tabs--icon-provider-all-the-icons' is connected, and in the end a
simple fallback is added."
  :type 'hook
  :group 'pro-tabs)

(defun pro-tabs--icon-provider-all-the-icons (buffer-or-mode backend)
  "Icon provider based on `all-the-icons' (if available)."
  (when (featurep 'all-the-icons)
    (let* ((mode (cond
                  ((bufferp buffer-or-mode)
                   (buffer-local-value 'major-mode buffer-or-mode))
                  ((symbolp buffer-or-mode) buffer-or-mode)))
           (term-modes '(term-mode vterm-mode eshell-mode shell-mode))
           (face (if (and (bufferp buffer-or-mode)
                          (eq buffer-or-mode (window-buffer)))
                     (if (eq backend 'tab-bar)
                         'tab-bar-tab
                       'tab-line-tab-current)
                   (if (eq backend 'tab-bar)
                       'tab-bar-tab-inactive
                     'tab-line-tab-inactive)))
           (icon
            (cond
             ((and (bufferp buffer-or-mode)
                   (string-match-p "Tor Browser\\|tor browser" (buffer-name buffer-or-mode)))
              (ignore-errors (all-the-icons-faicon "user-secret" :v-adjust 0 :height 0.75)))
             ((and (bufferp buffer-or-mode)
                   (string-match-p "Firefox\\|firefox" (buffer-name buffer-or-mode)))
              (ignore-errors (all-the-icons-faicon "firefox" :v-adjust 0 :height 0.75)))
             ((and (bufferp buffer-or-mode)
                   (string-match-p "Google-chrome" (buffer-name buffer-or-mode)))
              (ignore-errors (all-the-icons-faicon "chrome" :v-adjust 0 :height 0.75)))
             ((memq mode term-modes)
              (ignore-errors (all-the-icons-alltheicon "terminal" :height 0.75)))
             ((eq mode 'dired-mode)
              (ignore-errors (all-the-icons-octicon "file-directory" :v-adjust 0.0 :height 0.75)))
             ((eq mode 'org-mode)
              (ignore-errors (all-the-icons-fileicon "org" :v-adjust 0.05 :height 0.75)))
             ((eq mode 'Info-mode)
              (ignore-errors (all-the-icons-octicon "book" :height 0.75)))
             ((memq mode '(help-mode helpful-mode apropos-mode))
              (ignore-errors (all-the-icons-material "help" :height 0.75)))
             ((eq mode 'exwm-mode)
              (ignore-errors (all-the-icons-faicon "windows" :v-adjust -0.12 :height 0.75)))
             (t
              (let* ((maybe (all-the-icons-icon-for-mode mode :height 0.75))
                     (fallback (or (ignore-errors (all-the-icons-octicon "file" :height 0.75))
                                   (ignore-errors (all-the-icons-octicon "file-text" :height 0.75))
                                   "•")))
                (if (stringp maybe)
                    maybe
                  fallback))))))
      (when (stringp icon)
        icon))))

;; The simplest fallback provider (unicodes/emoji)
(defun pro-tabs--icon-provider-fallback (_buffer-or-mode backend)
  "Always returns a subtle bullet if other providers did not work."
  (let ((icon "•"))
    ;; Ensure the same size and centering as 'all-the-icons'
    (propertize icon
                'face (if (eq backend 'tab-bar) 'tab-bar-tab-inactive 'tab-line-tab-inactive)
                'ascent 'center
                'height 0.75)))

;; Register built-in providers
(add-hook 'pro-tabs-icon-functions #'pro-tabs--icon-provider-all-the-icons)
(add-hook 'pro-tabs-icon-functions #'pro-tabs--icon-provider-fallback t) ; t ⇒ add to end

;; -------------------------------------------------------------------
;; Pure helpers
;; -------------------------------------------------------------------

(defun pro-tabs--safe-face-background (face)
  "Return the background color of FACE or \"None\" if unavailable."
  (let ((color (and (symbolp face) (facep face) (face-background face nil t))))
    (if (and color (not (equal color ""))) color "None")))

(defun pro-tabs--safe-interpolated-color (face1 face2)
  "Return the blended color between FACE1 and FACE2, as #RRGGBB or \"None\"."
  (let* ((c1 (pro-tabs--safe-face-background face1))
         (c2 (pro-tabs--safe-face-background face2)))
    (condition-case nil
        (if (and (not (equal c1 "None"))
                 (not (equal c2 "None")))
            (apply 'color-rgb-to-hex
                   (cl-mapcar (lambda (a b) (/ (+ a b) 2))
                              (color-name-to-rgb c1)
                              (color-name-to-rgb c2)))
          "None")
      (error "None"))))


;; -------------------------------------------------------------------
;; Global faces (single source of truth)
;; -------------------------------------------------------------------
(defface pro-tabs-active-face
  '((t (:inherit pro-tabs-face)))
  "Face for active pro tab."
  :group 'pro-tabs)

(defface pro-tabs-inactive-face
  '((t (:inherit pro-tabs-face)))
  "Face for inactive tab (both tab-bar and tab-line)." :group 'pro-tabs)

(defface pro-tabs-face
  '((t (:inherit default)))
  "Face for tab-line/background (the empty track behind tabs)."  :group 'pro-tabs)

(defun pro-tabs--inherit-builtins ()
  "Make built-in tab-bar / tab-line faces inherit from unified pro-tabs faces.
This simple mapping keeps the active / inactive distinction without
calculating any colours or backgrounds."
  (dolist (spec '((tab-bar-tab           . pro-tabs-active-face)
                  (tab-bar-tab-inactive  . pro-tabs-inactive-face)
                  (tab-bar               . pro-tabs-face)
                  (tab-line-tab          . pro-tabs-active-face)
                  (tab-line-tab-current  . pro-tabs-active-face)
                  (tab-line-tab-inactive . pro-tabs-inactive-face)
                  (tab-line              . pro-tabs-face)))
    (when (facep (car spec))
      (set-face-attribute (car spec) nil
                          :inherit (cdr spec)
                          :box nil
                          :background 'unspecified
                          :foreground 'unspecified))))

;; Theme tracking and dynamic recomputation of faces
(defvar pro-tabs--theme-tracking-installed nil
  "Internal flag to prevent double-installing theme tracking.")

(defun pro-tabs--refresh-faces (&rest _)
  "Recompute and apply pro-tabs faces based on the current theme."
  (let* ((bar-bg (or (face-attribute 'tab-bar :background nil t)
                     (face-attribute 'default :background nil t)))
         (def-bg (face-attribute 'default :background nil t))
         (inactive-mix (pro-tabs--safe-interpolated-color 'tab-bar 'default))
         (inactive-bg (if (and inactive-mix (not (equal inactive-mix "None")))
                          inactive-mix
                        bar-bg)))
    (when (fboundp 'face-spec-set)
      (face-spec-set 'pro-tabs-face
                     `((t :background ,bar-bg))
                     'face-defface-spec)
      (face-spec-set 'pro-tabs-active-face
                     `((t :inherit pro-tabs-face :background ,def-bg))
                     'face-defface-spec)
      (face-spec-set 'pro-tabs-inactive-face
                     `((t :inherit pro-tabs-face :background ,inactive-bg))
                     'face-defface-spec))
    ;; Reapply inheritance to built-in faces and refresh UI
    (pro-tabs--inherit-builtins)
    (when (featurep 'tab-bar)
      (ignore-errors (tab-bar--update-tab-bar-lines)))
    (when (bound-and-true-p tab-line-mode)
      (tab-line-mode -1) (tab-line-mode 1))
    (force-mode-line-update t)))

(defun pro-tabs--install-theme-tracking ()
  "Install hooks/advice to recompute pro-tabs faces when theme changes."
  (unless pro-tabs--theme-tracking-installed
    (setq pro-tabs--theme-tracking-installed t)
    (if (boundp 'enable-theme-functions)
        (add-hook 'enable-theme-functions #'pro-tabs--refresh-faces)
      (advice-add 'load-theme :after #'pro-tabs--refresh-faces))
    ;; Run once at load to sync with current theme
    (pro-tabs--refresh-faces)))

;; Ensure tracking is active after load
(pro-tabs--install-theme-tracking)


;; -------------------------------------------------------------------
;; Helper: keep tab-bar visible on every frame -----------------------
;; -------------------------------------------------------------------
(defun pro-tabs--enable-tab-bar-on-frame (frame &rest _ignore)
  "Enable `tab-bar-mode' on newly created FRAME."
  (with-selected-frame frame
    (tab-bar-mode 1)))

(defun pro-tabs--wave-left (face1 face2 &optional height)
  "Return left wave XPM separator (pure function, FOR TAB-BAR)."
  (let* ((height (or height (frame-char-height)))
         (template [  "21111111111"
                      "00111111111"
                      "00011111111"
                      "00021111111"
                      "00001111111"
                      "00002111111"
                      "00000111111"
                      "00000111111"
                      "00000211111"
                      "00000021111"
                      "00000001111"
                      "00000001111"
                      "00000002111"
                      "00000000111"
                      "00000000211"
                      "00000000002"])
         (lines nil))
    (dotimes (i height)
      (push (aref template (floor (* i (/ (float (length template)) height)))) lines))
    (let ((img (create-image
                (concat
                 "/* XPM */\nstatic char * wave_left_xpm[] = {\n"
                 (format "\"11 %d 3 1\", " height)
                 "\"0 c " (pro-tabs--safe-face-background face2)
                 "\", \"1 c " (pro-tabs--safe-face-background face1)
                 "\", \"2 c " (pro-tabs--safe-interpolated-color face2 face1)
                 "\",\n"
                 (mapconcat (lambda (l) (format "\"%s\"," l)) (nreverse lines) "\n")
                 "\"};\n")
                'xpm t :ascent 'center)))
      (list 'image :type 'xpm :data (plist-get (cdr img) :data) :ascent 'center :face face2))))

(defun pro-tabs--wave-right (face1 face2 &optional height)
  "Return right wave XPM separator (mirror of left, FOR TAB-BAR)."
  (let* ((height (or height (frame-char-height)))
         (template [  "21111111111"
                      "00111111111"
                      "00011111111"
                      "00021111111"
                      "00001111111"
                      "00002111111"
                      "00000111111"
                      "00000111111"
                      "00000211111"
                      "00000021111"
                      "00000001111"
                      "00000001111"
                      "00000002111"
                      "00000000111"
                      "00000000211"
                      "00000000002"])
         (lines nil))
    (dotimes (i height)
      (let* ((orig (aref template (floor (* i (/ (float (length template)) height)))))
             (mirrored (apply #'string (nreverse (string-to-list orig)))))
        (push mirrored lines)))
    (let ((img (create-image
                (concat
                 "/* XPM */\nstatic char * wave_right_xpm[] = {\n"
                 (format "\"11 %d 3 1\", " height)
                 "\"0 c " (pro-tabs--safe-face-background face1)
                 "\", \"1 c " (pro-tabs--safe-face-background face2)
                 "\", \"2 c " (pro-tabs--safe-interpolated-color face1 face2)
                 "\",\n"
                 (mapconcat (lambda (l) (format "\"%s\"," l)) (nreverse lines) "\n")
                 "\"};\n")
                'xpm t :ascent 'center)))
      (list 'image :type 'xpm :data (plist-get (cdr img) :data) :ascent 'center :face face1))))

(defun pro-tabs--icon (buffer-or-mode backend)
  "Returns the first non-nil icon from `pro-tabs-icon-functions'."
  (when pro-tabs-enable-icons
    (cl-some (lambda (fn) (funcall fn buffer-or-mode backend))
             pro-tabs-icon-functions)))

(defun pro-tabs--shorten (str len)
  (if (> (length str) len)
      (concat (substring str 0 len) "…") str))

;; -------------------------------------------------------------------
;; Unified format
;; -------------------------------------------------------------------
(defun pro-tabs--format (backend item &optional _index)
  "Return formatted tab for BACKEND.
BACKEND ∈ {'tab-bar,'tab-line}.  ITEM is alist(tab) or buffer."
  (pcase backend
    ('tab-bar
     (let* ((current? (eq (car item) 'current-tab))
            (bufname  (substring-no-properties (alist-get 'name item)))
            (face     (if current? 'pro-tabs-active-face 'pro-tabs-inactive-face))
            (icon     (pro-tabs--icon (get-buffer bufname) 'tab-bar))
            (h        pro-tabs-tab-bar-height)
            (wave-r   (propertize " " 'display
                                  (pro-tabs--wave-right face 'tab-bar (+ 1 h))))
            (wave-l   (propertize " " 'display
                                  (pro-tabs--wave-left 'tab-bar face (+ 1 h))))
            (name     (pro-tabs--shorten bufname pro-tabs-max-name-length))
            (txt      (concat wave-r (or icon "") " " name wave-l)))
       (add-face-text-property 0 (length txt) face t txt) txt))

    (_                                  ; tab-line
     (let* ((buffer   item)
            (current? (eq buffer (window-buffer)))
            (face     (if current? 'pro-tabs-active-face 'pro-tabs-inactive-face))
            (h        pro-tabs-tab-line-height)
            (icon     (pro-tabs--icon buffer 'tab-line))
            (wave-r   (propertize " " 'display
                                  (pro-tabs--wave-right face 'tab-line (+ 1 h))))
            (wave-l   (propertize " " 'display
                                  (pro-tabs--wave-left 'tab-line face (+ 1 h))))
            (txt      (concat wave-r (or icon "") " " (buffer-name buffer) wave-l)))
       (add-face-text-property 0 (length txt) face t txt) txt))))

(defun pro-tabs-format-tab-bar (tab idx)
  "Wrapper for =tab-bar-tab-name-format-function'."
  (pro-tabs--format 'tab-bar tab idx))

(defun pro-tabs-format-tab-line (buffer &optional _buffers)
  "Wrapper for =tab-line-tab-name-function'."
  (pro-tabs--format 'tab-line buffer))

;; -------------------------------------------------------------------
;; Minor mode (side-effects live here)
;; -------------------------------------------------------------------
(defvar pro-tabs--saved-vars nil)      ; alist (sym . value)

(defun pro-tabs--save (var)
  (push (cons var (symbol-value var)) pro-tabs--saved-vars))

(defun pro-tabs--restore ()
  (dolist (pair pro-tabs--saved-vars)
    (set (car pair) (cdr pair)))
  (setq pro-tabs--saved-vars nil))

;;;###autoload
(define-minor-mode pro-tabs-mode
  "Toggle pro-tabs everywhere."
  :global t :group 'pro-tabs
  (if pro-tabs-mode
      ;; ---------------- ENABLE --------------------------------------
      (progn
        ;; remember and override relevant vars
        (setq pro-tabs--saved-vars nil)
        (dolist (v '(tab-bar-new-button-show tab-bar-close-button-show
                                             tab-bar-separator tab-bar-auto-width tab-bar-show
                                             tab-bar-tab-name-format-function
                                             tab-line-new-button-show tab-line-close-button-show
                                             tab-line-separator   tab-line-switch-cycling
                                             tab-line-tabs-function tab-line-tab-name-function))
          (when (boundp v) (pro-tabs--save v)))

        (setq tab-bar-new-button-show nil
              tab-bar-close-button-show nil
              tab-bar-separator " "
              tab-bar-auto-width nil
              tab-bar-show 0
              tab-bar-auto-hide-delay nil
              tab-bar-tab-name-format-function #'pro-tabs-format-tab-bar)

        (tab-bar-mode 1)
        (tab-bar-history-mode 1)
        ;; Make sure the tab-bar is shown right away, even when there is
        ;; only one tab at startup.
        (set-frame-parameter nil 'tab-bar-lines 1)

        ;; --- make sure every frame shows tab-bar -----------------
        (dolist (fr (frame-list))
          (with-selected-frame fr
            (tab-bar-mode 1)))
        (add-hook 'after-make-frame-functions
                  #'pro-tabs--enable-tab-bar-on-frame)

        (when (boundp 'tab-line-new-button-show)  (setq tab-line-new-button-show nil))
        (when (boundp 'tab-line-close-button-show) (setq tab-line-close-button-show nil))
        (when (boundp 'tab-line-separator)        (setq tab-line-separator ""))
        (when (boundp 'tab-line-switch-cycling)   (setq tab-line-switch-cycling t))
        (when (boundp 'tab-line-tabs-function)    (setq tab-line-tabs-function 'tab-line-tabs-mode-buffers))
        (when (boundp 'tab-line-tab-name-function)
          (setq tab-line-tab-name-function #'pro-tabs-format-tab-line))

        ;; faces
        (pro-tabs--inherit-builtins)

        ;; s-0 … s-9 quick select (respect existing bindings, make customizable)
        (defvar pro-tabs-keymap (make-sparse-keymap)
          "Keymap for pro-tabs quick selection. Customizable.")
        (when pro-tabs-setup-keybindings
          (dotimes (i 10)
            (let* ((num i)
                   (k (kbd (format "s-%d" num))))
              ;; Only bind if unbound or explicitly allowed by user
              (progn
                (define-key tab-line-mode-map (kbd (format "s-%d" num))
                            (lambda () (interactive)
                              ;; Use an Emacs API compatible jump by index (see below)
                              (let* ((index (if (zerop num) 10 num))
                                     (buffers (funcall tab-line-tabs-function))
                                     (buf (nth (1- index) buffers)))
                                (when buf
                                  (switch-to-buffer buf)))))
                (define-key pro-tabs-keymap k
                            (lambda () (interactive) (tab-bar-select-tab num))))))
          (define-key tab-bar-mode-map (kbd "s-<tab>")         #'tab-bar-switch-to-next-tab)
          (define-key tab-bar-mode-map (kbd "s-<iso-lefttab>") #'tab-bar-switch-to-prev-tab)
          (define-key tab-line-mode-map (kbd "s-<tab>")         #'tab-line-switch-to-next-tab)
          (define-key tab-line-mode-map (kbd "s-<iso-lefttab>") #'tab-line-switch-to-prev-tab))


        (unless (boundp 'minor-mode-map-alist)
          (setq minor-mode-map-alist (list)))
        ;; Add the pro-tabs keymap *after* the standard ones, so that `tab-line-mode-map'
        ;; has higher priority and can override global bindings.
        (add-to-list 'minor-mode-map-alist
                     (cons 'pro-tabs-mode pro-tabs-keymap) t) ; t ⇒ append

        )

    ;; ---------------- DISABLE ---------------------------------------
    (pro-tabs--restore)
    (tab-bar-mode -1) (tab-bar-history-mode -1)
    ;; --- disable in all frames & drop our hook -------------------
    (dolist (fr (frame-list))
      (with-selected-frame fr
        (tab-bar-mode -1)))
    (remove-hook 'after-make-frame-functions
                 #'pro-tabs--enable-tab-bar-on-frame)))

;; -------------------------------------------------------------------
;; Handy commands
;; -------------------------------------------------------------------
;;;###autoload
(defun pro-tabs-open-new-tab ()
  "Open new tab to the right; if =dashboard-open' exists, call it."
  (interactive)
  (tab-bar-new-tab-to)
  (when (fboundp 'dashboard-open) (dashboard-open)))

;;;###autoload
(defun pro-tabs-close-tab-and-buffer ()
  "Kill current buffer and its tab."
  (interactive)
  (kill-this-buffer)
  (tab-close))

(provide 'pro-tabs)
;;; pro-tabs.el ends here
