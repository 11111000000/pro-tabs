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
;; (require 'cl-lib)                       ; cl-mapcar, cl-some, … ; already required above
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

(defcustom pro-tabs-tab-bar-darken-percent 50
  "How much darker than `default' the tab-bar track should be."
  :type 'integer :group 'pro-tabs)

(defcustom pro-tabs-setup-keybindings t
  "When non-nil, pro-tabs will install its default keybindings (s-0…s-9)
for quick tab selection.  Set to nil before loading =pro-tabs' if you
prefer to manage those bindings yourself or if they conflict with
existing ones."
  :type 'boolean
  :group 'pro-tabs)

(defcustom pro-tabs-enable-waves t
  "Render wave separators. Disable to reduce CPU usage in heavy redisplay."
  :type 'boolean :group 'pro-tabs)

(defcustom pro-tabs-tab-line-wave-threshold 40
  "When the number of tab-line tabs in a window exceeds this value, wave separators are disabled for tab-line to reduce redisplay cost."
  :type 'integer :group 'pro-tabs)

(defcustom pro-tabs-tab-line-icons-threshold 60
  "When the number of tab-line tabs in a window exceeds this value, icons are disabled for tab-line to reduce redisplay cost."
  :type 'integer :group 'pro-tabs)

(defvar pro-tabs--generation 0
  "Monotonic generation counter for event-driven cache invalidation.")

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
              (ignore-errors
                (all-the-icons-alltheicon "terminal"
                                          :height 0.75
                                          :v-adjust 0.03)))
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
                     (fallback (or (ignore-errors (all-the-icons-octicon "file" :height 0.75 :v-adjust 0.05))
                                   (ignore-errors (all-the-icons-octicon "file-text" :height 0.75  :v-adjust 0.07))
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

(defvar pro-tabs--color-blend-cache (make-hash-table :test 'equal)
  "Cache for blended colors between FACE1 and FACE2.")

(defun pro-tabs--safe-interpolated-color (face1 face2)
  "Return the blended color between FACE1 and FACE2, as #RRGGBB or \"None\".
Uses cache for performance."
  (let* ((key (list face1 face2))
         (cached (gethash key pro-tabs--color-blend-cache)))
    (if cached
        cached
      (let* ((c1 (pro-tabs--safe-face-background face1))
             (c2 (pro-tabs--safe-face-background face2))
             (val (condition-case nil
                      (if (and (not (equal c1 "None"))
                               (not (equal c2 "None")))
                          (apply 'color-rgb-to-hex
                                 (cl-mapcar (lambda (a b) (/ (+ a b) 2))
                                            (color-name-to-rgb c1)
                                            (color-name-to-rgb c2)))
                        "None")
                    (error "None"))))
        (puthash key val pro-tabs--color-blend-cache)
        val))))


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
      (if (memq (car spec) '(tab-bar tab-line))
          (set-face-attribute (car spec) nil
                              :inherit (cdr spec)
                              :box nil)
        (set-face-attribute (car spec) nil
                            :inherit (cdr spec)
                            :box nil
                            :background 'unspecified)))))

;; Theme tracking and dynamic recomputation of faces
(defvar pro-tabs--theme-tracking-installed nil
  "Internal flag to prevent double-installing theme tracking.")

(defun pro-tabs--refresh-faces (&rest _)
  "Recompute and apply pro-tabs faces based on the current theme.
Also rebuild cached color blends and wave image specs."
  (let* ((def-bg (or (face-background 'default nil t)
                     "#777777"))
         (bar-bg (or (ignore-errors (color-darken-name def-bg pro-tabs-tab-bar-darken-percent))
                     def-bg))
         (inactive-mix (ignore-errors
                         (apply 'color-rgb-to-hex
                                (cl-mapcar (lambda (a b) (/ (+ a b) 2.0))
                                           (color-name-to-rgb bar-bg)
                                           (color-name-to-rgb def-bg)))))
         (inactive-bg (or inactive-mix bar-bg)))
    (when (fboundp 'face-spec-set)
      (face-spec-set 'pro-tabs-face
                     `((t :background ,bar-bg))
                     'face-defface-spec)
      (face-spec-set 'pro-tabs-active-face
                     `((t :inherit pro-tabs-face :background ,def-bg))
                     'face-defface-spec)
      (face-spec-set 'pro-tabs-inactive-face
                     `((t :inherit pro-tabs-face :background ,inactive-bg))
                     'face-defface-spec)
      (face-spec-set 'tab-bar
                     `((t :inherit pro-tabs-face :background ,bar-bg))
                     'face-defface-spec)
      (face-spec-set 'tab-line
                     `((t :inherit pro-tabs-face :background ,bar-bg))
                     'face-defface-spec))
    ;; Reapply inheritance to built-in faces and refresh UI
    (pro-tabs--inherit-builtins)
    (pro-tabs--clear-caches)
    (pro-tabs--precompute-waves)
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



;;;###autoload
(defun pro-tabs-refresh ()
  "Manually recompute pro-tabs faces from current theme and refresh UI."
  (interactive)
  (pro-tabs--refresh-faces))


;; -------------------------------------------------------------------
;; Helper: keep tab-bar visible on every frame -----------------------
;; -------------------------------------------------------------------
(defun pro-tabs--enable-tab-bar-on-frame (frame &rest _ignore)
  "Enable `tab-bar-mode' on newly created FRAME."
  (with-selected-frame frame
    (tab-bar-mode 1)))

;; -------------------------------------------------------------------
;; Caches for images and icons
;; -------------------------------------------------------------------
(defvar pro-tabs--wave-image-cache (make-hash-table :test 'equal)
  "Internal cache for wave image display specs. Keys are (DIR H C0 C1 MIX).")

(defvar pro-tabs--wave-token-cache (make-hash-table :test 'equal)
  "Cache of pre-propertized single-space strings for wave display specs.")

(defvar pro-tabs--icon-cache-by-buffer (make-hash-table :test 'eq :weakness 'key)
  "Internal cache for icons per buffer. Weak keys so dead buffers are collected.")

(defvar pro-tabs--icon-cache-by-mode (make-hash-table :test 'equal)
  "Internal cache for icons per (MODE . BACKEND).")

(defvar pro-tabs--format-cache (make-hash-table :test 'equal)
  "Short-term cache for formatted tab strings: key -> (STRING . TIMESTAMP).")

(defun pro-tabs--clear-format-cache ()
  "Clear formatted string cache."
  (clrhash pro-tabs--format-cache))

(defun pro-tabs--bump-generation ()
  "Increment generation counter and clear format cache."
  (setq pro-tabs--generation (1+ pro-tabs--generation))
  (pro-tabs--clear-format-cache))

(defun pro-tabs--on-buffer-list-update (&rest _)
  "Hook: buffer list changed."
  (pro-tabs--bump-generation))

(defun pro-tabs--on-window-selectionchange (&rest _)
  "Hook: selected window changed."
  (pro-tabs--bump-generation))

(defun pro-tabs--on-window-config-change (&rest _)
  "Hook: window configuration changed."
  (pro-tabs--bump-generation))

(defconst pro-tabs--wave-template
  [ "21111111111"
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
    "00000000002"]
  "Base 16-row template for wave shapes; will be resampled to requested height.")

(defun pro-tabs--wave--lines (height mirror)
  "Return list of strings representing wave rows resampled to HEIGHT.
If MIRROR is non-nil, horizontally flip each row."
  (let ((lines nil)
        (len (length pro-tabs--wave-template)))
    (dotimes (i height)
      (let* ((orig (aref pro-tabs--wave-template (floor (* i (/ (float len) height)))))
             (row  (if mirror
                       (apply #'string (nreverse (string-to-list orig)))
                     orig)))
        (push row lines)))
    (nreverse lines)))

(defvar pro-tabs--precalculated-waves nil
  "Precalculated most frequent wave image specs for (backend state dir height).")

(defun pro-tabs--precompute-waves ()
  "Precompute and cache image specs for common tab states, directions and heights.
Populates `pro-tabs--precalculated-waves'."
  (let* ((heights `((tab-bar . ,(1+ pro-tabs-tab-bar-height))
                    (tab-line . ,(1+ pro-tabs-tab-line-height))))
         (backends '(tab-bar tab-line))
         (dirs '(left right))
         (states '((active . pro-tabs-active-face)
                   (inactive . pro-tabs-inactive-face)))
         table)
    (dolist (backend backends)
      (dolist (dir dirs)
        (dolist (state states)
          (let* ((height (alist-get backend heights))
                 (face1 (if (eq backend 'tab-bar)
                            (if (eq dir 'left)
                                (cdr state) ; foreground
                              'tab-bar)   ; background
                          (if (eq dir 'left)
                              (cdr state)
                            'tab-line)))
                 (face2 (if (eq backend 'tab-bar)
                            (if (eq dir 'left)
                                'tab-bar
                              (cdr state))
                          (if (eq dir 'left)
                              'tab-line
                            (cdr state))))
                 (key (list backend (car state) dir height))
                 ;; image spec, call original function
                 (spec (pro-tabs--wave-image-spec dir face1 face2 height)))
            (push (cons key spec) table)))))
    (setq pro-tabs--precalculated-waves (nreverse table))))

(defun pro-tabs--find-precalculated-wave (backend state dir height)
  "Lookup precomputed wave spec or fallback."
  (alist-get (list backend state dir height) pro-tabs--precalculated-waves nil nil #'equal))

(defun pro-tabs--wave-image-spec (dir face1 face2 &optional height)
  "Return cached display spec for wave separator.
If precomputed, use quick lookup."
  (let* ((backend (cond
                   ((eq face1 'tab-bar) 'tab-bar)
                   ((eq face1 'tab-line) 'tab-line)
                   ((eq face2 'tab-bar) 'tab-bar)
                   ((eq face2 'tab-line) 'tab-line)
                   ;; do not infer backend from pro-tabs faces; they are shared
                   (t nil)))
         (state (cond
                 ((eq face1 'pro-tabs-active-face) 'active)
                 ((eq face1 'pro-tabs-inactive-face) 'inactive)
                 ((eq face2 'pro-tabs-active-face) 'active)
                 ((eq face2 'pro-tabs-inactive-face) 'inactive)
                 (t nil)))
         (h (or height (frame-char-height)))
         (try (and backend state (pro-tabs--find-precalculated-wave backend state dir h))))
    (or try
        ;; Fallback as before
        (let* ((mirror (eq dir 'right))
               ;; Palettes based on direction (swap to fix negative/inverted colors)
               (c0 (if (eq dir 'left)
                       (pro-tabs--safe-face-background face1)
                     (pro-tabs--safe-face-background face2)))
               (c1 (if (eq dir 'left)
                       (pro-tabs--safe-face-background face2)
                     (pro-tabs--safe-face-background face1)))
               (mix (if (eq dir 'left)
                        (pro-tabs--safe-interpolated-color face2 face1)
                      (pro-tabs--safe-interpolated-color face1 face2)))
               (face-for-image (if (eq dir 'left) face2 face1))
               (key (list dir h c0 c1 mix))
               (cached (gethash key pro-tabs--wave-image-cache)))
          (if cached
              cached
            (let* ((lines (pro-tabs--wave--lines h mirror))
                   (xpm (concat
                         "/* XPM */\nstatic char * wave_xpm[] = {\n"
                         (format "\"11 %d 3 1\", " h)
                         "\"0 c " c0
                         "\", \"1 c " c1
                         "\", \"2 c " mix
                         "\",\n"
                         (mapconcat (lambda (l) (format "\"%s\"," l)) lines "\n")
                         "\"};\n"))
                   (img (create-image xpm 'xpm t :ascent 'center))
                   (spec (list 'image :type 'xpm
                               :data (plist-get (cdr img) :data)
                               :ascent 'center
                               :face face-for-image)))
              (puthash key spec pro-tabs--wave-image-cache)
              spec))))))

(defun pro-tabs--clear-caches ()
  "Clear internal caches used by pro-tabs rendering."
  (clrhash pro-tabs--wave-image-cache)
  (clrhash pro-tabs--wave-token-cache)
  (clrhash pro-tabs--icon-cache-by-buffer)
  (clrhash pro-tabs--icon-cache-by-mode)
  (clrhash pro-tabs--color-blend-cache)
  (pro-tabs--clear-format-cache)
  (setq pro-tabs--precalculated-waves nil))

(defun pro-tabs--wave-left (face1 face2 &optional height)
  "Return left wave XPM separator (pure function, FOR TAB-BAR)."
  (pro-tabs--wave-image-spec 'left face1 face2 height))

(defun pro-tabs--wave-right (face1 face2 &optional height)
  "Return right wave XPM separator (mirror of left, FOR TAB-BAR)."
  (pro-tabs--wave-image-spec 'right face1 face2 height))

(defun pro-tabs--wave-token-left (face1 face2 &optional height)
  "Return cached pre-propertized token (single space) for left wave."
  (let* ((h (or height (frame-char-height)))
         (key (list 'left face1 face2 h))
         (tok (gethash key pro-tabs--wave-token-cache)))
    (or tok
        (let* ((spec (pro-tabs--wave-left face1 face2 h))
               (s (propertize " " 'display spec)))
          (puthash key s pro-tabs--wave-token-cache)
          s))))

(defun pro-tabs--wave-token-right (face1 face2 &optional height)
  "Return cached pre-propertized token (single space) for right wave."
  (let* ((h (or height (frame-char-height)))
         (key (list 'right face1 face2 h))
         (tok (gethash key pro-tabs--wave-token-cache)))
    (or tok
        (let* ((spec (pro-tabs--wave-right face1 face2 h))
               (s (propertize " " 'display spec)))
          (puthash key s pro-tabs--wave-token-cache)
          s))))

(defun pro-tabs--icon (buffer-or-mode backend)
  "Return cached icon for BUFFER-OR-MODE and BACKEND.
Silences messages during provider calls and protects against provider errors."
  (when pro-tabs-enable-icons
    (let ((inhibit-message t)) ; some providers or deps may call `message'
      (if (bufferp buffer-or-mode)
          (or (gethash buffer-or-mode pro-tabs--icon-cache-by-buffer)
              (let ((val (cl-some (lambda (fn)
                                    (condition-case nil
                                        (funcall fn buffer-or-mode backend)
                                      (error nil)))
                                  pro-tabs-icon-functions)))
                (puthash buffer-or-mode val pro-tabs--icon-cache-by-buffer)
                val))
        (let* ((key (cons buffer-or-mode backend)))
          (or (gethash key pro-tabs--icon-cache-by-mode)
              (let ((val (cl-some (lambda (fn)
                                    (condition-case nil
                                        (funcall fn buffer-or-mode backend)
                                      (error nil)))
                                  pro-tabs-icon-functions)))
                (puthash key val pro-tabs--icon-cache-by-mode)
                val)))))))

(defun pro-tabs--shorten (str len)
  (if (> (length str) len)
      (concat (substring str 0 len) "…") str))

;; -------------------------------------------------------------------
;; Unified format
;; -------------------------------------------------------------------
(defun pro-tabs--format-internal (backend item &optional _index)
  "Pure formatter used by the caching wrapper."
  (pcase backend
    ('tab-bar
     (let* ((current? (eq (car item) 'current-tab))
            (bufname  (substring-no-properties (alist-get 'name item)))
            (face     (if current? 'pro-tabs-active-face 'pro-tabs-inactive-face))
            (h        pro-tabs-tab-bar-height)
            (icon     (pro-tabs--icon (get-buffer bufname) 'tab-bar))
            (wave-r   (if pro-tabs-enable-waves
                          (pro-tabs--wave-token-right face 'tab-bar (+ 1 h))
                        " "))
            (wave-l   (if pro-tabs-enable-waves
                          (pro-tabs--wave-token-left 'tab-bar face (+ 1 h))
                        " "))
            (name     (pro-tabs--shorten bufname pro-tabs-max-name-length))
            (txt      (concat wave-r (or icon "") " " name wave-l)))
       (add-face-text-property 0 (length txt) face t txt) txt))

    (_                                  ; tab-line
     (let* ((buffer    item)
            (win       (selected-window))
            (count     (or (window-parameter win 'pro-tabs--tab-line-count) 0))
            (many      (or (window-parameter win 'pro-tabs--tab-line-many) nil))
            (current?  (eq buffer (window-buffer win)))
            (face      (if current? 'pro-tabs-active-face 'pro-tabs-inactive-face))
            (h         pro-tabs-tab-line-height)
            (waves?    (and pro-tabs-enable-waves (not many)))
            (icons?    (and pro-tabs-enable-icons
                            (or (not (numberp pro-tabs-tab-line-icons-threshold))
                                (<= pro-tabs-tab-line-icons-threshold 0)
                                (< count pro-tabs-tab-line-icons-threshold))))
            (icon      (and icons? (pro-tabs--icon buffer 'tab-line)))
            (wave-r    (if waves?
                           (pro-tabs--wave-token-right 'tab-line face (+ 1 h))
                         " "))
            (wave-l    (if waves?
                           (pro-tabs--wave-token-left face 'tab-line (+ 1 h))
                         " "))
            (name      (pro-tabs--shorten (buffer-name buffer) pro-tabs-max-name-length))
            (txt       (concat wave-r (or icon "") " " name wave-l)))
       (add-face-text-property 0 (length txt) face t txt) txt))))

(defun pro-tabs--format (backend item &optional _index)
  "Return formatted tab for BACKEND with event-driven caching.
BACKEND ∈ {'tab-bar,'tab-line}. ITEM is alist(tab) or buffer."
  (let* ((key (if (eq backend 'tab-bar)
                  (let* ((current? (eq (car item) 'current-tab))
                         (bufname (alist-get 'name item)))
                    (vector backend bufname current?
                            pro-tabs-enable-icons pro-tabs-enable-waves
                            pro-tabs-max-name-length pro-tabs-tab-bar-height))
                (let* ((buffer item)
                       (win (selected-window))
                       (current? (eq buffer (window-buffer win)))
                       (bufname (buffer-name buffer))
                       (many (or (window-parameter win 'pro-tabs--tab-line-many) nil)))
                  (vector backend win bufname current? many
                          pro-tabs-enable-icons pro-tabs-enable-waves
                          pro-tabs-max-name-length pro-tabs-tab-line-height))))
         (val (gethash key pro-tabs--format-cache)))
    (or val
        (let ((txt (pro-tabs--format-internal backend item _index)))
          (puthash key txt pro-tabs--format-cache)
          txt))))

(defun pro-tabs-format-tab-bar (tab idx)
  "Wrapper for =tab-bar-tab-name-format-function'."
  (pro-tabs--format 'tab-bar tab idx))

(defun pro-tabs-format-tab-line (buffer &optional _buffers)
  "Wrapper for =tab-line-tab-name-function'."
  (pro-tabs--format 'tab-line buffer))

;; -------------------------------------------------------------------
;; Fast tab list for tab-line (no seq, per-window cache, event-driven)
;; -------------------------------------------------------------------
(defun pro-tabs--same-family-mode-p (m1 m2)
  "Return non-nil if major mode M1 is M2 or derived from it."
  (or (eq m1 m2)
      (and (fboundp 'provided-mode-derived-p)
           (provided-mode-derived-p m1 m2))))

(defun pro-tabs-tabs-function-fast ()
  "Return buffers for tab-line in current window, cached per generation.
Mimics `tab-line-tabs-mode-buffers' but avoids seq/sort/uniq on redisplay."
  (let* ((win (selected-window))
         (curr (window-buffer win))
         (curr-mode (buffer-local-value 'major-mode curr))
         (gen pro-tabs--generation)
         (cache (window-parameter win 'pro-tabs--tabs-cache)))
    (if (and cache (eq (plist-get cache :gen) gen))
        (plist-get cache :tabs)
      (let ((tabs nil) (count 0))
        (dolist (b (buffer-list))
          (let ((name (buffer-name b)))
            (when (and name (> (length name) 0)
                       (not (eq (aref name 0) ?\s)))
              (let ((mm (buffer-local-value 'major-mode b)))
                (when (pro-tabs--same-family-mode-p mm curr-mode)
                  (push b tabs)
                  (setq count (1+ count)))))))
        (setq tabs (nreverse tabs))
        (set-window-parameter win 'pro-tabs--tab-line-count count)
        (set-window-parameter
         win 'pro-tabs--tab-line-many
         (and (numberp pro-tabs-tab-line-wave-threshold)
              (> pro-tabs-tab-line-wave-threshold 0)
              (> count pro-tabs-tab-line-wave-threshold)))
        (set-window-parameter win 'pro-tabs--tabs-cache (list :gen gen :tabs tabs))
        tabs))))

(defun pro-tabs--tab-line-cache-key (&rest args)
  "Key for Emacs 29+ tab-line cache; stable until generation or window changes.
Accept any calling convention; extract WINDOW from ARGS when present."
  (let ((win (cl-some (lambda (x) (and (windowp x) x)) args)))
    (list :gen pro-tabs--generation :win (or win (selected-window)))))

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
                                             tab-line-tabs-function tab-line-tab-name-function
                                             tab-line-cache tab-line-cache-key-function))
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
        (when (boundp 'tab-line-tabs-function)    (setq tab-line-tabs-function #'pro-tabs-tabs-function-fast))
        (when (boundp 'tab-line-tab-name-function)
          (setq tab-line-tab-name-function #'pro-tabs-format-tab-line))
        (when (boundp 'tab-line-cache) (setq tab-line-cache nil))
        (when (boundp 'tab-line-cache-key-function) (setq tab-line-cache-key-function nil))

        ;; faces
        (pro-tabs--inherit-builtins)

        ;; event-driven cache invalidation
        (add-hook 'buffer-list-update-hook #'pro-tabs--on-buffer-list-update)
        (add-hook 'window-selection-change-functions #'pro-tabs--on-window-selectionchange)
        (add-hook 'window-configuration-change-hook #'pro-tabs--on-window-config-change)

        ;; s-0 … s-9 quick select (tab-bar only)
        (defvar pro-tabs-keymap (make-sparse-keymap)
          "Keymap for pro-tabs quick selection. Customizable.")
        (when pro-tabs-setup-keybindings
          (dotimes (i 10)
            (let* ((num i)
                   (k (kbd (format "s-%d" num))))
              (define-key pro-tabs-keymap k
                          (lambda () (interactive) (tab-bar-select-tab num)))))
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
                 #'pro-tabs--enable-tab-bar-on-frame)
    (remove-hook 'buffer-list-update-hook #'pro-tabs--on-buffer-list-update)
    (remove-hook 'window-selection-change-functions #'pro-tabs--on-window-selectionchange)
    (remove-hook 'window-configuration-change-hook #'pro-tabs--on-window-config-change)))

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

;;;###autoload
(defun pro-tabs-repair-tab-line ()
  "Repair tab-line when cache or functions are misconfigured.
Disables built-in tab-line cache, restores default tab functions,
clears pro-tabs window parameters, and restarts tab-line."
  (interactive)
  (let ((inhibit-redisplay t))
    (tab-line-mode -1)
    (tab-bar-mode -1)
    (when (boundp 'tab-line-cache)
      (setq tab-line-cache nil))
    (when (boundp 'tab-line-cache-key-function)
      (setq tab-line-cache-key-function nil))
    (when (boundp 'tab-line-tabs-function)
      (setq tab-line-tabs-function 'tab-line-tabs-window-buffers))
    (when (boundp 'tab-line-tab-name-function)
      (setq tab-line-tab-name-function 'tab-line-tab-name))
    (setq-default tab-line-format 'tab-line-format)
    (setq tab-line-format 'tab-line-format)
    (walk-windows
     (lambda (w)
       (set-window-parameter w 'pro-tabs--tabs-cache nil)
       (set-window-parameter w 'pro-tabs--tab-line-count nil)
       (set-window-parameter w 'pro-tabs--tab-line-many nil))
     'no-mini)
    (force-mode-line-update t))
  (tab-line-mode 1)
  (when pro-tabs-mode
    (pro-tabs-flush-caches)))

;;;###autoload
(defun pro-tabs-flush-caches ()
  "Clear pro-tabs caches and refresh UI."
  (interactive)
  (pro-tabs--clear-caches)
  (when (featurep 'tab-bar)
    (ignore-errors (tab-bar--update-tab-bar-lines)))
  (force-mode-line-update t))

;; Ensure tracking is active after load
(pro-tabs--install-theme-tracking)

(provide 'pro-tabs)
;;; pro-tabs.el ends here
