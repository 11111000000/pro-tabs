;;;  pro-tabs.el --- Simple & reusable tabs for Emacs -*-lexical-binding:t-*-
;;
;;  Author: Peter Kosov  ·  https://github.com/11111000000/pro-tabs
;;  Version: 2.0  (reborn in the spirit of Dao)
;;  Package-Requires: ((emacs "27.1") (all-the-icons "5.0.0"))
;;  Keywords: convenience, tabs, ui
;;
;;  “Пустота полезна, именно ею пользуемся.”           — 道德经, 11

;;; Commentary:

;;  pro-tabs 2.0 предоставляет единообразное, минималистичное и
;;  переиспользуемое оформление tab-bar и tab-line.  Весь рендеринг
;;  сосредоточен в чистых функциях, a side-effects живут только в
;;  =pro-tabs-mode'.
;;
;;  Главное новшество:
;;    • два глобальных фейса  =pro-tabs-active-face'   и
;;                             =pro-tabs-inactive-face'
;;      на которые *наследуются* все штатные лица tab-bar / tab-line.
;;    • единый генератор волны  =pro-tabs--wave'  (direction 'left / 'right).
;;    • единая функция форматирования    =pro-tabs--format'
;;      с thin wrappers для tab-bar и tab-line.
;;
;;  Настройка:
;;      (require 'pro-tabs)
;;      (pro-tabs-mode 1)        ; включить
;;
;;  Всё остальное – через M-x customize-group RET pro-tabs RET.

;;; Code:

(require 'cl-lib)
(require 'tab-bar)
(require 'tab-line)
(require 'color)
(require 'all-the-icons)

;; -------------------------------------------------------------------
;; Customisation
;; -------------------------------------------------------------------
(defgroup pro-tabs nil "Unified, beautiful tab-bar / tab-line."
  :group 'convenience :prefix "pro-tabs-")

(defcustom pro-tabs-enable-icons t
  "Show icons in tabs when non-nil."  :type 'boolean :group 'pro-tabs)

(defcustom pro-tabs-max-name-length 25
  "Trim buffer / tab names to this length (ellipsis afterwards)."
  :type 'integer :group 'pro-tabs)

(defcustom pro-tabs-tab-bar-height 25
  "Height in px used for wave on tab-bar."  :type 'integer :group 'pro-tabs)

(defcustom pro-tabs-tab-line-height 21
  "Height in px used for wave on tab-line." :type 'integer :group 'pro-tabs)

;; -------------------------------------------------------------------
;; Global faces (single source of truth)
;; -------------------------------------------------------------------
(defface pro-tabs-active-face
  '((t :background unspecified :inherit default))
  "Face for active tab (both tab-bar and tab-line): same background as Emacs default."  :group 'pro-tabs)

(defface pro-tabs-inactive-face
  ;; Цвет будет переустанавливаться динамически
  '((t :inherit default :background "grey"))
  "Face for inactive tab (both tab-bar and tab-line): a bit lighter than the darkest possible tab-bar color." :group 'pro-tabs)

(defface pro-tabs-face
  '((t :background "DarkGrey" :inherit default))
  "Face for tab-line background (the empty track behind tabs)."  :group 'pro-tabs)

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
  "Return a correct icon string for BUFFER-OR-MODE (buffer or major-mode symbol).
BACKEND is 'tab-bar or 'tab-line. Returns a string (propertized with face when for tab-line) or nil."
  (when pro-tabs-enable-icons
    (let* ((mode (cond
                  ((bufferp buffer-or-mode)
                   (buffer-local-value 'major-mode buffer-or-mode))
                  ((symbolp buffer-or-mode) buffer-or-mode)
                  (t nil)))
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
             ;; explicit buffer name overrides
             ((and (bufferp buffer-or-mode)
                   (string-match-p "Firefox\\|firefox" (buffer-name buffer-or-mode)))
              (all-the-icons-faicon "firefox"))
             ((and (bufferp buffer-or-mode)
                   (string-match-p "Google-chrome" (buffer-name buffer-or-mode)))
              (all-the-icons-faicon "chrome"))
             ((memq mode term-modes)
              (all-the-icons-alltheicon "terminal"))
             ((eq mode 'dired-mode)
              (all-the-icons-octicon "file-directory" :height 0.95 :v-adjust 0.0))
             ((eq mode 'org-mode)
              (all-the-icons-fileicon "org" :v-adjust 0.05))
             ((eq mode 'Info-mode)
              (all-the-icons-octicon "book"))
             ((memq mode '(help-mode helpful-mode apropos-mode))
              (all-the-icons-material "help"))
             (t
              (let ((maybe (all-the-icons-icon-for-mode mode)))
                (if (stringp maybe)
                    maybe
                  (all-the-icons-octicon "file")))))))
      (when (stringp icon)
        (if (eq backend 'tab-line)
            (propertize icon 'face face)
          icon)))))

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
            (txt      (concat wave-r " " (or icon "") " " name " " wave-l)))
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
            (txt      (concat wave-r " " (or icon "") " " (buffer-name buffer) " " wave-l)))
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
(defvar pro-tabs--saved-keys nil)      ; hash key → binding

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
                    tab-bar-separator tab-bar-auto-width
                    tab-bar-tab-name-format-function
                    tab-line-new-button-show tab-line-close-button-show
                    tab-line-separator   tab-line-switch-cycling
                    tab-line-tabs-function tab-line-tab-name-function))
          (when (boundp v) (pro-tabs--save v)))

        (setq tab-bar-new-button-show nil
              tab-bar-close-button-show nil
              tab-bar-separator " "
              tab-bar-auto-width nil
              tab-bar-tab-name-format-function #'pro-tabs-format-tab-bar)

        (tab-bar-mode 1) (tab-bar-history-mode 1)

        (when (boundp 'tab-line-new-button-show)  (setq tab-line-new-button-show nil))
        (when (boundp 'tab-line-close-button-show) (setq tab-line-close-button-show nil))
        (when (boundp 'tab-line-separator)        (setq tab-line-separator ""))
        (when (boundp 'tab-line-switch-cycling)   (setq tab-line-switch-cycling t))
        (when (boundp 'tab-line-tabs-function)    (setq tab-line-tabs-function 'tab-line-tabs-mode-buffers))
        (when (boundp 'tab-line-tab-name-function)
          (setq tab-line-tab-name-function #'pro-tabs-format-tab-line))

        ;; faces
        (pro-tabs--inherit-builtins)

        ;; s-0 … s-9 quick select
        (setq pro-tabs--saved-keys (make-hash-table))
        (dotimes (i 10)
          (let* ((num i)
                 (k (kbd (format "s-%d" num))))
            (puthash k (lookup-key global-map k) pro-tabs--saved-keys)
            (global-set-key k
                            (lambda () (interactive)
                              (tab-bar-select-tab num)))))

        ;; Convenience: show tab-line automatically in vterm / telega
        (add-hook 'vterm-mode-hook   #'tab-line-mode)
        (add-hook 'telega-mode-hook  #'tab-line-mode))

    ;; ---------------- DISABLE ---------------------------------------
    (pro-tabs--restore)
    (tab-bar-mode -1) (tab-bar-history-mode -1)

    ;; restore keys
    (when (hash-table-p pro-tabs--saved-keys)
      (maphash (lambda (k v) (global-set-key k v)) pro-tabs--saved-keys)
      (setq pro-tabs--saved-keys nil))

    (remove-hook 'vterm-mode-hook   #'tab-line-mode)
    (remove-hook 'telega-mode-hook  #'tab-line-mode)))

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
  (kill-this-buffer) (tab-close))

(provide 'pro-tabs)
;;; pro-tabs.el ends here
