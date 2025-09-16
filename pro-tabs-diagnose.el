;;; pro-tabs-diagnose.el --- Быстрая диагностика tab-bar -*- lexical-binding: t; -*-

;;;###autoload
(defun pro-tabs-diagnose ()
  "Отобразить ключевые параметры tab-bar/tab-line и объяснить, почему tab-bar может быть невиден."
  (interactive)
  (let ((tab-bar-on tab-bar-mode)
        (show tab-bar-show)
        (auto-width (bound-and-true-p tab-bar-auto-width))
        (tabs (tab-bar-tabs))
        (format (bound-and-true-p tab-bar-format)))
    (message "tabs: %S" tabs)
    (message "tab-bar-mode=%s | tab-bar-show=%s | tab-bar-auto-width=%s | tab-bar-format=%s"
             tab-bar-on show auto-width format)
    (cond
     ((not tab-bar-on)
      (message "[pro-tabs/diagnose] tab-bar-mode выключен — tab-bar не показывается."))
     ;; tab-bar-lines = 0 или nil — Emacs не выводит полосу табов для кадра
     ((let ((lines (frame-parameter nil 'tab-bar-lines)))
        (or (null lines) (zerop lines)))
      (message "[pro-tabs/diagnose] frame parameter tab-bar-lines=%s — tab-bar скрыт (его установил другой пакет или сам Emacs)."
               (frame-parameter nil 'tab-bar-lines)))
     ((and (eq show 1) (= (length tabs) 1))
      (message "[pro-tabs/diagnose] tab-bar-show==1 и лишь одна вкладка — tab-bar скрыт (по умолчанию Emacs)."))
     ((null tabs)
      (message "[pro-tabs/diagnose] tab-bar-tabs пуст — вкладки не инициализированы?"))
     ((eq show nil)
      (message "[pro-tabs/diagnose] tab-bar-show=nil — tab-bar никогда не показывается."))
     ((not (equal format tab-bar-format))
      (message
       "[pro-tabs/diagnose] tab-bar-format переопределён! (другой пакет может маскировать tab-bar)."))
     ((and tab-bar-on (> (length tabs) 1) (eq show t))
      (let* ((win (frame-root-window))
             (height (window-pixel-height win)))
        (if (and (display-graphic-p) (< height 30))
            (message "[pro-tabs/diagnose] tab-bar не видно — фрейм слишком мал?")
          (message "[pro-tabs/diagnose] tab-bar должен быть видим, но если его реально не видно — проверьте не задействованы ли темы/overrides на faces tab-bar/tab-bar-tab."))))
     (t
      (message "[pro-tabs/diagnose] tab-bar должен отображаться (если не мешает внешний пакет)!")))
    ;; ----------------------------------------------------------
    ;; Сводка по всем кадрам — поможет найти «проблемный» frame
    (dolist (fr (frame-list))
      (message "[pro-tabs/diagnose]   frame=%s   tab-bar-lines=%s   tab-bar-mode=%s"
               fr
               (frame-parameter fr 'tab-bar-lines)
               (frame-parameter fr 'tab-bar-mode)))))
