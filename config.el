;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face

(setq doom-font (font-spec :family "DejaVu Sans Mono" :size 18 :weight 'medium))

;; Transparency
(set-frame-parameter nil 'alpha-background 100)
(add-to-list 'default-frame-alist '(alpha-background . 100))
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-tokyo-night)
(setq doom-theme 'doom-plain)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(setq display-line-numbers-type 'relative)

(setq org-return-follows-link  t)

(with-eval-after-load 'browse-url
  (setq browse-url-browser-function 'browse-url-firefox
        browse-url-firefox-program "/run/current-system/sw/bin/firefox"))

(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-roam-directory "~/org-roam/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.



(use-package! org
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-clock-sound "~/.config/audio/alarm.wav"
	org-startup-with-inline-images t
	org-hide-leading-stars t
	org-directory "~/org"
	org-agenda-files '("Tasks.org" "Habits.org" "~/org-roam/daily")
	org-agenda-start-with-log-mode t
	org-log-done 'time
	org-log-into-drawer t
	org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	  (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)"))
        org-format-latex-options (plist-put org-format-latex-options :scale 2.0)
        ))

(use-package! org-modern
  :after org
  :config
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")
  (global-org-modern-mode)
  )

(with-eval-after-load 'org (global-org-modern-mode))

(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-super-agenda-groups
        '((:log t)  ; Automatically named "Log"
          (:name "Schedule"
           :time-grid t)
          (:name "Today"
           :scheduled today)
          (:habit t)
          (:name "Due today"
           :deadline today)
          (:name "Overdue"
           :deadline past)
          (:name "Due soon"
           :deadline future)
          (:name "Unimportant"
           :todo ("SOMEDAY" "MAYBE" "CHECK" "TO-READ" "TO-WATCH")
           :order 100)
          (:name "Waiting..."
           :todo "WAITING"
           :order 98)
          (:name "Scheduled earlier"
           :scheduled past)))
  :config
  (org-super-agenda-mode)
  )

(use-package! olivetti
  :hook (text-mode . olivetti-mode)
  :hook (prog-mode . olivetti-mode)
  :hook (olivetti-mode . (lambda() (olivetti-set-width 150)))
  )

(use-package! nix-mode
  :mode "\\.nix\\'")

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t)
  (setq org-roam-ui-latex-macros
        '(("\\kk" . "\\textbf{kk}")
          ("\\vv" . "\\textbf{vv}")
          ("\\xx" . "\\textbf{xx}")
          )))

(use-package! org-roam-timestamps
  :after org-roam
  :config (org-roam-timestamps-mode))

(define-derived-mode luau-mode
  lua-mode "Luau" "Major mode for luau.")
(add-to-list 'auto-mode-alist '("\\.luau\\'" . luau-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((luau-mode) . ("luau-lsp" "lsp" "--definitions=/home/hmp/.config/doom/globalTypes.d.lua")))
  (add-to-list 'eglot-server-programs
               '((c++-mode) . ("clangd" "--enable-config")))
  (add-to-list 'eglot-server-programs
               '((cmake-mode) . ("cmake-language-server")))
  (add-to-list 'eglot-server-programs
               '((glsl-mode) . ("glsl_analyzer")))
  )

(use-package! pdf-tools
  :config
  (setq pdf-annot-activate-created-annotations t)
  )

(use-package! org-remark-global-tracking
  ;; It is recommended that `org-remark-global-tracking-mode' be
  ;; enabled when Emacs initializes. You can set it in
  ;; `after-init-hook'.
  :hook after-init
  :config
  ;; Selectively keep or comment out the following if you want to use
  ;; extensions for Info-mode, EWW, and NOV.el (EPUB) respectively.
  (use-package! org-remark-info :after info :config (org-remark-info-mode +1))
  (use-package! org-remark-eww  :after eww  :config (org-remark-eww-mode +1))
  (use-package! org-remark-nov  :after nov  :config (org-remark-nov-mode +1)))

(use-package! org-remark
  :bind (;; :bind keyword also implicitly defers org-remark itself.
         ;; Keybindings before :map is set for global-map. Adjust the keybinds
         ;; as you see fit.
         ("C-c n m" . org-remark-mark)
         ("C-c n l" . org-remark-mark-line)
         :map org-remark-mode-map
         ("C-c n o" . org-remark-open)
         ("C-c n ]" . org-remark-view-next)
         ("C-c n [" . org-remark-view-prev)
         ("C-c n r" . org-remark-remove)
         ("C-c n d" . org-remark-delete)))

(add-hook 'flycheck-before-syntax-check-hook 'direnv-update-environment)

(setq TeX-engine 'default)
(setq TeX-command-default "LaTeX")
(setq TeX-process-asynchronous t)
(setq TeX-command-extra-options "-shell-escape")
(setq TeX-parse-self t)
(setq TeX-auto-save t)
(setq TeX-source-correlate-mode t)

;; ===========================
;; Org LaTeX MLA Configuration
;; ===========================
(use-package! ox-latex
  :after org
  :config
  ;; Define custom LaTeX class "mla"
  (add-to-list 'org-latex-classes
               '("mla"
                 "\\documentclass[12pt]{article}
\\usepackage[letterpaper]{geometry}
\\usepackage[margin=1in]{geometry}
\\usepackage{setspace}
\\doublespacing
\\usepackage[style=mla]{biblatex}
\\usepackage{times}
\\usepackage{sectsty}
\\setcounter{secnumdepth}{0}
\\allsectionsfont{\\normalsize}
\\sectionfont{\\normalsize\\bfseries\\raggedright}        % Level 1 → bold
\\subsectionfont{\\normalsize\\itshape\\raggedright}     % Level 2 → italics
\\subsubsectionfont{\\normalsize\\bfseries\\raggedright} % Level 3 → bold
\\paragraphfont{\\normalsize\\itshape\\raggedright}      % Level 4 → italics
\\subparagraphfont{\\normalsize\\underline\\raggedright} % Level 5 → underline
\\usepackage{indentfirst}
\\usepackage{fancyhdr}
\\pagestyle{fancy}
\\fancyhead{}
\\lhead{}
\\chead{}
\\rhead{\\AUTHORLAST~\\thepage}  % right header: last name + page
\\lfoot{} \\cfoot{} \\rfoot{} % all footers empty
\\renewcommand{\\headrulewidth}{0pt} 
\\renewcommand{\\footrulewidth}{0pt} 
\\setlength\\headsep{0.333in}
\\defbibheading{bibliography}[
  Works Cited
]{%
  \\begin{center}
    \\normalfont\\normalsize Works Cited
  \\end{center}%
}
\\AtBeginDocument{
  \\noindent
  \\AUTHOR \\\\
  \\PROFESSOR \\\\
  \\CLASSNAME \\\\
  \\DATE
}
\\renewcommand{\\maketitle}{
  \\\\
  \\begin{center}
  \\TITLE \\\\
  \\end{center}
  \\\\
}"
                 ;; Map Org levels to LaTeX sections
                 ("\\section{%s}" . "\\section*{%s}")       ;; Level 1: bold flush left
                 ("\\subsection{%s}" . "\\textit{%s}")     ;; Level 2: italics flush left
                 ("\\subsubsection{%s}" . "\\underline{%s}"))) ;; Level 3: underline flush left

  ;; Default to using biber for biblatex
  (setq org-latex-pdf-process
        '("pdflatex -interaction nonstopmode -output-directory %o %f"
          "biber %b"
          "pdflatex -interaction nonstopmode -output-directory %o %f"
          "pdflatex -interaction nonstopmode -output-directory %o %f")))

;; ===========================
;; Org-cite & BibLaTeX MLA setup
;; ===========================
(after! org
  (setq org-cite-export-processors '((latex biblatex)))
  (setq org-latex-prefer-user-labels t)
  (add-to-list 'org-file-apps '("\\.html$" . system))
  (add-to-list 'org-file-apps '("https?" . default))
  )

(use-package! ob-mermaid
  :config
  (setq ob-mermaid-cli-path "mmdc"))

(after! org
  ;; Enable mermaid in org-babel
  (add-to-list 'org-babel-load-languages '(mermaid . t))

  ;; Reload babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   org-babel-load-languages))

(setq lsp-eldoc-render-all t)

(setq-default tab-width 4)
(setq-default evil-shift-width 4)
