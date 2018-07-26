;;; ~/.doom.d/theming/doom-dream-gradient.el -*- lexical-binding: t; -*-

;;; doom-dream-gradient.el --- For use with Dream Wallpapers
(require 'doom-themes)

;;
(defgroup doom-dream-gradient-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-dream-gradient-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-dream-gradient-theme
  :type 'boolean)

(defcustom doom-dream-gradient-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-dream-gradient-theme
  :type 'boolean)

(defcustom doom-dream-gradient-comment-bg doom-dream-gradient-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-dream-gradient-theme
  :type 'boolean)

(defcustom doom-dream-gradient-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-dream-gradient-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-dream-gradient
  "A dark theme for Dream Gradient Wallpapers"

  ;; name        default   256       16
  ((bg         '("#141420" nil       nil            ))
   (bg-alt     '("#13131e" nil       nil            ))
   (base0      '("#1a1b1e" "black"   "black"        ))
   (base1      '("#1e1f22" "#1e1e1e" "brightblack"  ))
   (base2      '("#25272a" "#2e2e2e" "brightblack"  ))
   (base3      '("#282a2e" "#262626" "brightblack"  ))
   (base4      '("#3b3d41" "#3f3f3f" "brightblack"  ))
   (base5      '("#54545c" "#525252" "brightblack"  ))
   (base6      '("#828288" "#6b6b6b" "brightblack"  ))
   (base7      '("#929297" "#979797" "brightblack"  ))
   (base8      '("#dfdfe1" "#dfdfdf" "white"        ))
   (fg         '("#97bfc2" "#bfbfbf" "brightwhite"  ))
   (fg-alt     '("#6e8b8e" "#2d2d2d" "white"        ))

   (grey       base4)
   (red        '("#995c84" "#ff6655" "red"          ))
   (orange     '("#b56e96" "#dd8844" "brightred"    ))
   (green      '("#5c9985" "#99bb66" "green"        ))
   (teal       '("#6eb597" "#44b9b1" "brightgreen"  ))
   (yellow     '("#b57f72" "#ECBE7B" "yellow"       ))
   (blue       '("#3f8ab5" "#51afef" "brightblue"   ))
   (dark-blue  '("#327098" "#2257A0" "blue"         ))
   (magenta    '("#5c6699" "#c678dd" "magenta"      ))
   (violet     '("#6464b5" "#a9a1e1" "brightmagenta"))
   (cyan       '("#35b5b5" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#2d9999" "#5699AF" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        magenta)
   (comments       (if doom-dream-gradient-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if doom-dream-gradient-brighter-comments dark-cyan base5) 0.25))
   (constants      violet)
   (functions      magenta)
   (keywords       (doom-lighten blue 0.05))
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        dark-cyan)
   (variables      (doom-lighten magenta 0.2))
   (numbers        orange)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base0) 0.35)))
   (error          (doom-lighten orange 0.2))
   (warning        yellow)
   (success        (doom-lighten green 0.2))
   (vc-modified    orange)
   (vc-added       (doom-lighten green 0.2))
   (vc-deleted     orange)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-dream-gradient-brighter-modeline)
   (-modeline-pad
    (when doom-dream-gradient-padded-modeline
      (if (integerp doom-dream-gradient-padded-modeline) doom-dream-gradient-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        (doom-darken blue 0.475)
      `(,(doom-darken (car bg-alt) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken blue 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   (doom-darken bg-alt 0.1))
   (modeline-bg-inactive-l `(,(car bg-alt) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-dream-gradient-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; ivy-mode
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-code-face :background (doom-lighten base3 0.05))

   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden))


  ;; --- extra variables ---------------------
  ;; ()
  )

;;; doom-dream-gradient-theme.el ends here
