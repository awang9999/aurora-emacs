(defcustom aurora-font-family-monospaced "FiraCode"
  "Name of the font-family to use for aurora.
Defaults to Fira Code. Customizing this might lead to conflicts
if the family does not have sufficient bold/light etc faces."
  :group 'aurora
  :type 'string)

(defcustom aurora-font-family-proportional nil
  "Font to use for variable pitch faces.
Setting this allows aurora to display variable pitch faces when,
for instance, 'variable-pitch-mode' or 'mixed-pitch-mode' is active in a buffer.
Defaults to nil."
  :group 'aurora
  :type 'string)

(defcustom aurora-font-size 200
  "Default value for the font size of aurora-theme in pt units.
Note: to change this after startup, call
\(aurora-faces\) and \(aurora-themes\)."
  :group 'aurora
  :type 'integer)

;; Enable ligatures in programming modes

(defvar fira-code--all-ligatures
  '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "[]" "::"
    ":::" ":=" "!!" "!=" "!==" "-}" "--" "---" "-->" "->" "->>" "-<"
    "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_(" ".-"
    ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**" "/=" "/==" "/>"
    "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>" "++" "+++" "+>"
    "=:=" "==" "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">=" ">=>"
    ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>" "<$" "<$>" "<!--"
    "<-" "<--" "<->" "<+" "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-"
    "<<=" "<<<" "<~" "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>"
    "%%"))

;; Use ligature.el
(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode fira-code--all-ligatures)
  (global-ligature-mode 't))

;; Moderate font lock
(setq font-lock-maximum-decoration nil)

;; No limit on font lock
(setq font-lock-maximum-size nil)

;; Set default font
(set-face-attribute 'default nil
                    :family 'aurora-font-family-monospaced
                    :height aurora-font-size)

(provide 'aurora-fonts)
