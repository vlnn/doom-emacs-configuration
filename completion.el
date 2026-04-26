;;; completion.el -*- lexical-binding: t; -*-

(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq tab-always-indent nil)

(map! :map corfu-map
      "TAB"     #'corfu-next
      [tab]     #'corfu-next
      [backtab] #'corfu-previous)

(after! corfu
  (setq corfu-auto-delay  0
        corfu-auto-prefix 1))

(after! abbrev
  (setq abbrev-file-name (concat doom-user-dir "abbrev_defs")
        save-abbrevs 'silently))
(setq-default abbrev-mode 1)

(setq completion-ignored-extensions
      '(".a" ".aux" ".bbl" ".bin" ".blg" ".class" ".cp" ".cps" ".elc"
        ".fmt" ".fn" ".fns" ".git/" ".glo" ".glob" ".gmo" ".hg/" ".idx"
        ".ky" ".kys" ".la" ".lib" ".ln" ".lo" ".lof" ".lot" ".mem" ".mo"
        ".o" ".pg" ".pgs" ".pyc" ".pyo" ".so" ".tfm" ".toc" ".tp" ".tps"
        ".v.d" ".vio" ".vo" ".vok" ".vos" ".vr" ".vrs"
        "~"))
