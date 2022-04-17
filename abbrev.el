;; -*- coding: utf-8; lexical-binding: t; -*-

(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(

    ;; DnD abbrevs
    ("hp" "hit points" nil 0)
    ("dmg" "damage" nil 0)
    ("inish" "initiative" nil 0)

    ;; work abbrevs
    ("pct" "percent" nil 0)

    ;; org-mode abbrevs
    ("icb" "- [ ]" nil 0)

    ;; doom utilities
    ("pkg" "(package!" nil 0)
    ("ttl" "#+title: " nil 0)

    ;; english word abbrev
    ("abbout" "about" nil 0)

    ;; ukrainian words abbrev
    ("будьласка" "будь ласка" nil 0)
    ))
(set-default 'abbrev-mode t)

(setq save-abbrevs nil)

