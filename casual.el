;;; casual.el -*- lexical-binding: t; -*-

(after! org-agenda
  (require 'casual-agenda)
  (map! :map org-agenda-mode-map
        :n "C-j" #'casual-agenda-tmenu
        :n "go"  #'casual-agenda-tmenu))

(after! calc
  (require 'casual-calc)
  (map! :map calc-mode-map
        "C-j" #'casual-calc-tmenu)
  (map! :map calc-alg-map
        "C-j" #'casual-calc-tmenu))

(after! dired
  (require 'casual-dired)
  (map! :map dired-mode-map
        :n "C-j" #'casual-dired-tmenu))

(after! info
  (require 'casual-info)
  (map! :map Info-mode-map
        :n "C-j" #'casual-info-tmenu))

(after! ibuffer
  (require 'casual-ibuffer)
  (map! :map ibuffer-mode-map
        :n "C-j" #'casual-ibuffer-tmenu
        :n "F"   #'casual-ibuffer-filter-tmenu
        :n "s"   #'casual-ibuffer-sortby-tmenu))

(after! bookmark
  (require 'casual-bookmarks)
  (map! :map bookmark-bmenu-mode-map
        :n "C-j" #'casual-bookmarks-tmenu))

(after! isearch
  (require 'casual-isearch)
  (map! :map isearch-mode-map
        "C-o" #'casual-isearch-tmenu))

(after! re-builder
  (require 'casual-re-builder)
  (map! :map reb-mode-map
        "C-j" #'casual-re-builder-tmenu)
  (map! :map reb-lisp-mode-map
        "C-j" #'casual-re-builder-tmenu))

(after! avy
  (require 'casual-avy)
  (map! :leader
        :desc "Casual Avy" "j a" #'casual-avy-tmenu))
