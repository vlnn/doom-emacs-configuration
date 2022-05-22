# doom-emacs-configuration
Custom configuration for Doom Emacs

## Installation

(Given you have Doom Emacs installed as described here https://github.com/doomemacs/doomemacs#install)

```sh
mv ~/.doom.d ~./.doom.d.backup # backup if needed
git clone https://github.com/vlnn/doom-emacs-configuration/ ~/.doom.d
~/.emacs.d/bin/doom sync
```

## What's inside

This is primitive but not-so-clean configuration of doom-emacs. Of course you should grow your own. No problem, just steal interesting stuff from here as I did before.

Some features you may expect to be working:

1. Commented `config.el` where every implemented idea has at least one comment.
1. Use blasphemous `jkl;` instead everyone's loved `hjkl` in evil-mode. There is a good reason for that, and you should buy one for yourself -- it's called Kinesis Advantage. See https://github.com/vlnn/kinesis-advantage-2 for more.
1. Simple org- and zettel- configurations (`~/org` and `~/Zettels` respectively). If you do not use org yet you should. If you use zettels already you perhaps should continue your way.
1. Different size and styles mapped to different levels of org outlines.
1. Automatic saving of the buffer when you leave the focus out of emacs. It sounds scary, but it's *very* useful (and twice so in tiling windows manager).
1. Statistics of your keypresses shown with `M-x keyfreq-show`. Now I know that I'm using `evil-forward-char` and `right-char` in around `1/5` of all keypresses, which is quite crazy!
1. Built-in support of Ukrainian language (press <C-\\> to toggle between English and Ukrainian inputs).

## If something doesn't work

Please open an issue (preferably followed by PR). I'll investigate and at least communicate my thoughts back to you (or even implement something useful!)
