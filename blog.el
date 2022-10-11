;;; blog.el -*- lexical-binding: t; -*-
;; this is pretty much a copy of example configuration from https://github.com/bastibe/org-static-blog/
;; you have to add favicons and style.css though to get pretty site as the result. I have mine at http://vlnn.dev

(setq org-static-blog-publish-title "vlnn.dev")
(setq org-static-blog-publish-url "https://vlnn.dev/")
(setq org-static-blog-publish-directory "~/src/vlnn.github.io/")
(setq org-static-blog-posts-directory "~/src/vlnn.github.io/posts/")
(setq org-static-blog-drafts-directory "~/src/vlnn.github.io/drafts/")
(setq org-static-blog-enable-tags t)
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)

(setq org-static-blog-page-header
      "<meta name=\"author\" content=\"Volodymyr Anokhin\">
<meta name=\"referrer\" content=\"no-referrer\">
<link href= \"static/style.css\" rel=\"stylesheet\" type=\"text/css\" />
<link rel=\"icon\" href=\"static/favicon.ico\">
<link rel=\"apple-touch-icon-precomposed\" href=\"static/favicon-152.png\">
<link rel=\"msapplication-TitleImage\" href=\"static/favicon-144.png\">
<link rel=\"msapplication-TitleColor\" href=\"#0141ff\">
<script src=\"static/katex.min.js\"></script>
<script src=\"static/auto-render.min.js\"></script>
<script src=\"static/lightbox.js\"></script>
<link rel=\"stylesheet\" href=\"static/katex.min.css\">
<script>document.addEventListener(\"DOMContentLoaded\", function() { renderMathInElement(document.body); });</script>
<meta http-equiv=\"content-type\" content=\"application/xhtml+xml; charset=UTF-8\">
<meta name=\"viewport\" content=\"initial-scale=1,width=device-width,minimum-scale=1\">")

(setq org-static-blog-page-preamble
      "<div class=\"h1\">
  <a href=\"https://vlnn.dev\">Generalistic stuff VLNN writes</a>
  <div class=\"sitelinks\">
    <a href=\"https://github.com/vlnn\">Github</a> | <a href=\"https://vlnn.dev/projects.html\">Projects</a> | <a href=\"https://www.linkedin.com/in/volodymyr-anokhin/\">Linkedin</a>
  </div>
</div>")

(setq org-static-blog-page-postamble
      "<div id=\"archive\">
  <a href=\"https://vlnn.dev/archive.html\">Other posts may be not available</a>
</div> ")

(setq org-static-blog-post-comments nil)
