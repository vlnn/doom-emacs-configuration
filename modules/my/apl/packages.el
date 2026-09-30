;; -*- no-byte-compile: t; -*-
(package! gnu-apl-mode)
(package! ride-apl
  :recipe (:host github :repo "vlnn/ride-apl"
           :files ("*.el" (:exclude "*-test.el"))))
