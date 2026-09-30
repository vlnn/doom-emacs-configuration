;;; test/vc-test.el -*- lexical-binding: t; -*-

(ert-deftest +github-explorer--parent-buffer-name/strips-last-segment ()
  (ert-info ("parent should drop the trailing directory segment")
    (should (equal "owner/repo/src/" (+github-explorer--parent-buffer-name "owner/repo/src/lib/")))))

(ert-deftest +github-explorer--parent-buffer-name/root-stays ()
  (ert-info ("a single-segment name should collapse to the empty root")
    (should (equal "" (+github-explorer--parent-buffer-name "repo/")))))
