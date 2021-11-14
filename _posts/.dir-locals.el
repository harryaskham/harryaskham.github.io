(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook 'org-jekyll-md-export-to-md nil t)))
