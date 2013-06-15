

(omnisharp--display-autocomplete-suggestions
 (list (json-read-from-string
 "{\"CompletionText\":\"ToString()\",\"Description\":\"string
 ToString();\\nMethodSystem.String\\nReturns a System.String
 representation of the value of the current instance. \\nReturns: \\nA
 System.String representation of the current
 instance.\\nRemarks:\\n\\n This method overrides
 System.Object.ToString.01.0.5000.02.0.0.04.0.0.0\",\"DisplayText\":\"string
 ToString()\"}")))

;; For quick testing
(define-key evil-insert-state-map
  (kbd "<f5>")
  (lambda () (interactive)
    (omnisharp-auto-complete)))
