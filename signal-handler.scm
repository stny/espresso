(define-module signal-handler
  (export signal-handler))
(select-module signal-handler)

(set-signal-handler! SIGINT
   (lambda _
     (newline)
     (display "bye bye ( ˘ω˘)")
     (exit)
     )
   )
