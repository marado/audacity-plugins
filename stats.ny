;nyquist plug-in
;version 1
;type analyze
;name "Wave Stats..."
;action "Gathering Data..."
;info "By Steve Daulton. http://audacity.easyspacepro.com Released under GPL v2.\n\nAttempting to analyse too much data may cause Nyquist to crash.\nIn the unlikey event of Audacity crashing, reduce the\n'Maximum Length' to less than 30 seconds."

;; version 0.3

;; A-weighting curve by Edgar
;; Much of inspiration for this plug-in from endolith.

;control time "Maximum Length to Analyse" real "(seconds)" 10 0 30

(setq time (min 30 (max 0 time))) ; 0 < time < 30

(setq bignum (truncate (* time *sound-srate*)))
(setq step (truncate (min bignum LEN))) ; 'peak' requires blocksize and stepsize as integers
(setq *float-format* "%#1.3f")
(setq msg (format NIL "Length of selection: ~a seconds.~%~a samples at ~a Hz.~%"(get-duration 1)(truncate LEN)(truncate *sound-srate*)))
(setq msg (strcat msg (format NIL "Analysis of first ~a seconds:~%(~a samples)~%" (min time (get-duration 1)) step)))
(setq channels (if (arrayp s) 1 0))
(setq *float-format* "%#1.1f")

(defun a-weight (s-in)
   (lp (lp (hp (hp (hp (hp s-in 20.6) 20.6) 107.7) 737.9) 12200) 12200))

(defun s-rms (s-in)
   (linear-to-dB (sqrt (peak (snd-avg (mult s-in s-in) step step OP-AVERAGE) bignum))))

(defun test (s-in)
   (if (> channels 0)
      (setq msg (strcat msg (format NIL "~%CHANNEL ~a~%" channels)))
      (setq msg (strcat msg (format NIL "~%Mono Track.~%"))))
      
   (setq msg (strcat msg (format NIL "Peak Level: ~a dBFS~%" (linear-to-db (peak s-in bignum)))))
   (setq msg (strcat msg (format NIL "Peak Positive: ~a dBFS~%" (linear-to-db (peak (sum 1 (clip (sum s-in -1) 1)) bignum)))))
   (setq msg (strcat msg (format NIL "Peak Negative: ~a dBFS~%" (linear-to-db (peak (sum -1 (clip (sum s-in 1) 1)) bignum)))))
   (setq msg (strcat msg (format NIL "DC offset: ~a %~%" (* 100.0 (peak (snd-avg s-in step step op-average) bignum)))))
   (setq msg (strcat msg (format NIL "RMS: ~a dBFS~%" (s-rms s-in))))
   (setq msg (strcat msg (format NIL "RMS (A-weighted): ~a dBFS~%" (+ 2.5664444 (s-rms (a-weight s-in))))))
(setq channels (1+ channels)))

(multichan-expand #'test s)
(format NIL msg)