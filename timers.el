;;; timers.el -*- lexical-binding: t; -*-
(fset 'notifications-notify-method 'alert)

(setq alert-default-style 'osx-notifier)

(hammy-define "⅓-time"
  :documentation "Breaks that are ⅓ as long as the last work interval."
  :intervals
  (list
   (interval :name "Work"
             ;; It's intended that the user manually end this interval
             ;; when ready, but we specify a maximum of 90 minutes by
             ;; default.
             :duration "90 minutes"
             :before (do (announce "Starting work time (advance to break when ready).")
                         (notify "Starting work time (advance to break when ready)."))
             :advance (remind "10 minutes"
                              (do (let* ((current-duration (ts-human-format-duration
                                                            (float-time
                                                             (time-subtract (current-time) current-interval-start-time))))
                                         (message (format "You've worked for %s!" current-duration)))
                                    (announce message)
                                    (notify message)
                                    (when hammy-sound-end-work
                                      (play-sound-file hammy-sound-end-work))))))
   (interval :name "Break"
             :duration (do (pcase-let* ((`(,_interval ,start ,end) (car history))
                                        (work-seconds (float-time (time-subtract end start))))
                             (* work-seconds 0.33)))
             :before (do (let ((message (format "Starting break for %s."
                                                (ts-human-format-duration current-duration))))
                           (announce message)
                           (notify message)))
             :advance (remind "5 minutes"
                              (do (announce "Break time is over!")
                                  (notify "Break time is over!")
                                (when hammy-sound-end-break
                                  (play-sound-file hammy-sound-end-break)))))))
