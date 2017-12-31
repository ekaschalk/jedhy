;; (require [src.utils.macros [*]])
;; (require [hy.extra.anaphoric [*]])
;; (require [tests.hytest [*]])
;; (import [tests.hytest [*]])

;; (import
;;   [src.completion [Completer]]
;;   [src.models [Prefix]])


;; ;; TODO Right now Candidate, Completer, and Prefix all need namespace/local
;; ;; Either turn into a global datastructure or refactor

;; (defn test-stuff []
;;   (setv complete
;;         (Completer))

;;   ;; print isn't in there
;;   ;; modules aren't in there
;;   ;; imported macros (fn->) *are* in there
;;   ;; (print complete.candidates)

;;   ;; (print (complete (Prefix "print._")))
;;   )


;; ;; * To-port

;; ;; (defn test-candidates-globals []
;; ;;   (assert-all-in ["first" "in" "+" "even?"]
;; ;;                  (--HYCOMPANY-get-globals)))

;; ;; (defn test-candidates-trimmed []
;; ;;   (setv candidates
;; ;;         (--HYCOMPANY-get-globals))

;; ;;   (assert-all-in ["first" "even?"] candidates)

;; ;;   (setv trimmed-candidates
;; ;;         (--HYCOMPANY-trim-candidates candidates "fi"))

;; ;;   (assert-in "first" trimmed-candidates)
;; ;;   (assert-not-in "even?" trimmed-candidates))

;; ;; (defn test-candidates-formatted []
;; ;;   (assert-in "builtins"
;; ;;              (--HYCOMPANY "built"))
;; ;;   (assert-in "builtins.eval"
;; ;;              (--HYCOMPANY "builtins.e"))
;; ;;   (assert-in "builtins.eval.--call--"
;; ;;              (--HYCOMPANY "builtins.eval.")))
