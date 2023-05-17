#lang racket

(require "etapa2.rkt")

(provide (all-defined-out))

; TODO 1
; După modelul funcției stable-match?, implementați funcția
; get-unstable-couples care primește o listă de logodne
; engagements, o listă de preferințe masculine mpref și o 
; listă de preferințe feminine wpref, și întoarce lista
; tuturor cuplurilor instabile din engagements.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; Nu este permisă recursivitatea pe stivă.
; Nu sunt permise alte funcții ajutătoare decât
; better-match-exists? și funcțiile de manipulare a listelor de
; preferințe definite în etapele anterioare.
; Nu este permisă apelarea multiplă a aceleiași funcții pe
; aceleași argumente.
; Folosiți una sau mai multe dintre expresiile let, let*, letrec,
; named let pentru a vă putea conforma acestor restricții.
(define (get-unstable-couples engagements mpref wpref)
  (let iter ((eng engagements) (uscpl '()))
    (if (null? eng)
        uscpl
        (let* ((couple (car eng))
               (man (cdr couple))
               (woman (car couple))
               (man-pref (get-pref-list mpref man))
               (wom-pref (get-pref-list wpref woman))
               (eng-wom (map (λ (pair) (cons (cdr pair) (car pair))) engagements)))
          (if (or (better-match-exists? man woman man-pref wpref engagements) (better-match-exists? woman man wom-pref mpref eng-wom))
              (iter (cdr eng) (cons couple uscpl))
              (iter (cdr eng) uscpl))))))

; TODO 2
; Implementați funcția engage care primește o listă free-men
; de bărbați încă nelogodiți, o listă de logodne parțiale 
; engagements (unde fiecare cuplu are pe prima poziție o femeie),
; o listă de preferințe masculine mpref și o listă de preferințe 
; feminine wpref, și întoarce o listă completă de logodne stabile,
; obținută conform algoritmului Gale-Shapley:
; - cât timp există un bărbat m încă nelogodit
;   - w = prima femeie din preferințele lui m pe care m nu a cerut-o încă
;   - dacă w este nelogodită, adaugă perechea (w, m) la engagements
;   - dacă w este logodită cu m'
;     - dacă w îl preferă pe m lui m'
;       - m' devine liber
;       - actualizează partenerul lui w la m în engagements
;     - altfel, repetă procesul cu următoarea femeie din lista lui m
; Folosiți named let pentru orice proces recursiv ajutător (deci nu
; veți defini funcții ajutătoare recursive).
; Folosiți let și/sau let* pentru a evita calcule duplicate.
(define (engage free-men engagements mpref wpref)
  (let iter-fm ((fm free-men) (eng engagements) (wom-for-man (get-pref-list mpref (car free-men))))
    (if (null? fm)
        eng
        (let* ((m (car fm))
               (w (car wom-for-man))
               (partner (get-partner eng w))
               (wom-pref (get-pref-list wpref w))
               (next-fm (cdr fm))
               (next-pref (if (null? next-fm) '() (get-pref-list mpref (car next-fm)))))
          (cond
            ((null? wom-for-man) (iter-fm (cdr fm) eng next-pref))
            ((equal? partner m) (iter-fm fm eng (cdr wom-for-man)))
            ((not partner) (iter-fm (cdr fm) (append eng (list (cons w m))) next-pref))
            (partner (if (preferable? wom-pref m partner)
                         (iter-fm (cdr (append fm (list partner))) (update-engagements eng w m) (if (empty? next-pref) (get-pref-list mpref partner) next-pref))
                         (iter-fm fm eng (cdr wom-for-man))))
            (else (iter-fm (cdr fm) eng next-pref)))))))
                            
; TODO 3
; Implementați funcția gale-shapley care este un wrapper pentru
; algoritmul implementat de funcția engage. Funcția gale-shapley
; primește doar o listă de preferințe masculine mpref și o listă
; de preferințe feminine wpref și calculează o listă completă de
; logodne stabile conform acestor preferințe.
(define (gale-shapley mpref wpref)
  (engage (get-men mpref) '() mpref wpref))


; TODO 4
; Implementați funcția get-couple-members care primește o listă
; de perechi cu punct și întoarce o listă simplă cu toate elementele 
; care apar în perechi.
; Folosiți funcționale, fără recursivitate explicită.
(define (get-couple-members pair-list)
  (foldr (λ (pair acc) (append (append acc (list (car pair))) (list (cdr pair))))
         '()
         pair-list))

