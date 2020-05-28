;; Copyright (c) 2020, Andrew C. Young
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.
;;
;; 3. Neither the name of the copyright holder nor the names of its
;;    contributors may be used to endorse or promote products derived from
;;    this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(import scheme)
(import (srfi 1))
(import (srfi 13))

(define jarl-prefix-list
  '(
    "JA1" "JD1" "JE1" "JF1" "JG1" "JH1" "JI1" "JJ1" "JK1" "JL1" "JM1" "JN1" "JO1" "JP1" "JQ1" "JR1" "JS1"
        "JA2" "JE2" "JF2" "JG2" "JH2" "JI2" "JJ2" "JK2" "JL2" "JM2" "JN2" "JO2" "JP2" "JQ2" "JR2" "JS2"
        "JA3" "JE3" "JF3" "JG3" "JH3" "JI3" "JJ3" "JK3" "JL3" "JM3" "JN3" "JO3" "JP3" "JQ3" "JR3" "JS3"
        "JA4" "JE4" "JF4" "JG4" "JH4" "JI4" "JJ4" "JK4" "JL4" "JM4" "JN4" "JO4" "JR4"
        "JA5" "JE5" "JF5" "JG5" "JH5" "JI5" "JJ5" "JR5"
        "JA6" "JE6" "JF6" "JG6" "JH6" "JI6" "JJ6" "JK6" "JL6" "JM6" "JN6" "JO6" "JP6" "JQ6" "JR6" "JS6"
        "JA7" "JE7" "JF7" "JG7" "JH7" "JI7" "JJ7" "JK7" "JL7" "JM7" "JN7" "JO7" "JP7" "JR7"
        "JA8" "JE8" "JF8" "JG8" "JH8" "JI8" "JJ8" "JK8" "JL8" "JM8" "JR8"
        "JA9" "JE9" "JF9" "JH9" "JR9"
        "JA0" "JE0" "JF0" "JG0" "JH0" "JI0" "JJ0" "JR0"
        "7J1" "7J2" "7J3" "7J4" "7J5" "7J6" "7J7" "7J8" "7J9" "7J0"
        "7K1" "7K2" "7K3" "7K4" "7L1" "7L2" "7L3" "7L4" "7M1" "7M2" "7M3" "7M4" "7N1" "7N2" "7N3" "7N4"
        "8J1" "8J2" "8J3" "8J4" "8J5" "8J6" "8J7" "8J8" "8J9" "8J0" "8N1" "8N2" "8N3" "8N4" "8N5" "8N6" "8N7" "8N8" "8N9" "8N0"
        ))

(define (get-jarl-sort-key callsign)
  (define (find-sort-key callsign sort-key prefix-list)
    (cond ((or (null? prefix-list)
               (string-prefix? (car prefix-list) callsign))
           sort-key)
          (else (find-sort-key callsign (add1 sort-key) (cdr prefix-list)))))
  (find-sort-key (string-upcase callsign) 0 jarl-prefix-list))

(define (jarl-callsign< a b)
  (let ((sort-key-a (get-jarl-sort-key a))
        (sort-key-b (get-jarl-sort-key b)))
    (if (= sort-key-a sort-key-b)
        (string< a b)
        (< sort-key-a sort-key-b))))
