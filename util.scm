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
(import (chicken base))
(import (chicken sort))

(import (srfi 1))
(import (srfi 13))


;; Get one the value of a key in an alist
(define (alist-value alist name default-value)
  (let ((value (assoc name alist)))
    (if value (cdr value) default-value)))

;; Change the zone-offset of a date to 0 without changing the time.
(define (date->utc date)
  (make-date (date-nanosecond date)
             (date-second date)
             (date-minute date)
             (date-hour date)
             (date-day date)
             (date-month date)
             (date-year date)
             0))

;; Combine a date and time field into a single date object
(define (combine-date-time date time)
  (cond ((and date time)
         (make-date (date-nanosecond time)
                    (date-second time)
                    (date-minute time)
                    (date-hour time)
                    (date-day date)
                    (date-month date)
                    (date-year date)
                    (date-zone-offset time)))
        (date date)
        (time time)
        (else #f)))
