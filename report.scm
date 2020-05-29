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

(include "util.scm")
(include "adif.scm")
(include "jarl.scm")

(define (report-fields report-type)
  (cond ((equal? report-type "jarl")
          '((x . "No.")
            (call . "Callsign")
            (qso_date . "Date")
            (band . "Band")
            (mode . "Mode")
            (gridsquare . "Grid")))
        (else
         '((qso_date . "Date")
           (time_on . "Time")
           (call . "Callsign")
           (name . "Name")
           (band . "Band")
           (mode . "Mode")
           (gridsquare . "Grid")
           (cnty . "JCC/JGC/County")
           (qth . "QTH")
           (rst_sent . "Sent")
           (rst_rcvd . "Rcvd")
           (qsl_sent . "QSL Sent")
           (qsl_sent_via . "QSL Sent Via")
           (qsl_rcvd . "QSL Rcvd")
           (qsl_rcvd_via . "QSL Rcvd Via")
           (lotw_qsl_sent . "LotW Sent")
           (lotw_qsl_rcvd . "LotW Rcvd")
           (eqsl_qsl_sent . "eSQL Sent")
           (eqsl_qsl_rcvd . "eSQL Rcvd")
           (qslmsg . "QSL Message")))))


;; Returns the correct sort method for a given report type
(define (report-sort-method report-type)
  (cond ((equal? report-type "jarl") (qso-callsign-less jarl-callsign<))
        (else qso-date-time<)))

;; Print a report
(define (print-report report-type qsos)

  ;; Get a list of values for a given list of QSO field names
  (define (get-fields qso field-names)
    (define (get-field qso field-names)
      (list (string-upcase (alist-value qso (car field-names) ""))))
    
    (if (null? (cdr field-names))
        (get-field qso field-names)
        (append (get-field qso field-names)
                (get-fields qso (cdr field-names)))))

  ;; Convert QSO list to record list
  (define (get-records qsos field-names)
    (let ((record (get-fields (car qsos) field-names)))
      (if (null? (cdr qsos))
          (list record)
          (append (list record) (get-records (cdr qsos) field-names)))))

  ;; Print a record list
  (define (print-records records)
    (unless (null? records)
      (print (string-join (car records) "\t"))
      (print-records (cdr records))))

  (define (print-header header)
    (print (string-join header "\t")))

  
  (let* ((header (map cdr (report-fields report-type)))
         (field-names (map car (report-fields report-type)))
         (records (get-records (sort qsos (report-sort-method report-type)) field-names)))

    ;; Print Header
    (print-header header)

    ;; Print Records
    (print-records records)))
