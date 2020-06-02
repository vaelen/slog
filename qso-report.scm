#! /bin/sh
#|
exec csi -s "$0" "$@"
|#


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
(import (srfi 69))

(include "version.scm")
(include "util.scm")
(include "adif.scm")
(include "report.scm")

(import (optimism))

(define (print-version-info)
  (print "SLog QSO Report v1.0.0")
  (print slog-copyright))

(define (print-usage)
  (print-version-info)
  (print "")
  (print "Usage: qso-report [options] <file> [file...]")
  (print "")
  (print "Options:")
  (print "    -t | --type <type>          Set the report type.")
  (print "    -l | --list-types           List the different report types.")
  (print "    -m | --mode <mode>          Only show QSOs for the given mode.")
  (print "    -b | --band <band>          Only show QSOs for the given band.")
  (print "    -c | --callsign <callsign>  Only show QSOs for the given callsign.")
  (print "    -d | --dedup                Remove duplicate callsigns.")
  (print "    -D | --dedup-by-band        Remove duplicate callsigns in each band.")
  (print "    -S | --swl                  Include SWLs (excluded by default).")
  (print "    -h | --help                 Show this help message.")
  (print "    -v | --version              Show version and license information."))

(define (print-type-list)
  (print "Supported Report Types:")
  (print "    standard - This is the default report type.")
  (print "    jarl     - This generates a report for submission to JARL."))

(define (parse-options)
  (parse-command-line
   '(((-t --type) . type)
     ((-l --list-types))
     ((-m --mode) . mode)
     ((-b --band) . band)
     ((-c --callsign) . callsign)
     ((-d --dedup))
     ((-D --dedup-by-band))
     ((-S --swl))
     ((-h --help))
     ((-v --version)))))

(define (main)
  (let* ((options (parse-options))
         (filenames (alist-value options '-- '()))
         (report-type (alist-value options '--type (alist-value options '-t "")))
         (mode (alist-value options '--mode (alist-value options '-m #f)))
         (callsign (alist-value options '--callsign (alist-value options '-c #f)))
         (band (alist-value options '--band (alist-value options '-b #f)))
         (dedup (alist-value options '--dedup (alist-value options '-d #f)))
         (dedup-by-band (alist-value options '--dedup-by-band (alist-value options '-D #f)))
         (swl (alist-value options '--swl (alist-value options '-S #f))))
    (cond ((or (assoc '-v options) (assoc '--version options))
           (print-version-info))
          ((or (assoc '-l options) (assoc '--list-types options))
           (print-type-list))
          ((or (null? filenames) (assoc '-h options) (assoc '--help options))
           (print-usage))
          (else
           (let* ((generate-filter
                     (lambda (field-name value)
                       (let ((type (alist-value adif-data-types field-name 'string)))
                         (cond ((and value (eq? type 'number))
                                (let ((number-value (string->number value)))
                                  (lambda (qso) (eq? (alist-value qso field-name 0)
                                                     number-value))))
                               ((eq? type 'boolean)
                                (let ((boolean-value (if (string? value)
                                                         (or (equal? value "Y")
                                                             (equal? value "y"))
                                                         value)))
                                  (lambda (qso) (if boolean-value
                                                    (alist-value qso field-name #f)
                                                    (not (alist-value qso field-name #f))))))
                               (value
                                (let ((upcase-value (string-upcase value)))
                                  (lambda (qso) (equal?
                                                 (string-upcase (alist-value qso field-name ""))
                                                 upcase-value))))
                               (else 
                                (lambda (qso) #t))))))
                  (generate-dedup-filter
                   (lambda (seen-values fields)
                     (lambda (qso)
                       (let* ((key (string-join (map
                                                 (lambda (field-name)
                                                   (string-upcase (alist-value qso field-name "")))
                                                 fields)
                                                "|"))
                              (seen (hash-table-exists? seen-values key)))
                         (hash-table-set! seen-values key #t)
                         (not seen)))))
                  (mode-filter (generate-filter 'mode mode))
                  (band-filter (generate-filter 'band band))
                  (callsign-filter (generate-filter 'call callsign))
                  (seen-callsigns (make-hash-table))
                  (dedup-filter (cond (dedup
                                       (generate-dedup-filter seen-callsigns '(call)))
                                      (dedup-by-band
                                       (generate-dedup-filter seen-callsigns '(call mode)))
                                      (else (lambda (qso) #t))))
                  (swl-filter (if swl
                                  (lambda (qso) #t)
                                  (generate-filter 'swl #f)))
                  (qso-filter
                   (lambda (qso)
                     (and (swl-filter qso)
                          (mode-filter qso)
                          (band-filter qso)
                          (callsign-filter qso)
                          (dedup-filter qso))))
                  (qsos (filter qso-filter (load-adi filenames))))
             (print-report (string-downcase report-type) qsos)
             (print (string-append "\nTotal QSOs:\t" (number->string (length qsos)))))))))

(main)
