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

(include "util.scm")

;; Needed for with-input-from-string
(import (chicken port))

;; Parse an ADI file and return a list of QSO records
(define (read-adi)
  ;; Reads a single line comment from the default port.
  ;; Throws the characters that were read.
  ;; Returns the last character read.
  (define skip-adi-comment
    (case-lambda
      (() (skip-adi-comment (read-char)))
      ((next-char)
       (if (or (eof-object? next-char)
               (eqv? next-char #\newline))
           next-char
           (skip-adi-comment)))))

  ;; Read a field name from the default port and return it.
  (define read-adi-field-name
    (case-lambda
      (() (read-adi-field-name #f #\null (peek-char)))
      ((field-name last-char) (read-adi-field-name field-name last-char (peek-char)))
      ((field-name last-char next-char)
       (cond ((eof-object? next-char) #f)
             ((and (or (eqv? last-char #\newline)
                       (eqv? last-char #\null))
                   (eqv? next-char #\#))
              (read-adi-field-name field-name (skip-adi-comment) (peek-char)))
             ((and field-name (or (eqv? next-char #\:)
                                  (eqv? next-char #\>)))
              ;; End of field name
              (when (eqv? next-char #\:) (read-char))
              (string->symbol (string-downcase (list->string (reverse field-name)))))
             (field-name
              ;; Add to the field name
              (read-adi-field-name (cons next-char field-name)
                                   (read-char)))
             ((and (not field-name) (eqv? next-char #\<))
              ;; This is the start of the field
              (read-adi-field-name '() (read-char)))
             (else
              ;; Keep looking for the start of the field
              (read-adi-field-name field-name (read-char)))))))

  ;; Read a field length from the default port and return it.
  (define read-adi-field-length
    (case-lambda
      (() (read-adi-field-length '() (peek-char)))
      ((field-length next-char)
       (if (or (eof-object? next-char)
               (eqv? next-char #\:)
               (eqv? next-char #\>))
           ;; End of field length
           (or (string->number (list->string (reverse field-length))) 0)
           ;; Add to field length
           (read-adi-field-length (cons (read-char) field-length) (peek-char))))))

  ;; Read a field type from the default port and return it.
  (define read-adi-field-type
    (case-lambda
      (() (read-adi-field-type '() (read-char)))
      ((field-type next-char)
       (if (or (eof-object? next-char)
               (eqv? next-char #\>))
           ;; End of field type
           (list->string (reverse field-type))
           ;; Add to field type
           (read-adi-field-type (cons next-char field-type) (read-char))))))

  ;; Read a field value and return it.
  (define read-adi-field-value
    (case-lambda
      ((field-length) (read-adi-field-value field-length '() (peek-char)))
      ((field-length field-value next-char)
       (if (and (not (eof-object? next-char))
                (positive? field-length))
           (read-adi-field-value (sub1 field-length)
                                 (cons (read-char) field-value)
                                 (peek-char))
           (list->string (reverse field-value))))))

  ;; Read a single ADI field from the current input port and return a key/value pair.
  (define (read-adi-field)
    (let* ((field-name (read-adi-field-name))
           (field-length (read-adi-field-length))
           (field-type (read-adi-field-type))
           (field-value (read-adi-field-value field-length)))
      ;; TODO: Handle field types
      (if field-name
          (cons field-name field-value)
          #f)))

  ;; Read an ADI section and return a list of fields.
  (define read-adi-section
    (case-lambda
      (() (read-adi-section `() `() (read-adi-field)))
      ((section record field)
       (cond ((or (not field)
                  (eqv? (car field) 'eoh))
              (if (null? record)
                  (reverse section)
                  (reverse (cons (reverse record) section))))
             ((eqv? (car field) 'eor)
              (read-adi-section (cons (reverse record) section) '() (read-adi-field)))
             (else
              (read-adi-section section (cons field record) (read-adi-field)))))))

  ;; Read header
  (read-adi-section)

  ;; Read and return QSOs
  (read-adi-section))

;; Load one or more ADI files from disk and return the QSO list
(define (load-adi filenames)
  (cond ((null? filenames) '())
        ((list? filenames)
         (append (load-adi (car filenames))
                 (load-adi (cdr filenames))))
        ((string? filenames)
         (with-input-from-file filenames read-adi))
        (else `())))

;; Write an ADI file to the default output port
(define (write-adi qsos)
  ;; Write a single field
  (define (write-field name value)
    (cond ((symbol? name) (write-field (symbol->string name) value))
          ((null? value) (write-field name ""))
          ((symbol? value) (write-field na,e (symbol->string value)))
          ((number? value) (write-field name (number->string value)))
          (value
           (display (string-append "<" name ":"
                                   (number->string (string-length value))
                                   ">" value "\n")))
          (else
           (display (string-append "<" name ">\n")))))

  ;; Write a list of fields
  (define (write-fields fields)
    (when (not (null? fields))
      (let ((field (car fields)))
        (write-field (car field) (cdr field)))
      (write-fields (cdr fields))))

  ;; Write a single QSO record
  (define (write-record qso)
    (write-fields qso)
    (write-field `eor #f)
    (write-char #\newline))
  
  ;; Write an adi header
  (define (write-header header)
    (print "SLog ADI 3.x File Export")
    (write-fields (car header))
    (write-field 'eoh #f)
    (write-char #\newline))

  ;; Write QSO records
  (define (write-qsos qsos)
    (when (not (null? qsos))
      (write-record (car qsos))
      (write-qsos (cdr qsos))))

  (write-header '(((adif_ver . "3.1.0") (programid . "SLog") (programversion . "1.0"))))
  ;; (write-header (adif-header adif))
  (write-qsos qsos))

;; Save an ADI file to disk
(define (save-adi filename adif)
  (with-output-to-file filename (lambda () (write-adi adif))))

;; Returns true if the first QSO has an earlier QSO date than the second QSO
(define (qso-date-time< qso-a qso-b)
  (define (date-time-string qso)
    (string-append (alist-value qso 'qso_date "00000000") (alist-value qso 'time_on "")))
  (string< (date-time-string qso-a) (date-time-string qso-b)))

;; Generate a less function for QSO callsigns that in turn uses the given less function
(define (qso-callsign-less less)
  (lambda (qso-a qso-b)
    (less (alist-value qso-a 'call "")
          (alist-value qso-b 'call ""))))

;; Returns true if the first callsign sorts before the second callsign
(define (qso-callsign< qso-a qso-b)
  ((qso-callsign-sort string<) qso-a qso-b))

;; Returns true if the QSO has been sent to eQSL
(define (eqsl-sent? qso)
  (let ((sent (cdr (assoc 'eqsl_qsl_sent qso))))
    (and (string? sent)
         (equal? (string-upcase sent) "Y"))))

;; Returns true if the QSO has been sent to eQSL
(define (eqsl-sent? qso)
  (let ((sent (cdr (assoc 'eqsl_qsl_sent qso))))
    (and (string? sent)
         (equal? (string-upcase sent) "Y"))))

;; Return the list of QSOs that have been sent to eQSL
(define (eqsl-sent qsos)
  (filter eql-sent qsos))

