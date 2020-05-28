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


(include "adif.scm")

;; Test adif library
(define (adif-test)
  (let ((input (string-append
                "# Comment <foo:3>bar\n"
                "<ADIF_VER:5>3.0.4\n"
                "<PROGRAMID:10>QRZLogBook\n"
                "<PROGRAMVERSION:3>2.0\n"
                "<eoh>\n"
                "<BAND:4>70cm <mode:2>FM <callsign:6>AB2CDE\n"
                "<ADDRESS:19>somewhere\nsomeplace\n<EOR> "
                "<band:2>2m   <mode:4>C4FM<callsign:4>A2BC<eor>\n"
                "<mode:2>AM"))
        (expected '((header ((adif_ver . "3.0.4")
                             (programid . "QRZLogBook")
                             (programversion . "2.0")))
                    (qsos ((band . "70cm")
                           (mode . "FM")
                           (callsign . "AB2CDE")
                           (address . "somewhere\nsomeplace"))
                          ((band . "2m")
                           (mode . "C4FM")
                           (callsign . "A2BC"))
                          ((mode . "AM")))))
        (expected-output (string-append
                          "<adif_ver:5>3.0.4\n"
                          "<programid:10>QRZLogBook\n"
                          "<programversion:3>2.0\n"
                          "<eoh>\n\n"
                          "<band:4>70cm\n<mode:2>FM\n<callsign:6>AB2CDE\n"
                          "<address:19>somewhere\nsomeplace\n<eor>\n\n"
                          "<band:2>2m\n<mode:4>C4FM\n<callsign:4>A2BC\n<eor>\n\n"
                          "<mode:2>AM\n<eor>\n\n")))
    (define (check-value expected actual name)
      (if (equal? expected actual)
          (begin
            (print (string-append "Passed: " name))
            #t)
          (begin
            (print (string-append "Failed: " name))
            (print "Expected:\n")
            (write expected)
            (print "\n\n")
            (print "Actual:\n")
            (write actual)
            (print "\n")
            #f)))
      
    (let ((adif (with-input-from-string input read-adi)))
      (if (check-value expected adif "Parse ADI")
          (let ((output (with-output-to-string (lambda () (write-adi adif)))))
            (check-value expected-output output "Generate ADI"))))))
