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

(define adif-data-types
  '((address . string)
    (age . number)
    (a_index . number)
    (ant_az . number)
    (ant_el . number)
    (ant_path . string)
    (arrl_sect . string)
    (award_submitted . list)
    (award_granted . list)
    (band . string)
    (band_rx . string)
    (call . string)
    (check . string)
    (class . string)
    (clublog_qso_upload_date . date)
    (clublog_qso_upload_status . string)
    (cnty . string)
    (comment . string)
    (cont . string)
    (contacted_op . string)
    (contest_id . string)
    (country . string)
    (cqz . number)
    (credit_submitted . list)
    (credit_granted . list)
    (dark_dok . string)
    (distance . number)
    (dxcc . string)
    (email . string)
    (eq_call . string)
    (eqsl_qslrdate . date)
    (eqsl_qslsdate . date)
    (eqsl_qsl_rcvd . string)
    (eqsl_qsl_sent . string)
    (fists . number)
    (fists_cc . number)
    (force_init . boolean)
    (freq . number)
    (freq_rx . number)
    (gridsquare . string)
    (hrdlog_qso_upload_date . date)
    (hrdlog_qso_upload_status . string)
    (iota . string)
    (iota_island_id . number)
    (ituz . number)
    (k_index . number)
    (lat . string)
    (lon . string)
    (lotw_qslrdate . date)
    (lotw_qslsdate . date)
    (lotw_qsl_rcvd . string)
    (lotw_qsl_sent . string)
    (max_bursts . number)
    (mode . string)
    (ms_shower . string)
    (my_antenna . string)
    (my_city . string)
    (my_cnty . string)
    (my_country . string)
    (my_cq_zone . number)
    (my_dxcc . string)
    (my_fists . number)
    (my_gridsquare . string)
    (my_iota . string)
    (my_iota_island_id . number)
    (my_itu_zone . number)
    (my_lat . string)
    (my_lon . string)
    (my_name . string)
    (my_postal_code . string)
    (my_rig . string)
    (my_sig . string)
    (my_sig_info . string)
    (my_sota_ref . string)
    (my_state . string)
    (my_street . string)
    (my_usaca_counties . list)
    (my_vucc_grids . list)
    (name . string)
    (notes . string)
    (nr_bursts . number)
    (nr_pings . number)
    (operator . string)
    (owner_callsign . string)
    (pfx . string)
    (precedence . string)
    (prop_mode . string)
    (public_key . string)
    (qrzcom_qso_upload_date . date)
    (qrzcom_qso_upload_status . string)
    (qslmsg . string)
    (qslrdate . date)
    (qslsdate . date)
    (qsl_rcvd . string)
    (qsl_rcvd_via . string)
    (qsl_sent . string)
    (qsl_sent_via . string)
    (qsl_via . string)
    (qso_complete . string)
    (qso_date . date)
    (qso_date_off . date)
    (qso_random . boolean)
    (qth . string)
    (region . string)
    (rig . string)
    (rst_rcvd . string)
    (rst_sent . string)
    (rx_pwr . number)
    (sat_mode . string)
    (sat_name . string)
    (sfi . number)
    (sig . string)
    (sig_info . string)
    (silent_key . boolean)
    (skcc . string)
    (sota_ref . string)
    (srx . number)
    (srx_string . string)
    (state . string)
    (station_callsign . string)
    (stx . number)
    (stx_string . string)
    (submode . string)
    (swl . boolean)
    (ten_ten . number)
    (time_off . time)
    (time_on . time)
    (tx_power . number)
    (uksmg . number)
    (usaca_counties . list)
    (vucc_grids . list)
    (web . string)))
