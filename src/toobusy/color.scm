;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

(define-module (toobusy color)
  #:use-module ((srfi srfi-1) #:select (filter-map))

  #:export (colorize-string
            colorized-display))

(define *color-list*
  `((CLEAR       .   "0")
    (RESET       .   "0")
    (BOLD        .   "1")
    (DARK        .   "2")
    (UNDERLINE   .   "4")
    (UNDERSCORE  .   "4")
    (BLINK       .   "5")
    (REVERSE     .   "6")
    (CONCEALED   .   "8")
    (BLACK       .  "30")
    (RED         .  "31")
    (GREEN       .  "32")
    (YELLOW      .  "33")
    (BLUE        .  "34")
    (MAGENTA     .  "35")
    (CYAN        .  "36")
    (WHITE       .  "37")
    (ON-BLACK    .  "40")
    (ON-RED      .  "41")
    (ON-GREEN    .  "42")
    (ON-YELLOW   .  "43")
    (ON-BLUE     .  "44")
    (ON-MAGENTA  .  "45")
    (ON-CYAN     .  "46")
    (ON-WHITE    .  "47")))

(define (get-color color)
  (assq-ref *color-list* color))

(define (generate-color colors)
  (let ((color-list
         (filter-map get-color colors)))
    (if (null? color-list)
        ""
        (string-append "\x1b[" (string-join color-list ";" 'infix) "m"))))

(define* (colorize-string-helper color str control #:optional (rl-ignore #f))
  (if rl-ignore
      (string-append "\x01" (generate-color color) "\x02" str "\x01" (generate-color control) "\x02")
      (string-append (generate-color color) str (generate-color control))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define* (colorize-string str color #:optional (rl-ignore #f))
  "Example: (colorize-string \"hello\" '(BLUE BOLD))"
  (and (not (list? color)) (error colorize-string "color should be a list!" color))
  (colorize-string-helper color str '(RESET) rl-ignore))

(define (colorized-display str color)
  "Example: (colorized-display \"hello\" '(BLUE BOLD))"
  (display (colorize-string str color)))
