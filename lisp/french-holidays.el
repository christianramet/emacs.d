;;; french-holidays.el --- French holidays -*- lexical-binding: t -*-

;; Author: Christian Ramet
;; Maintainer: Christian Ramet
;; Version: 1.0
;; Package-Requires:
;; Homepage:
;; Keywords:


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; To show French holidays (and only those) in your Emacs calendar,
;; put the following lines into your .emacs:
;;   (require 'french-holidays)
;;   (setq calendar-holidays holiday-french-holidays)

;; Source: https://www.emacswiki.org/emacs/french-holidays.el

;;; Code:

(eval-when-compile
  (require 'calendar)
  (require 'holidays))

(defvar holiday-french-holidays nil
  "French holidays")

(setq holiday-french-holidays
      `((holiday-fixed 1 1 "Jour de l'an")
        (holiday-fixed 1 6 "Épiphanie")
        (holiday-fixed 2 2 "Chandeleur")
        (holiday-fixed 2 14 "Saint Valentin")
        (holiday-fixed 5 1 "Fête du travail")
        (holiday-fixed 5 8 "Victoire des alliés")
        (holiday-fixed 6 21 "Fête de la musique")
        (holiday-fixed 7 14 "Fête nationale")
        (holiday-fixed 8 15 "Assomption")
        (holiday-fixed 11 11 "Armistice de 1918")
        (holiday-fixed 11 1 "Toussaint")
        (holiday-fixed 12 25 "Noël")
        (holiday-easter-etc 0 "Pâques")
        (holiday-easter-etc 1 "Lundi de Pâques")
        (holiday-easter-etc 39 "Ascension")
        (holiday-easter-etc 49 "Pentecôte")
        (holiday-easter-etc -47 "Mardi gras")
        (holiday-float 5 0 4 "Fête des mères") ;; dernier dimanche de mai ou
        ;; premier dimanche de juin si c'est le même jour que la pentecôte
        (holiday-float 6 0 3 "Fête des pères")))

(provide 'french-holidays)

;;; french-holidays.el ends here
