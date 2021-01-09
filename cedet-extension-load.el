;;; cedet-extension-load.el --- Utility for loading in CEDET extensions
;;
;; Copyright (C) 2021 Eric Ludlam
;;
;; Author: Eric Ludlam <zappo@gnu.org>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see https://www.gnu.org/licenses/.

;;; Commentary:
;;
;; Setup paths to load code from the file layout of the cedet
;; extensions git repository.

;;; Code:

(when (featurep 'cedet-extension-load)
  (error "CEDET extensions already loaded."))

;; This file must be in "<INSTALL-DIR>" where lisp/cedet is a subdirectory.
(let ((CE-DIR (file-name-directory
	       (or load-file-name (buffer-file-name)))))

  ;; SETUP LOAD PATHS
  (add-to-list 'load-path (expand-file-name "lisp/cedet" CE-DIR))

  ;; Requirements before pulling in loaddefs
  (require 'ede)

  ;; Load in the loaddefs
  (load (expand-file-name "lisp/cedet/loaddefs.el" CE-DIR) nil t t)
  (load (expand-file-name "lisp/cedet/semantic/loaddefs.el" CE-DIR) nil t t)
  (load (expand-file-name "lisp/cedet/ede/loaddefs.el" CE-DIR) nil t t)
  
  )

(provide 'cedet-extension-load)

;;; cedet-extension-load.el ends here
