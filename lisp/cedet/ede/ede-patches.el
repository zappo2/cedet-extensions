;;; ede-patches.el --- Patches to make various versions of Emacs work with these extensions
;;
;; Copyright (C) 2021 Eric Ludlam
;;
;; Author: Eric Ludlam <zappo@ballista>
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
;; Stomp on parts of core emacs to enable features needed by these extensions.

;;; Code:

(require 'ede/auto)
(require 'ede/proj) ;; So we can patch it

;; WARNING: ORDER DEPENDENCY
;; Type class type patches need to happen in a particular order
;; so changes to baseclass can work their way into subclasses.
(when (featurep 'ede/proj-elisp)
  (error "ede patches loaded too late."))

;; This function is needed early on for patching up data types in
;; various classes.
(defun ede-patches-type-of-slot (class slot &optional newtype)
  "For CLASS and SLOT Get the :type.  If NEWTYPE is specified, modify it."
  ;; Most of this is copied from eieio-oset and validate-slot Emacs
  ;; 26, and hacked to focus on the type of the slot.  All error
  ;; checking was removed since this is just about patching up old
  ;; Emacsen
  (let* ((class (eieio--class-object class))
         (slot-idx (eieio--slot-name-index class slot)))
    (if (not slot-idx)
	;; It might be missing because it is a :class allocated slot.
	;; Let's check that info out.
	(error "Cannot patch %S %S: slot not found" class slot)
      ;; Here we can  get the type
      ;;(eieio--validate-slot-value class c value slot)

      ;; Trim off object IDX junk added in for the object index.
      (let* ((slot-idx (- slot-idx (eval-when-compile eieio--object-num-slots)))
	     (cslots (eieio--class-slots class))
	     (slotv (aref cslots slot-idx)))

	(if newtype
	    (progn
					;(message "Setting ruletype to %S" newtype)
	      (setf (cl--slot-descriptor-type slotv) newtype))
					;(message "newtype not specified, doing get")
	  (cl--slot-descriptor-type slotv))))))

(when (eq (ede-patches-type-of-slot 'ede-proj-target-makefile 'rules) 'list)
  ;; Type type from ancient CEDET was 'list for this slot.  This was fixed in CEDET
  ;; repository, but patches didn't make it into EMACS.  This patches that up.
  (ede-patches-type-of-slot 'ede-proj-target-makefile 'rules 'ede-makefile-rule-list)
  )

;; These for testing
;(ede-patches-type-of-slot 'ede-proj-target-makefile 'rules)
;(ede-patches-type-of-slot 'ede-proj-target-elisp 'rules)


;; PATCH: 
(when (>= emacs-major-version 24)
  (when (< emacs-major-version 27)
    ;; emacs 25&26 doesn't autoload classes, so projet loading fails.  Need to bootstrap.
    ;; But I fixed in Emacs 27
    (require 'ede/proj-elisp)
    (require 'ede/proj-misc)
    ;; how to enable these w/ e25?
    ;; (require 'ede/linux)
    ;; (require 'ede/emacs)
    ;; (require 'ede/android)
    ;; (require 'ede/arduino)
    )
  )


(when (not (fboundp 'ede-calc-fromconfig-in-patch))
  ;; CEDET from sourceforge includes the below code.
  ;; Patch was provided for Emacs 28 that will declare the above fcn
  ;; and prevent loading this section.

  (cl-defmethod ede-calc-fromconfig-in-patch ((dirmatch ede-project-autoload-dirmatch))
    "Calculate the value of :fromconfig from DIRMATCH."
    (let* ((fc (oref dirmatch fromconfig))
	   (found (cond ((stringp fc) fc)
			((functionp fc) (funcall fc))
			(t (error "Unknown dirmatch object match style.")))))
      (expand-file-name found)
      ))

  (cl-defmethod ede-dirmatch-installed ((dirmatch ede-project-autoload-dirmatch))
    "Return non-nil if the tool DIRMATCH might match is installed on the system."
    (file-exists-p (ede-calc-fromconfig-in-patch dirmatch)))

  (cl-defmethod ede-do-dirmatch ((dirmatch ede-project-autoload-dirmatch) file)
    "Does DIRMATCH match the filename FILE."
    (let ((fc (ede-calc-fromconfig-in-patch dirmatch)))

      (cond
       ;; If the thing to match is stored in a config file.
       ((stringp fc)
	(when (file-exists-p fc)
	  (let ((matchstring
		 (if (slot-boundp dirmatch 'configdatastash)
		     (oref dirmatch configdatastash)
		   nil)))
	    (when (and (not matchstring) (not (slot-boundp dirmatch 'configdatastash)))
	      (save-current-buffer
		(let* ((buff (get-file-buffer fc))
		       (readbuff
			(let ((find-file-hook nil)) ;; Disable ede from recursing
			  (find-file-noselect fc))))
		  (set-buffer readbuff)
		  (save-excursion
		    (goto-char (point-min))
		    (when (re-search-forward (oref dirmatch configregex) nil t)
		      (setq matchstring
			    (match-string (or (oref dirmatch configregexidx) 0)))))
		  (if (not buff) (kill-buffer readbuff))))
	      (when matchstring
		;; If this dirmatch only finds subdirs of matchstring, then
		;; force matchstring to be a directory.
		(when (oref dirmatch subdir-only)
		  (setq matchstring (file-name-as-directory matchstring)))
		;; Convert matchstring to a regexp
		(setq matchstring (concat "^" (regexp-quote matchstring)))
		;; Stash it for later.
		(oset dirmatch configdatastash matchstring))
	      ;; Debug
	      ;;(message "Stashing config data for dirmatch %S as %S" (eieio-object-name dirmatch) matchstring)
	      )
	    ;;(message "dirmatch %s against %s" matchstring (expand-file-name file))
	    ;; Match against our discovered string
	    (setq file (file-name-as-directory (expand-file-name file)))
	    (and matchstring (string-match matchstring (expand-file-name file))
		 (or (not (oref dirmatch subdir-only))
		     (not (= (match-end 0) (length file))))
		 )
	    )))

       ;; Add new matches here
       ;; ((stringp somenewslot ...)
       ;;   )

       ;; Error if none others known
       (t
	(error "Unknown dirmatch object match style.")))
      ))

  ) ;; Patch on how config files are identified

(when (not (child-of-class-p 'ede-proj-project 'eieio-named))
  ;; Older EDE didn't subclass eieio-named, and doesn't get :object-name slot.
  ;; We'll need to dynamically hack ede-proj-project and ede-target
  ;; to have this slot so that we can load Project files form newer
  ;; versions of Emacs.

  ;; Do this with the slot-unbound method.  This isn't bound on these
  ;; classes yet, and we can use it to fake existence of these names.

  (cl-defmethod slot-missing ((obj ede-proj-project)
			      slot-name operation &optional new-value)
    "Called when a non-existent slot is accessed.
For `ede-proj-project', provide an imaginary `:object-name' slot.
Argument OBJ is the named object.
Argument SLOT-NAME is the slot that was attempted to be accessed.
OPERATION is the type of access, such as `oref' or `oset'.
NEW-VALUE is the value that was being set into SLOT if OPERATION were
a set type."
    (if (memq slot-name '(object-name :object-name))
	(cond ((eq operation 'oset)
	       (if (not (stringp new-value))
		   (signal 'invalid-slot-type
			   (list obj slot-name 'string new-value)))
	       (eieio-object-set-name-string obj new-value))
	      (t (eieio-object-name-string obj)))
      (call-next-method)))

  (cl-defmethod slot-missing ((obj ede-target)
			      slot-name operation &optional new-value)
    "Called when a non-existent slot is accessed.
For `ede-proj-project', provide an imaginary `:object-name' slot.
Argument OBJ is the named object.
Argument SLOT-NAME is the slot that was attempted to be accessed.
OPERATION is the type of access, such as `oref' or `oset'.
NEW-VALUE is the value that was being set into SLOT if OPERATION were
a set type."
    (if (memq slot-name '(object-name :object-name))
	(cond ((eq operation 'oset)
	       (if (not (stringp new-value))
		   (signal 'invalid-slot-type
			   (list obj slot-name 'string new-value)))
	       (eieio-object-set-name-string obj new-value))
	      (t (eieio-object-name-string obj)))
      (call-next-method)))
  
  )

(provide 'ede/ede-patches)

;;; ede-patches.el ends here
