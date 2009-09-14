;;; typing-outputz.el --- 

;; Copyright (C) 2009  Yuto Hayamizu

;; Author: Yuto Hayamizu <y.hayamizu@gmail.com>
;; Keywords: Outputz

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This elisp helps you record the number of characters you typed on
;; Outputz.com
;;

;;; Installation:
;;
;; To use this elisp, locate typing-outputz.el in a directory included
;; in 'load-path', and write following codes in your .emacs
;;
;;     (require 'typing-outputz)
;;     (global-typing-outputz-mode t)
;;     (setq typing-outputz-key "*****-*****")
;;

;;; Tips:
;;
;; In default configuration, only 'self-insert-command' is the command
;; to be counted. Of course, you can add commands to be counted by
;; adding them to 'typing-outputz-counted-commands'
;; For example, if you are using SKK, you may want this:
;;
;;     (add-to-list 'typing-outputz-counted-commands
;;                  'skk-insert)
;;

;;
;; If 'notify-send' command is available on your machine, following
;; codes enables pop-up notification on update.
;;
;;     (defun to-growl-record-buffer ()
;;       (let ((growl-command-template
;;              (cond
;;               ((eq 0 (shell-command "which notify-send"))
;;                "notify-send -i gnome-emacs \"Emacs notify\" \"%s\"")
;;               (t nil))))
;;         (when growl-command-template
;;           (shell-command
;;            (format growl-command-template
;;                    (format "Outputz: %d chars"
;;                            typing-outputz-buffer-local-counter))))))
;;     
;;     (add-hook 'typing-outputz-record-buffer-hook
;;               'to-growl-record-buffer nil)


;;; Code:


(require 'easy-mmode)
(require 'url)

(defvar typing-outputz-key nil)
(defvar typing-outputz-url nil)

(defvar typing-outputz-url-generator-function
  'typing-outputz-default-url-generator)

(defvar typing-outputz-record-idle-timer nil)

(defcustom typing-outputz-record-buffer-hook nil
  "Hook that is run after a buffer is recorded on Outputz."
  :type 'hook)

(defcustom typing-outputz-counted-commands '(self-insert-command)
  "Commands that should be counted as typing."
  :type '(list symbol))

;; mailcap.el problem: See http://d.hatena.ne.jp/hayamiz/20081121/1227228535
(unless (fboundp 'mailcap-parse-mailcaps)
  (block nil
    (dolist (path load-path)
      (when (or (file-exists-p (expand-file-name "mailcap.el" path))
		(file-exists-p (expand-file-name "mailcap.elc" path)))
	(load (expand-file-name "mailcap" path)))
      (when (fboundp 'mailcap-parse-mailcaps)
	(return)))))

(defun typing-outputz-percent-encode (str &optional coding-system)
  (if (or (null coding-system)
	    (not (coding-system-p coding-system)))
      (setq coding-system 'utf-8))
  (mapconcat
   (lambda (c)
     (cond
      ((typing-outputz-url-reserved-char-p c)
       (char-to-string c))
      ((eq c ? ) "+")
      (t (format "%%%x" c))))
   (encode-coding-string str coding-system)
   ""))

(defun typing-outputz-url-reserved-char-p (ch)
  (or (and (<= ?A ch) (<= ch ?z))
      (and (<= ?0 ch) (<= ch ?9))
      (eq ?. ch)
      (eq ?- ch)
      (eq ?_ ch)
      (eq ?~ ch)))


(defun typing-outputz-make-local-counter (&optional value)
  (unless value (setq value 0))
  (set (make-local-variable 'typing-outputz-buffer-local-counter) value))

(defun typing-outputz-ensure-counter (&optional value)
  (unless (boundp 'typing-outputz-buffer-local-counter)
    (typing-outputz-make-local-counter))
  (when value (setq typing-outputz-buffer-local-counter value)))

(defun typing-outputz-set-counter (value)
  (typing-outputz-ensure-counter 0))

(defun typing-outputz-increment-local-counter (&optional delta)
  (unless delta (setq delta 1))
  (typing-outputz-ensure-counter)
  (incf typing-outputz-buffer-local-counter delta))

(defun typing-outputz-handle-post-command ()
  (when (and typing-outputz-mode
	     (memq this-command typing-outputz-counted-commands))
    (typing-outputz-increment-local-counter 1)))

(define-minor-mode typing-outputz-mode
  "Typing Outputz mode"
  :lighter " TypOutputz"
  :group 'typing-outputz
  (if typing-outputz-mode
      (progn
        (typing-outputz-make-local-counter)
	(add-hook 'post-command-hook 'typing-outputz-handle-post-command nil)
        (run-hooks 'typing-outputz-mode-hook))
    nil))

(defun typing-outputz-mode-maybe ()
  "Return t and enable typing-outputz-mode if `typing-outputz-mode' can called on current buffer."
  (unless (minibufferp (current-buffer))
    (typing-outputz-mode t)))

(define-global-minor-mode global-typing-outputz-mode
  typing-outputz-mode typing-outputz-mode-maybe
  :group 'typing-outputz)

(defun typing-outputz-default-url-generator ()
  (typing-outputz-percent-encode
   (replace-regexp-in-string
	  "c\\+\\+" "cpp"
	  (format "http://%s.localhost/%s/%s/%s"
	   (symbol-name major-mode)
	   (system-name)
	   (user-login-name)
	   (file-name-nondirectory (or (buffer-file-name) (buffer-name)))))))

(defun typing-outputz-record-buffers ()
  (dolist (buf (buffer-list))
    (typing-outputz-record-buffer buf)))

(defun typing-outputz-record-buffer (buf)
  (with-current-buffer buf
    (if typing-outputz-mode
	(progn
	  (typing-outputz-ensure-counter)
	  (when (> typing-outputz-buffer-local-counter 0)
	    (let ((url "http://outputz.com/api/post")
		  (url-request-method "POST")
		  (url-request-extra-headers
		   '(("Content-Type" . "application/x-www-form-urlencoded")))
		  (url-request-data
		   (format "key=%s&uri=%s&size=%d"
			   typing-outputz-key
			   (funcall typing-outputz-url-generator-function)
			   typing-outputz-buffer-local-counter)))
	      (url-retrieve url 'typing-outputz-record-callback))
	    (run-hooks 'typing-outputz-record-buffer-hook)
	    (typing-outputz-set-counter 0)
	    )))))

(defun typing-outputz-record-callback (status)
  (url-mark-buffer-as-dead (current-buffer))
  status t)

(unless typing-outputz-record-idle-timer
  (setq typing-outputz-record-idle-timer
	(run-with-idle-timer 30 t 'typing-outputz-record-buffers)))

(provide 'typing-outputz)
;;; typing-outputz.el ends here
