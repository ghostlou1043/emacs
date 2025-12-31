;; init-const.el --- Define constants. -*- lexical-binding: t -*-
;; Linux,MacOS,NixOS,Windows
(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/nixosp
  (file-exists-p "/etc/NIXOS")
  "Are we running on a NixOS system?")

(defconst sys/linuxp
  (and (eq system-type 'gnu/linux)
       (not sys/nixosp))
  "Are we running on a GNU/Linux system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/nixos-x-p
  (and (display-graphic-p) sys/nixosp)
  "Are we running under X on a NixOS system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst kaladin/lsp-bridge nil)

(defconst kaladin/drivish nil)

(provide 'init-const)
