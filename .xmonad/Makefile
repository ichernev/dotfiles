all: xmonad-i386-linux

lib/XMonad/Hooks/PerWindowKbdLayout.hs: lib/XMonad/Hooks/PerWindowKbdLayout.hsc
	hsc2hs $<

xmonad-i386-linux: lib/XMonad/Hooks/PerWindowKbdLayout.hs xmonad.hs
	xmonad --recompile

.PHONY: all
