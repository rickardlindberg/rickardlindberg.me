---
title: Xmodmap on Fedora
date: 2014-09-17
---

*This text was in my notes for quite a while before I published it on my
website.*

I have the following in my `~/.Xmodmap`:

    ! setup mode switch on Alt_GR
    keysym Alt_R = Mode_switch

    ! remap Alt_GR + [ ' ; to å ä ö
    keycode 34 = bracketleft braceleft aring Aring aring Aring
    keycode 48 = apostrophe quotedbl adiaeresis Adiaeresis adiaeresis Adiaeresis
    keycode 47 = semicolon colon odiaeresis Odiaeresis odiaeresis Odiaeresis

I use it to be able to type the Swedish characters on a US keyboard. For most
of my work, I prefer the US keyboard layout, but I sometimes have to type
Swedish characters. With this setting, I can press for example "Alt_GR + [" to
get an "å".

## Problem with Alt + Shift order

When pressing "Alt_GR + Shift_L + [" I get an "Å" (as expected).

When pressing "Shift_L + Alt_GR + [" I get an "{" (but expected an "Å").

Checking the output from xev using `xev | grep KeyPress\\\|keysym` I got the
following:

Pressing "Alt_GR + Shift_L + [":

    KeyPress event, serial 30, synthetic NO, window 0x4600001,
    	state 0x10, keycode 108 (keysym 0xff7e, Mode_switch), same_screen YES,
    KeyPress event, serial 30, synthetic NO, window 0x4600001,
    	state 0x2010, keycode 50 (keysym 0xffe1, Shift_L), same_screen YES,
    KeyPress event, serial 30, synthetic NO, window 0x4600001,
    	state 0x2011, keycode 34 (keysym 0xc5, Aring), same_screen YES,

Pressing "Shift_L + Alt_GR + [":

    KeyPress event, serial 30, synthetic NO, window 0x4600001,
    	state 0x10, keycode 50 (keysym 0xffe1, Shift_L), same_screen YES,
    KeyPress event, serial 30, synthetic NO, window 0x4600001,
    	state 0x11, keycode 108 (keysym 0x0, NoSymbol), same_screen YES,    <-- NoSymbol
    KeyPress event, serial 30, synthetic NO, window 0x4600001,
    	state 0x11, keycode 34 (keysym 0x7b, braceleft), same_screen YES,

On my Debian system, the output from xev looks the same no matter in which
order I press Shift_L and Alt_GR, but for some reason this does not work on my
Fedora 14 system. Any ideas what might be wrong? Why do I get the NoSymbol on
Fedora? (Fedora 20 has the same problem.)

I have used the same keyboard on both systems, so I don't believe this problem
is hardware related.

## Suspend after resume

On my Fedora 14 system, I had the following in `/usr/lib/systemd/system-sleep/run-xmodmap.sh`:

    #!/bin/sh
    if [ "$1" = "post" ] ; then
    	typeset -x DISPLAY=:0.0
    	sleep 1
    	su -c "/usr/bin/xmodmap ~/.Xmodmap" rick
    fi

Without it, I had to type `xmodmap ~/.Xmodmap` manually each time after
resuming my laptop from sleep. In Fedora 20 this seems to be no longer
required.
