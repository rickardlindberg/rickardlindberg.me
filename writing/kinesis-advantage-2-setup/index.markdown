---
title: "DRAFT: Kinesis Advantage 2 Setup"
date: 2020-10-03
tags: draft
---

**This is a work in progress that will change. Like to see it finished? Let me
know by sending me an email.**

* Do I want to describe my whole setup or just Swedish characters

# Typing Swedish characters on a US version of Kinesis Advantage 2

I've been using a Kinesis Advantage 2 keyboard for a few years. I've recently
had to change its configuration, and it this article I explain what the setup
looks like now and why it is that way.

I have the US
version, and I prefer the US keyboard layout because may characters that are
common in programming are easier to type on a US keyboard compared to a Swedish
keyboard.

However, I sometimes need to type Swedish characters. Previously I have solved
that by [remapping keys in Xmodmap](/writing/xmodmap-on-fedora/index.html) to
produce the Swedish characters.

Recently I have had to use a Windows computer where Xmodmap is obviously not
available. Instead of figuring out how to remap keys in Windows, I started to
think if I could configure my Advantage keyboard with macros to produce the
Swedish characters.

On both Linux and Windows it is possible to enter any unicode character by
entering its code. Wikipedia [describes
how](https://en.wikipedia.org/wiki/Unicode_input#In_X11_(Linux_and_other_Unix_variants_including_Chrome_OS)).

So to type the Swedish character "å" you have to do the following sequence:

* Linux: Press and hold Ctrl+Shift, press 'u', press '0', press '0', press 'e',
  press
  '5', release Ctrl+Shift.
* Windows: Press and hold Alt, press, Keypad_0, press Keypad_2, press Keypad_2,
  press Keypad_9, release Alt.

`00e5` is the hexadecimal number for the unicode character 'å' which is used by
Linux. `0229` is the same decimal number used by Windows. The numbers can be
found on [this Wikipedia
page](https://en.wikipedia.org/wiki/List_of_Unicode_characters).

## How I typed 'å' before

## Problems

### Numlock on Windows

NumLock important on Windows.

### Underscores in Hangouts

### Inserting 0 after åäö

# Updrage process

I upgraded Kinesis Advantage 2 keyboard and made mappings for Windows and
Linux.

Before:

    Model name> Advantage2 Keyboard
    Firmware version> 1.0.0.us (2MB), 06/29/2016
    Current keyboard config file> w_qwerty.txt
    Current thumb keys mode> pc
    Macro playback status> active
    Macro playback speed> slow=1, normal=3, fast=9> 3
    Keyclick status> clicks off
    Toggle action tone status> tones on
    Number of stored macros> 9
    Number of keys remapped> 5
    Power user mode> on

After:

    Model> Advantage2
    Firmware> 1.0.521.us (2MB), 06/25/2020
    Active layout file> w_qwerty.txt
    Thumb keys mode> none
    Macro play speed> off=0, slow=1, normal=3, fast=9> 3
    Status report play speed> off=0, slow=1, normal=3, fast=4> 3
    Keyclick status> off
    Toggle tone status> on
    Stored macros> 9
    Keys remapped> 5
    Power user mode> on

Mappings Windows:

    [caps]>[lctrl]
    [lalt]>[kpshift]
    [kp-lalt]>[kpshift]
    [lctrl]>[lalt]
    [rctrl]>[enter]

    {kp-\}>{speed9}{-lalt}{kp0}{kp2}{kp2}{kp9}{+lalt}
    {kp-lshift}{kp-\}>{speed9}{-lalt}{kp0}{kp1}{kp9}{kp7}{+lalt}
    {lshift}{kp-\}>{speed9}{-lalt}{kp0}{kp1}{kp9}{kp7}{+lalt}

    {kp-'}>{speed9}{-lalt}{kp0}{kp2}{kp2}{kp8}{+lalt}
    {kp-lshift}{kp-'}>{speed9}{-lalt}{kp0}{kp1}{kp9}{kp6}{+lalt}
    {lshift}{kp-'}>{speed9}{-lalt}{kp0}{kp1}{kp9}{kp6}{+lalt}

    {kpplus}>{speed9}{-lalt}{kp0}{kp2}{kp4}{kp6}{+lalt}
    {kp-lshift}{kpplus}>{speed9}{-lalt}{kp0}{kp2}{kp1}{kp4}{+lalt}
    {lshift}{kpplus}>{speed9}{-lalt}{kp0}{kp2}{kp1}{kp4}{+lalt}


## Installation

https://kinesis-ergo.com/wp-content/uploads/Adv2-Users-Manual-fw1.0.517-4-24-20.pdf

How to know which keyboard model I should use?

Landet keyboard. Before:

    Model> Advantage2
    Firmware> 1.0.431.us (4MB), 11/14/2017
    Active layout file> qwerty.txt
    Thumb keys mode> none
    Macro play speed> off=0, slow=1, normal=3, fast=9> 3
    Status report play speed> off=0, slow=1, normal=3, fast=4> 3
    Keyclick status> off
    Toggle tone status> on
    Stored macros> 0
    Keys remapped> 5

After:

    Model> Advantage2
    Firmware> 1.0.521.us (4MB), 06/25/2020
    Active layout file> l_qwerty.txt
    Thumb keys mode> none
    Macro play speed> off=0, slow=1, normal=3, fast=9> 3
    Status report play speed> off=0, slow=1, normal=3, fast=4> 3
    Keyclick status> off
    Toggle tone status> on
    Stored macros> 9
    Keys remapped> 5

Had to unmount v-drive for update to take effect. Step 6.5 in this
https://kinesis-ergo.com/wp-content/uploads/Adv2-Firmware-Update-Instructions-4-2-19.pdf.

## Furure

Experiment with more symbols in the embedded layer? Put braces in there?
