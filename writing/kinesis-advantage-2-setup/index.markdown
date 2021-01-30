---
title: "DRAFT: Kinesis Advantage 2 Setup"
date: 2021-01-30
tags: draft
---

**This is a work in progress that will change. Like to see it finished? Let me
know by sending me an email.**

In this article I explain how I have configured my Kinesis Advantage 2 keyboard
(US version) to also type Swedish characters.

# TL;DR

I have the following macros for Windows/Linux:

    tada

# Background

Even though I am a Swede, I am a long time user of the US keyboard layout. The
reason that I switched was that many keys common when programming are more
convenient to type on a US keyboard.

Because I am a Swede, I also have the need to type Swedish characters which are
not available on a US keyboard. I have solved that problem by [remapping
keys](/writing/xmodmap-on-fedora/index.html) in software like this:

          Alt_GR+[  =>  'å'
          Alt_GR+'  =>  'ä'
          Alt_GR+;  =>  'ö'
    Shift+Alt_GR+[  =>  'Å'
    Shift+Alt_GR+'  =>  'Ä'
    Shift+Alt_GR+;  =>  'Ö'

Basically, I use the modifier `Alt_GR` plus the key where the character is
located on a Swedish keyboard. For example, on a Swedish keyboard, the key
for 'Å' is located in the same position as the key for '[' on a US keyboard (to
the right of 'P').

My solution only works on Linux, which was fine up until I had to use Windows
on a regular basis.

Since I'm not that familiar with Windows, figuring out how to remap keys seemed
difficult, so I started to think of other solutions.

## Universal unicode input

Then I remembered that there is a universal way to enter any unicode character
in Linux. Perhaps there is something similar for Windows as well? It turns out
there is. The Wikipedia page [Unicode
input](https://en.wikipedia.org/wiki/Unicode_input) describes it well.

On Linux, the universal way to enter a unicode character is to hold
`Ctrl+Shift`, then type 'u', then type the unicode character hex code, then
release `Ctrl+Shift`.

To type 'å' on Linux, you use the key combination `Ctrl+Shift+u+00e5`.

On Windows, the universal way to enter a unicode character is to press and
hold `Alt`, then type the unicode character decimal number on the *keypad*,
then release `Alt`.

To type 'å' on Windows, you use the key combination `Alt+0229`.

`00e5` is the hexadecimal number for the unicode character 'å' which is used by
Linux. `0229` is the same decimal number used by Windows. The numbers for
different unicode characters can be
found on [this Wikipedia
page](https://en.wikipedia.org/wiki/List_of_Unicode_characters).

Now I had figured out a way to enter all the Swedish characters on a US
keyboard in both operating systems.

But typing the key combinations and remembering the unicode numbers was not
very convenient.

## Macros

Luckily for me, I am also a long time user of the Kinesis Advantage 2 keyboard.
One of its features is that it is programmable. You can make one keypress on
the physical keyboard produce multiple key presses as seen by the computer.
This is known as macros.

Here is an example of a macro for producing an 'å' on Windows:

    {the key to trigger the macro}>{speed9}{-lalt}{kp0}{kp2}{kp2}{kp9}{+lalt}

This instruct the keyboard to press and hold left alt, then press keypad 0,
keypad 2, keypad 2, keypad 9, and then release left alt. And this at speed 9
which is the fastest speed. This macro can be triggered by any key at my
choosing.

In total I will need 12 macros. First 3 macros for the lowercase 'å', 'ä', and
'ö'. Then 3 more for the uppercase versions. And all those 6 are different on
Linux and Windows, yielding in total 12 versions.

Now to the next question: How should I trigger these macros? How do I want to
enter the characters?

I would like to enter them as I am used to. A modifier key + the key where the
character is located on a Swedish keyboard.

## Keypad layer

The Kinesis Advantage 2 also has another feature knows as the keypad layer.
The keypad layer is a second layer with keys. It is toggled permanently with
the `keyp` button. But it can also be made active during the press and hold of
a single button.

I designated the left alt key to the button that, while pressed, would activate
the keypad layer. It looks like this:

    [lalt]>[kpshift]
    [kp-lalt]>[kpshift]

(As the [manual](https://kinesis-ergo.com/support/advantage2/#manuals) says,
the button needs to be remapped in both layers to be able to have a shift-like
behavior.)

When I have left alt pressed, the keypad layer is active, and all the keys on
the keyboard now perform different functions. By default the keypad layer has
some mappings for digits. I don't use them, so I can override them to mean
something else.

TODO: expand this section

This is what macros look like to

    {kp-\}>{speed9}{-lalt}{kp0}{kp2}{kp2}{kp9}{+lalt}
    {kp-lshift}{kp-\}>{speed9}{-lalt}{kp0}{kp1}{kp9}{kp7}{+lalt}

New way to enter Swedish characters:

          LeftAlt+\  =>  'å'
          LeftAlt+'  =>  'ä'
          LeftAlt+;  =>  'ö'
    Shift+LeftAlt+\  =>  'Å'
    Shift+LeftAlt+'  =>  'Ä'
    Shift+LeftAlt+;  =>  'Ö'

These are the same mappings as I had previously, but with a different modifier
key.

On a Swedish keyboard, the 'Å' key is located to the right of the 'P' key. On a
standard US keyboard, that is the '[' key. But on the Kinesis Advantage 2, that
is the '\' key.

How to know which macro to trigger, the Linux version or the Windows version?

## Hotkey layouts

Another feature of the Kinesis Advantage 2 keyboard is hotkey
layouts. These are custom layouts that can be activated with a hotkey
(`progm+<key>`).

I created two separate hotkey layouts: one for Linux and one for Windows. They
have some remappings in common, but have specific macros for producing Swedish
characters.

To switch between the layouts, I press `progm+L` for Linux and `progm+W` for
Windows. Every time I switch operating system, I switch hotkey layout.

## Mapping summary

Linux:

Windows:

    [lalt]>[kpshift]
    [kp-lalt]>[kpshift]

    {kp-\}>{speed9}{-lalt}{kp0}{kp2}{kp2}{kp9}{+lalt}
    {kp-lshift}{kp-\}>{speed9}{-lalt}{kp0}{kp1}{kp9}{kp7}{+lalt}
    {lshift}{kp-\}>{speed9}{-lalt}{kp0}{kp1}{kp9}{kp7}{+lalt}

    {kp-'}>{speed9}{-lalt}{kp0}{kp2}{kp2}{kp8}{+lalt}
    {kp-lshift}{kp-'}>{speed9}{-lalt}{kp0}{kp1}{kp9}{kp6}{+lalt}
    {lshift}{kp-'}>{speed9}{-lalt}{kp0}{kp1}{kp9}{kp6}{+lalt}

    {kpplus}>{speed9}{-lalt}{kp0}{kp2}{kp4}{kp6}{+lalt}
    {kp-lshift}{kpplus}>{speed9}{-lalt}{kp0}{kp2}{kp1}{kp4}{+lalt}
    {lshift}{kpplus}>{speed9}{-lalt}{kp0}{kp2}{kp1}{kp4}{+lalt}

I created two macros for the shifted uppercase letters. That is because I
wanted to be sure to trigger it no matter if I activated the keypad layer
before or after pressing left shift. I'm not sure if it matters, but I
suspected it might.

## General problem with universal input method

On Linux, not all text entry fields support the universal input method. This is
a bit annoying. Then I have to enter the text in another text field and copy
paste the value. For the applications I use, it works most of the time, so I
have tolerated it.

One place where it works but produces weird text is in Google Hangouts chat.
When entering a unicode character, the text turns into underlined for some
reason. I'm not sure if there is a problem with my version of Firefox or what
is going on there.

On Windows, the universal input method have worked for me everywhere I've
tried, but it is important to have the NumLock switched on. Otherwise the
macros will not work. Sometimes NumLock gets turned off without me doing it, so
sometimes the macros didn't work, and I thought it was a problem in a certain
text field. But it always turned out to be NumLock.

## Problems with the keypad layer

When typing a Swedish character followed by a space, a '0' is sometimes
inserted instead. That is because the space key is mapped to the '0' key in the
keypad layer. I suspect that it happens if space is pressed quickly after
triggering the macro so that the release of the LeftAlt key has not happened
when the space is pressed.

This happens sometimes, and one solution would be to remap the keypad '0' to
produce a space instead.

The problem should arise with other characters as well, but the '0' is the only
one I have encountered.

## Furure

Experiment with more symbols in the embedded layer? Put braces in there?

## Appendix: Kinesis Commands

This is how to create a new hotkey layout:

    ...

Once that is done, the files can be edited with v-drive:

    ...
