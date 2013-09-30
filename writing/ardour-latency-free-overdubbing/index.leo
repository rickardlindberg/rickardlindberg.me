<?xml version="1.0" encoding="utf-8"?>
<!-- Created by Leo (http://webpages.charter.net/edreamleo/front.html) -->
<?xml-stylesheet ekr_test?>
<leo_file xmlns:leo="http://www.leo-editor.org/2011/leo" >
<leo_header file_format="2" tnodes="0" max_tnode_index="0" clone_windows="0"/>
<globals body_outline_ratio="0.5" body_secondary_ratio="0.5">
	<global_window_position top="50" left="50" height="500" width="700"/>
	<global_log_window_position top="0" left="0" height="0" width="0"/>
</globals>
<preferences/>
<find_panel_settings/>
<vnodes>
<v t="rick.20130921142352.1458" a="E"><vh>@rst index.html</vh>
<v t="rick.20130920195639.1450"><vh>Where is latency introduced?</vh></v>
<v t="rick.20130920195639.2895" a="E"><vh>Measure I/O latency</vh>
<v t="rick.20130920195639.2896"><vh>Setup</vh></v>
<v t="rick.20130921144244.1473"><vh>Process</vh></v>
<v t="rick.20130921144244.1471" a="E"><vh>Results</vh>
<v t="rick.20130921144244.1472"><vh>USB and 48kHz</vh></v>
</v>
</v>
<v t="rick.20130920195639.2897"><vh>@rst-ignore Notes</vh></v>
</v>
</vnodes>
<tnodes>
<t tx="rick.20130920195639.1450">Let's say we have one track with a metronome click. Now we want to record a
chord progression over this metronome click. ::

    playback  |&gt;   &gt;   &gt;   &gt;   |&gt;   &gt;   &gt;   &gt;   |
    ----------|----------------|----------------|
    recording |                |                |
                               t

At time t, we are at the beginning of beat 2. It starts with a metronome click.
At time t, Ardour sends the first sample of the metronome click to the speakers.
It passes various stages in the computer and a DA converter. It will take an
amout of time before we actually hear the sound. We call that output delay (OD).
When we hear the sound we react to it and play a chord on our instrument. It
will take an amount of time for our microphone to capture the sound, convert it
to a digital signal, and send it to Ardour. We call that input delay (ID).

* At time t       - Ardour sends playback signal to speakers
* At time t+OD    - We hear the metronome tick
* At time t+OD+ID - Our reaction has been recorded and arrived in Ardour

The time between Ardour sending the signal to the speakers and it receiving a
recorded signal back is thus OD+ID.

In order for the two tracks to line up perfectly, Ardour has to compensate for
this latency. It can do that, but you need to know the input and output latency
of your system.</t>
<t tx="rick.20130920195639.2895"></t>
<t tx="rick.20130920195639.2896">I use a Zoom H4n as a USB audio interface. It supports 44.1kHz and 48kHz.

The first way to close the loop is with a direct patch cable:

.. image:: direct.jpg

I connect the patch cable from the headphone line out to a line in. The H4n has
combo inputs that works for microphones, electric instrument, or line level
sources.

The second way to close the loop is with a speaker and a microphone:

.. image:: mic.jpg

I connect headphones to the headphone line out and put them close to the built
in microphone on the H4n.</t>
<t tx="rick.20130920195639.2897">http://www.linuxmusicians.com/viewtopic.php?f=27&amp;t=7702

so, I found it out. I get constant measurements only with SR of 48.000 and frames/period multiples of 48 :( :(
yea, usb sucks in some things


&lt;ricli85&gt; Some reports on the increasing number that jack_iodelay gives me: If I set 48kHz sample rate and frames/period=1056 (multiple of 48), I seem to get a more stable number.
&lt;las&gt; ricli85: many USB devices work correctly *only* at 48kHz
* idragosani has quit (Quit: Leaving.)
&lt;las&gt; ricli85: their transfer interval is also 1msec, which is unrelated to most period-of-two numbers used with most PCI devices for buffer/period size


ricli85&gt; I also got different roundtrip delays when using the performance governor.
&lt;ricli85&gt; But the delay still seemed constant when using the power saving governor.


http://manual.ardour.org/synchronization/latency-and-latency-compensation/
http://apps.linuxaudio.org/wiki/jack_latency_tests</t>
<t tx="rick.20130921142352.1458">@ @rst-options
call_docutils=True
code_mode=False
stylesheet_path=.
write_intermediate_file=False
@c

##################################
Latency free overdubbing in Ardour
##################################

In this article I explain how to set up JACK and Ardour for latency free
overdubbing.

Please send feedback to ricli85@gmail.com.</t>
<t tx="rick.20130921144244.1471">Measurements on my system gave the following values:

================= ============== ============== ========= ====================== =============
Sample rate (kHz) Frames/Period  Periods/Buffer Loop type Roundtrip latency (ms) Suggested I/O
================= ============== ============== ========= ====================== =============
44.1              1024           2              Direct                           ~1100 inc
44.1              1323 (44.1*30) 2              Direct                           ~1000 inc
48                1024           2              Direct    101.5                  ~897
48                1024           2              Mic       122.4                  ~895
48                1024           3              Direct    122.8                  ~899
48                1024           3              Mic       122.4                  ~889
48                1056 (48*22)   2              Direct    104.4                  ~920
================= ============== ============== ========= ====================== =============


</t>
<t tx="rick.20130921144244.1472">When measuring using 44.1kHz, the suggested value for latency I/O kept
increasing. I asked about this on IRC::

    &lt;ricli85&gt; Some reports on the increasing number that jack_iodelay gives me:
    If I set 48kHz sample rate and frames/period=1056 (multiple of 48), I seem
    to get a more stable number.
    &lt;las&gt; ricli85: many USB devices work correctly *only* at 48kHz

Conclusion: Always use 48kHz sample rate for your USB audio interface.</t>
<t tx="rick.20130921144244.1473">1. **Start JACK with the given settings**

   From QjackCtl, click "Setup...":
   
   .. image:: jack_setup.png
   
   Adjust frames/period, sample rate, and periods/buffer. Make sure latency I/O
   are both set to 0 (default).
   
   Click "OK" to close the dialog, then click "Start".

2. **Start jack_iodelay**

   Run the following command in the terminal::
   
       jack_iodelay
       
3. **Connect**

   From QjackCtl, click "Connect":
   
   .. image:: jack_connect.png
   
4. **Observe output**

   We should see something like this::
   
       4921.269 frames    102.526 ms total roundtrip latency
         extra loopback latency: 1849 frames
         use 924 for the backend arguments -I and -O

   Note that the suggested number (924) is only valid for the configuration that
   we entered in step 1.
   
   To compensate for the roundtrip latency, we should enter this number for both
   the input and the output latency in the settings dialog in step 1.</t>
</tnodes>
</leo_file>
