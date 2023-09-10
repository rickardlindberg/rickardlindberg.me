---
title: 'DevLog 013: Raspberry Pi game console'
date: 2023-09-10
tags: devlog,agdpp
devlog: true
---

It is time to revisit the [balloon shooter](/projects/agdpp/index.html). I'm
interested in building a "game console PC" so that my son can more easily play
the balloon shooter and other games. Until now we have played all games on my
laptop.

This will involve two main steps I think. The first is to get a Raspberry Pi
and install all games on it. The second involves auto starting a custom
application that can be used to select which game to play by using the gamepad.
Ideally, you should not need to use a mouse or a keyboard. My plan for this
custom application is to build it using the framework that we have in the
balloon shooter.

Let's get started.

## The Raspberry Pi

At first, I'm not sure what hardware to get for this game console PC. I look
around a bit, and then eventually settle on a Raspberry Pi starter kit.

<p>
<center>
![Raspberry Pi starter kit.](pibox.png)
</center>
</p>

I am bit concerned that it will not be powerful enough to play games. But it is
relatively cheap, and if it can't play all games, perhaps my son (or me) can
have some fun with it in another way.

## Assembly

The starter kit comes with everything you need to get started. That's also one
reason that I went with it. I'm not that interested in selecting hardware. I'm
more interested in quickly prototyping this game console PC. If it turns out
the Pi is not powerful enough, but the game console PC concept is a hit, we can
look for better hardware. However, if the game console PC is not a hit, we have
not wasted that much time or money.

And look. Apparently Raspberry Pis need heat sinks and fans nowadays. When I
last played with a Pi, many, many years ago, I don't remember that being the
case. Let's hope that means that they are more powerful now.

<p>
<center>
![Assembling the starter kit.](assembly.png)
</center>
</p>

I assemble the kit in about 15 minutes. Then I boot it up and install the
operating system that comes preconfigured. I let it do its thing, and come
back once it is installed.

## Setup

I want to install [SuperTux](https://www.supertux.org/) and the balloon
shooter on the Pi.

It seems like the version of SuperTux is older than what I have on my laptop.
And my laptop is old.  Furthermore, Python 2 seems to be the default Python. I
learn that when trying to install all requirements for the balloon shooter. I
also have to install a newer version of Pygame and for that I need to install
some SDL build dependencies.  Perhaps getting a newer operating system would be
nice.

Eventually, I get everything working:

<p>
<center>
![Setting up games.](setup.png)
</center>
</p>

The versions might be a little old. The performance might be so so. But we have
something setup that we can experiment with.

## A note on performance

Me and my son try to play SuperTux on the setup. It feels a little different.
Part of it might be that it is slightly different version of the game. Part of
it might be that the Pi has worse performance. We try to run the game at a
lower resolution, and it seems to help a bit. We can probably try different
things to get better performance, but this is absolutely fine for now. My son
is still having fun playing.

## Autostart

To start SuperTux on the Pi you first have to start the Pi and then you have to
select SuperTux from the menu with the mouse. The balloon shooter is even more
complicated to start. First you need to open a terminal and then run a
command.

I don't think that is good enough for a game console PC. I want to be able to
operate it using the gamepad only.

The first tiny step in that direction is to configure SuperTux as the startup
application. If we can do that, then SuperTux can be started and played without
using the keyboard or mouse.

Once we have that working, we can work on our own startup application that
let us select the game, and then we can start that one instead.

I search the internet for how to configure a startup application for the Pi.

I find an article that says that you can put a file in the autostart
directory. I try this:

$:output:text:
$ cat /etc/xdg/autostart/game_console_start.desktop
[Desktop Entry]
Name=Game console start
Exec=supertux2
$:END

I restart the Pi, and SuperTux actually starts automatically and you can start
playing it using the gamepad. Fantastic!

## Startup application idea

Let's move on to our custom startup application. Here is the idea that I have
for it:

$:output:python:
while True:
    subprocess.call(StartupApplication.create().run())
$:END

This code runs the startup application in a loop. Its `run` method should
return the command to run. (The game to play or shutdown command.)

I think we can test drive the `StartupApplication` and then we can hook it up
in the loop above.

Perhaps we should even test drive the loop.

We'll see.

## Test driving the application

I start with this in a new file:

$:output:python:
class StartupApplication:

    """
    I draw an application select screen:

    >>> events = StartupApplication.run_in_test_mode(
    ...     events=[
    ...         [GameLoop.create_event_user_closed_window()],
    ...     ]
    ... )
    """
$:END

I create the bare minimum that the test complains about and get this:

$:output:python:
class StartupApplication:

    ...

    @staticmethod
    def run_in_test_mode(events=[]):
        loop = GameLoop.create_null(
            events=events+[
                [GameLoop.create_event_user_closed_window()],
            ]
        )
        events = loop.track_events()
        StartupApplication(loop).run()
        return events

    def __init__(self, loop):
        self.loop = loop

    def run(self):
        self.loop.run(self)

    def event(self, event):
        pass

    def tick(self, dt):
        pass
$:END

Now it doesn't complain, but it seems to hang in an infinite loop.

I modify `event` to this:

$:output:python:
class StartupApplication:

    ...

    def event(self, event):
        if event.is_user_closed_window():
            raise ExitGameLoop()
$:END

And we're green. Let's commit.

$:output:text:
$ git commit -a -m 'Emryo to new startup application.'
[main a55d17e] Emryo to new startup application.
 2 files changed, 39 insertions(+)
 create mode 100644 startup.py
$:END

The test is not yet fleshed out. It doesn't test what it says it tests. But it
drove out the skeleton of the application.

## Reflecting on the design

It's been a while since I worked on the balloon shooter. What do I think when I
work in this design again?

I got stuck in an infinite loop. That happens because we have a `while True:`
in our game loop somewhere. I've always found testing infinite loops difficult.
That's one reason why I hesitated testing the loop for the startup application.
But now I get another idea. What if we create the loop like this instead?

$:output:python:
while self.loop_condition.active():
    ...
$:END

Then we can create different versions of the loop condition maybe something
like this:

$:output:python:
class InfiniteLoopCondition:

    def active(self):
        return True
$:END

$:output:python:
class TestLoopCondition:

    def __init__(self, iterations):
        self.counter = 0
        self.iterations = iterations

    def active(self):
        flag = self.counter >= self.iterations
        self.iterations += 1
        return flag
$:END

Let's see if we can try this out in the startup application. If it works out
well, perhaps we can port it to the game loop as well?

## A mistake

The test that we wrote does not assert anything on the events. Let's fix that.
I comment out the assignment of `events` and paste the expected test output:

$:output:python:
"""
I draw an application select screen:

>>> StartupApplication.run_in_test_mode(
...     events=[
...         [GameLoop.create_event_user_closed_window()],
...     ]
... )
GAMELOOP_INIT =>
    resolution: (1280, 720)
    fps: 60
GAMELOOP_QUIT =>
"""
$:END

## The looping concept

This startup application should run in an infinite loop. In each iteration it
should init the game loop and show the game selection screen. Once the
selection has been made, it should quit the game loop and run the command. Then
it starts all over.

Let's try the looping thing.

I start by TDDing the loop conditions:

$:output:python:
class InifiteLoopCondition:

    def active(self):
        """
        >>> InifiteLoopCondition().active()
        True
        """
$:END

That fails. Fix by return true. The other:

$:output:python:
class FiteLoopCondition:

    def __init__(self, iterations):
        self.iterations = iterations
        self.count = 0

    def active(self):
        """
        >>> condition = FiteLoopCondition(iterations=2)
        >>> condition.active()
        True
        >>> condition.active()
        True
        >>> condition.active()
        False
        """
        flag = self.count < self.iterations
        self.count += 1
        return flag
$:END

I actually got the condition wrong here at first. I'm glad I wrote a test for
it. The previous example, `TestLoopCondition`, above is actually wrong. Even
for really simple code like this, having tests is nice.

Let's see if we can use a loop condition and have the test show us that two
loops are actually made.

I change

$:output:python:
class StartupApplication:

    ...

    @staticmethod
    def run_in_test_mode(events=[]):
        ...
        StartupApplication(loop).run()
        ...
$:END

to

$:output:python:
class StartupApplication:

    ...

    @staticmethod
    def run_in_test_mode(events=[]):
        ...
        StartupApplication(
            loop=loop,
            loop_condition=FiteLoopCondition(2)
        ).run()
        ...
$:END

I also notice that i misspelled finite. I fix that and then add the parameter
to the class. Test passes. Let's add an actual loop:

$:output:python:
class StartupApplication:

    ...

    def run(self):
        while self.loop_condition.active():
            self.loop.run(self)
$:END

This, expectedly, output another loop which I add to the assertion. Perfect!

$:output:text:
    GAMELOOP_INIT =>
        resolution: (1280, 720)
        fps: 60
    GAMELOOP_QUIT =>
$:END

We are not yet using the `InfiniteLoopCondition`. Let's change that by adding a
`create` method:

$:output:python:
class StartupApplication:

    ...

    @staticmethod
    def create():
        """
        >>> isinstance(StartupApplication.create(), StartupApplication)
        True
        """
        return StartupApplication(
            loop=GameLoop.create(),
            loop_condition=InifiteLoopCondition()
        )
$:END

I also add this:

$:output:python:
if __name__ == "__main__":
    StartupApplication.create().run()
$:END

And when I run

$:output:text:
$ python startup.py
$:END

It indeed creates a new window every time I close it.

$:output:text:
$ git commit -a -m 'Add startup entry point and have it loop.'
[main aadd1a2] Add startup entry point and have it loop.
 1 file changed, 60 insertions(+), 5 deletions(-)
$:END

## Selecting a game

What is the simplest possible solution for selecting a game?

I imagine that the display shows an icon for each game that can be selected.
Then you move a cursor over it and press a key to select it.

I start by getting some games on the screen:

$:output:python:
def tick(self, dt):
    self.loop.clear_screen()
    self.loop.draw_text(Point(x=100, y=100), text="SuperTux")
    self.loop.draw_text(Point(x=100, y=200), text="Balloon Shooter")
$:END

It looks like this:

<p>
<center>
![Games in startup screen.](games.png)
</center>
</p>

I think we also need a cursor:

$:output:python:
def tick(self, dt):
    self.loop.clear_screen()
    self.loop.draw_text(Point(x=100, y=100), text="SuperTux")
    self.loop.draw_text(Point(x=100, y=200), text="Balloon Shooter")
    self.loop.draw_circle(Point(x=500, y=500), radius=20, color="pink")
$:END

It looks like this:

<p>
<center>
![Cursor in startup screen.](cursor.png)
</center>
</p>

Now I think two things are missing. The first is that at the press of a button,
the game closest to the cursor should start. The second is that you also need
to be able to move the cursor.

I think working on movement is secondary. It is more important to be able to
start **one** game instead of nothing. So let's work on that first.

## Starting a game

I want to write a test for the new behavior, but I find that testing at the top
level is tedious and error prone. I would therefore like to start by
refactoring and extracting a `StartupScene` maybe that has an interface that is
easier to test. I end up with this:

$:output:python:
class StartupScene:

    def event(self, event):
        if event.is_user_closed_window():
            raise ExitGameLoop()

    def draw(self, loop):
        loop.draw_text(Point(x=100, y=100), text="SuperTux")
        loop.draw_text(Point(x=100, y=200), text="Balloon Shooter")
        loop.draw_circle(Point(x=500, y=500), radius=20, color="pink")
$:END

I'm sure this refactoring works because I have tests to cover it.

Commit!

Now, let's see if we can write a test:

$:output:python:
"""
When XBOX_A is pressed, I start the game that is closest to the cursor:

>>> scene = StartupScene()
>>> scene.event(GameLoop.create_event_joystick_down(XBOX_A))
SuperTux
"""
$:END

I make it pass like this:

$:output:python:
class StartupScene:

    ...

    def event(self, event):
        ...
        elif event.is_joystick_down(XBOX_A):
            print("SuperTux")
$:END

This is obviously faking it. It is not supposed to print the name of the game,
it is supposed to run it, or, wait a minute. This class is not supposed to run
it, the top-level class is.

Let's scratch this and start over.

## Starting a game (again)

Let's have a look at the top-level test:

$:output:python:
"""
I draw an application select screen:

>>> StartupApplication.run_in_test_mode(
...     events=[
...         [],
...         [GameLoop.create_event_user_closed_window()],
...         [],
...         [GameLoop.create_event_user_closed_window()],
...     ]
... )
GAMELOOP_INIT =>
    resolution: (1280, 720)
    fps: 60
CLEAR_SCREEN =>
DRAW_TEXT =>
    x: 100
    y: 100
    text: 'SuperTux'
DRAW_TEXT =>
    x: 100
    y: 200
    text: 'Balloon Shooter'
DRAW_CIRCLE =>
    x: 500
    y: 500
    radius: 20
    color: 'pink'
GAMELOOP_QUIT =>
GAMELOOP_INIT =>
    resolution: (1280, 720)
    fps: 60
CLEAR_SCREEN =>
DRAW_TEXT =>
    x: 100
    y: 100
    text: 'SuperTux'
DRAW_TEXT =>
    x: 100
    y: 200
    text: 'Balloon Shooter'
DRAW_CIRCLE =>
    x: 500
    y: 500
    radius: 20
    color: 'pink'
GAMELOOP_QUIT =>
"""
$:END

This shows our game loop runs twice, but there is no mention that a command is
run. Let's modify

$:output:python:
class StartupApplication:

    ...

    def run(self):
        while self.loop_condition.active():
            self.loop.run(self)
$:END

to

$:output:python:
class StartupApplication:

    ...

    def run(self):
        while self.loop_condition.active():
            self.loop.run(self)
            print(f"TODO: run {self.startup_scene.get_command()}")
$:END

It complains that `get_command` does not exist. Let's add it:

$:output:python:
class StartupScene:

    def get_command(self):
        return ["supertux2"]

    ...
$:END

We are now getting a somewhat expected test failure:

$:output:text:
Differences (ndiff with -expected +actual):
    + TODO: run ['supertux2']
    + TODO: run ['supertux2']
      GAMELOOP_INIT =>
          resolution: (1280, 720)
          fps: 60
      CLEAR_SCREEN =>
$:END

I was thinking to fake this and postpone running the actual command. To do it
properly we need an infrastructure wrapper for running commands. I'll just do
it.

Here is a first faked version:

$:output:python:
class Command(Observable):

    @staticmethod
    def create():
        return Command()

    @staticmethod
    def create_null():
        return Command()

    def run(self, command):
        self.notify("COMMAND", {"command": command})
$:END

Instead of printing the command, it sends a notification so that we can assert
that the event happens at the right time in the test. That is, we can assert
that a command is run after the game loop is quit:

$:output:text:
...
GAMELOOP_QUIT =>
COMMAND =>
    command: ['supertux2']
...
$:END

This works. Let's commit:

$:output:text:
$ git commit -a -m 'Run command from StartupScene when game loop is quit.'
[main 4c47b18] Run command from StartupScene when game loop is quit.
 1 file changed, 31 insertions(+), 5 deletions(-)
$:END

For this to actually do something, we need to flesh out `Command`. Here is what
I end up with:

$:output:python:
class Command(Observable):

    """
    >>> Command.create().run(["echo", "hello"])

    >>> Command.create().run(["command-that-does-not-exist"])
    Traceback (most recent call last):
      ...
    FileNotFoundError: [Errno 2] No such file or directory: 'command-that-does-not-exist'

    >>> Command.create_null().run(["command-that-does-not-exist"])
    """

    @staticmethod
    def create():
        return Command(subprocess=subprocess)

    @staticmethod
    def create_null():
        class NullSubprocess:
            def run(self, command):
                pass
        return Command(subprocess=NullSubprocess())

    def __init__(self, subprocess):
        Observable.__init__(self)
        self.subprocess = subprocess

    def run(self, command):
        self.notify("COMMAND", {"command": command})
        self.subprocess.run(command)
$:END

When the startup application is run and then quit, SuperTux is actually
started.

This is actually some real progress.

$:output:text:
$ git commit -a -m 'Command actually runs commands.'
[main 270440e] Command actually runs commands.
 1 file changed, 23 insertions(+), 2 deletions(-)
$:END

## Selection behavior

Let's review the `StartupScene`:

$:output:python:
class StartupScene:

    def get_command(self):
        return ["supertux2"]

    def event(self, event):
        if event.is_user_closed_window():
            raise ExitGameLoop()

    def draw(self, loop):
        loop.draw_text(Point(x=100, y=100), text="SuperTux")
        loop.draw_text(Point(x=100, y=200), text="Balloon Shooter")
        loop.draw_circle(Point(x=500, y=500), radius=20, color="pink")
$:END

We have higher-level tests in place that checks that whatever `get_command`
returns is run when the game loop quits.

I think it should now be fairly easy to write tests for selection behavior.
Let's first modify the event handler to also exit the game loop when `XBOX_A`
is pressed:

$:output:python:
class StartupScene:

    ...

    def event(self, event):
        """
        >>> StartupScene().event(GameLoop.create_event_user_closed_window())
        Traceback (most recent call last):
          ...
        gameloop.ExitGameLoop

        >>> StartupScene().event(GameLoop.create_event_joystick_down(XBOX_A))
        Traceback (most recent call last):
          ...
        gameloop.ExitGameLoop
        """
        if event.is_user_closed_window() or event.is_joystick_down(XBOX_A):
            raise ExitGameLoop()
$:END

Now let's think about what `get_command` should return. It should return the
command of the game that is closest to the cursor. Let's write two tests for
that:

$:output:python:
class StartupScene:

    ...

    def get_command(self):
        """
        >>> scene = StartupScene()

        >>> scene.move_cursor(x=100, y=100)
        >>> scene.get_command()
        ['supertux2']

        >>> scene.move_cursor(x=100, y=200)
        >>> scene.get_command()
        ['python', '/home/.../agdpp/agdpp.py']
        """
$:END

It complains that `move_cursor` does not exist. I add it like this:

$:output:python:
class StartupScene:

    def __init__(self):
        self.cursor = Point(x=500, y=500)

    def move_cursor(self, x, y):
        self.cursor = Point(x=x, y=y)

    ...
$:END

I also modify the drawing code to use this point for the cursor.

Now the second test case fails:

$:output:text:
Failed example:
    scene.get_command()
Differences (ndiff with -expected +actual):
    - ['python', '/home/.../agdpp/agdpp.py']
    + ['supertux2']
$:END

I make a quick and dirty fix, because I want to go quickly to green so that I
can refactor and generalize the solution:

$:output:python:
    def get_command(self):
        if self.cursor.y == 200:
            return ["python", "/home/.../agdpp/agdpp.py"]
        return ["supertux2"]
$:END

And this is my favorite state of programming. This is actually where some
design happens. I have the safety net of the tests and I can push code around
until I think it looks good and the next thing is easy to add.

Here is what I come up with this time:

$:output:python:
class StartupScene:

    def __init__(self):
        self.cursor = Point(x=500, y=500)
        self.games = [
            Game(
                name="SuperTux",
                position=Point(x=100, y=100),
                command=["supertux2"],
            ),
            Game(
                name="Balloon Shooter",
                position=Point(x=100, y=200),
                command=["python", "/home/.../agdpp/agdpp.py"],
            ),
        ]

    def get_command(self):
        return min(
            self.games,
            key=lambda game: game.distance_to(self.cursor)
        ).command

    def draw(self, loop):
        for game in self.games:
            game.draw(loop)
        loop.draw_circle(self.cursor, radius=20, color="pink")

    ...
$:END

And here is the `Game` class:

$:output:python:
class Game:

    def __init__(self, name, position, command):
        self.name = name
        self.position = position
        self.command = command

    def draw(self, loop):
        loop.draw_text(self.position, text=self.name)

    def distance_to(self, point):
        return self.position.distance_to(point)
$:END

This implementation still passes all tests and is also generalized. Nice!

$:output:text:
$ git commit -a -m 'Run the command closest to the cursor.'
[main 921c71f] Run the command closest to the cursor.
 1 file changed, 64 insertions(+), 7 deletions(-)
$:END

## Cursor movement

Next I want to work on cursor movement so that we can actually select
different games.

I'm not quite sure how to write a low-level test for this in `GameScene`, so I
write a top-level test instead:

$:output:python:
"""
>>> StartupApplication.run_in_test_mode(
...     events=[
...         [],
...         [GameLoop.create_event_joystick_motion(axis=1, value=1.0)],
...         [GameLoop.create_event_user_closed_window()],
...     ],
...     iterations=1
... ).filter("DRAW_CIRCLE")
DRAW_CIRCLE =>
    x: 500
    y: 500
    radius: 20
    color: 'pink'
DRAW_CIRCLE =>
    x: 500
    y: 501
    radius: 20
    color: 'pink'
"""
$:END

We assert that the cursor is drawn in two different positions given a joystick
motion event.

The gist of the implementation is here:

$:output:python:
class StartupScene:

    ...

    def event(self, event):
        ...
        elif event.is_joystick_motion():
            if event.get_axis() == 0:
                self.dx = event.get_value()
            elif event.get_axis() == 1:
                self.dy = event.get_value()

    def update(self, dt):
        delta = Point(x=self.dx, y=self.dy)
        if delta.length() > 0.05:
            self.cursor = self.cursor.add(delta.times(dt))
$:END

The `update` method did not exist on `StartupScene` before. The pattern how
it is called is here:

$:output:python:
class StartupApplication:

    ...

    def event(self, event):
        self.startup_scene.event(event)

    def tick(self, dt):
        self.loop.clear_screen()
        self.startup_scene.update(dt)
        self.startup_scene.draw(self.loop)
$:END

So the scene will receive these calls in order:

* `event`
* `update`
* `draw`

This represents one game loop cycle. If this pattern becomes more permanent, we
can move the top-level test down to `StartupApplication` and have that test
call `event` + `update` and assert that the cursor moved. But for now, I want
the confidence that the high-level test gives, that everything is actually
working together.

I also test this in game to fist of all make sure that I got the axis right and
also to tweak numbers so that speed feels good. The length check is needed
because joystick movement events rarely return a value of 0. If we only move
the joystick a tiny bit, we don't want the cursor to move.

Also, we should probably add constant names for the axis to not compare to
numbers. Maybe `XBOX_AXIS_Y` for example.

Anyway, when I try this out, it actually works. I can move the cursor around,
and when I press `XBOX_A` the game closest to the cursor is started.

## Finishing touches

I want to visualize the game that is closest to the cursor. Let's do it with
another color.

$:output:python:
class StartupScene:

    ...

    def draw(self, loop):
        for game in self.games:
            game.draw(loop, self.game_closest_to_cursor())

    def game_closest_to_cursor(self):
        return min(
            self.games,
            key=lambda game: game.distance_to(self.cursor)
        )
$:END

$:output:python:
class Game:

    ...

    def draw(self, loop, closest):
        loop.draw_text(
            self.position,
            text=self.name,
            color="lightblue" if closest is self else "black"
        )
$:END

I modify tests to assert the correct color. This works perfectly.

Next I want to fix the games that are configured. I want them to display
evenly on the screen, and I want to have a "QUIT" game that runs a shutdown
command to shut down the Pi.

Here it is:

$:output:python:
class StartupScene:

    def __init__(self):
        self.cursor = Point(x=400, y=300)
        self.games = [
            Game(
                name="SuperTux",
                position=Point(x=100, y=100),
                command=["supertux2"],
            ),
            Game(
                name="Balloon Shooter",
                position=Point(x=400, y=300),
                command=["python3", "agdpp.py"],
            ),
            Game(
                name="QUIT",
                position=Point(x=1000, y=600),
                command=["shutdown", "now"],
            ),
        ]
$:END

And it looks like this:

<p>
<center>
![Final startup screen.](final.png)
</center>
</p>

## Trying on the Pi

I change the startup script, `/etc/xdg/autostart/game_console_start.desktop`,
to this:

$:output:text:
[Desktop Entry]
Name=Game console start
Exec=/home/pi/game_console_pc.sh
$:END

Where `/home/pi/game_console_pc.sh` is this:

$:output:shell:
#!/usr/bin/env bash

exec > /home/pi/game_console_pc.log

exec 2>&1

cd /home/pi/agdpp

for retry in 1 2 5 10 giveup; do
	if [ $retry = giveup ]; then
		echo giving up
		break
	elif git pull --ff-only; then
		break
	else
		echo Retrying in $retry
		sleep $retry
	fi
done

python3 startup.py
$:END

And it works beautifully.

Why did I not test drive this startup script? Good question. I for sure spend
some time debugging the loop, which, by the way, is needed to give the Pi time
to connect to the wireless network before it can download the latest version of
the startup application and balloon shooter.

$:output:text:
pi@raspberrypi:~ $ cat game_console_pc.log
fatal: unable to access 'https://github.com/rickardlindberg/agdpp.git/': Could not resolve host: github.com
Retrying in 1
fatal: unable to access 'https://github.com/rickardlindberg/agdpp.git/': Could not resolve host: github.com
Retrying in 2
fatal: unable to access 'https://github.com/rickardlindberg/agdpp.git/': Could not resolve host: github.com
Retrying in 5
Already up to date.
$:END

I feel like this script is maybe not part of the game itself. So that is one
reason why I just "hacked" it together on the Pi. But I'm not entirely happy
that it exists only there, and not in some repo, and doesn't have any tests.

However, for now, it works fine, but there is another problem. It is not
possible to quit the balloon shooter with the gamepad. So once you start it,
you are stuck in it.

## Add balloon shooter quit

I modify `GameScene` by adding a check for `XBOX_START`:

$:output:python:
class GameScene:

    ...

    def event(self, event):
        if event.is_user_closed_window() or event.is_joystick_down(XBOX_START):
            raise ExitGameLoop()
        ...
$:END

And by printing events, I figure out the value of `XBOX_START`:

$:output:python:
XBOX_START = 7
$:END

## Summary

Finally, I have the first version of the setup that I had in mind.

I find it a little difficult to document all my thinking in this DevLog format.
I feel like I make hundreds of decisions every minute when programming, and
writing about all of them seems impossible. I think one solution would be to
cover smaller changes in each DevLog. Your questions and commends are very
welcome.

Even if these DevLogs are not valuable to anyone else, they are valuable to me
because I get to practice writing and explaining my thinking.

See you next time!
