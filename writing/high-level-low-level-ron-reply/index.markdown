---
title: High-level or micro-tests? A discussion with Ron.
date: 2023-06-18
---

Ron and I have been discussing on Mastodon high-level vs micro-tests. Ron
has summarized the discussion
[here](https://ronjeffries.com/articles/-y023/python/-m130/139/) and also
expanded his thoughts on it. In this blog post, I want to expand my thoughts as
well.

# How it started

In [Python 136](https://ronjeffries.com/articles/-y023/python/-m130/136/), Ron
discusses a possible drawback with the decentralized design:

> Now, many of my objects interact with missiles, the ship and saucer and
> asteroids at least, perhaps others. And so when I implemented the
> SaucerMissile, saucer missiles stopped interacting via
> `interact_with_missile` and started calling `interact_with_saucermissile`.
>
> And that method is explicitly ignored unless the object implements it, i.e.
> unless the programmer, i.e. yours truly, remembers to implement the new
> method where it’s needed.
>
> And I didn’t remember them all. I spent most of my article yesterday
> discovering another case that I had forgotten, and I was relying too much on
> my tests. They did help me at first but after a while, things were broken but
> no tests existed to show the problem.

I interpret that (hopefully correctly) to mean something like this:

1. There are tests that call `asteroid.interact_with_missile` to verify
   behavior for asteroids colliding with missiles.

2. When the new missile subclass was added `interact_with_missile` was actually
   never called for real, but `interact_with_saucermissile` was called instead.

3. The tests that called `asteroids.interact_with_missile` continued to pass,
   but they tested something that never happened in reality.

4. All tests passed. The game was broken.

To this, I [replied](https://hachyderm.io/@rickardlindberg/110549123416204580):

> What about writing domain/"game rule" tests at a higher level? That is, you
> put a bunch of flyers in the collection, call update (or whatever it was
> called), have the collection do all the interact_with_, and assert something
> on the collection.
>
> Then you could more easily trust your tests and you are free to implement it
> however you want (implementation inheritance, events, etc).
>
> I think such tests might even read quite well as descriptions of the game
> rules.

Ron [replied](https://mastodon.social/@RonJeffries/110549805294230736):

> a good idea ... with a centralized test there's at least a place to look.
> Unfortunately such tests have to involve a lot of time stuff due to all the
> timers.

And so the discussion about high-level tests vs micro-tests began.

# Ron's arguments for micro-tests

Ron seems in favour of writing more micro-tests and fewer high-level tests. He
argues that

> Certainly it must be true that if every object does the right thing at the
> right time, the program must work. This is the root reason why micro-testing
> works.

I agree with this, and at the same time, I sometimes find it difficult to
convince myself that every object does the right thing at the right time. Let
me try to explain why.

# Mocking

Mocking and micro-testing have something in common. (At least if you squint.)
And that is that they are solitary-ish tests that focus on a very specific,
isolated part of the code.

I will illustrate what I think is problematic with that with a calculator
example that show the total on a display:

```python
class Calculator:

    def __init__(self, display):
        self.display = display
        self.total = 0

    def add(self, amount):
        self.total += amount
        self.display.show(f"Total = {self.total}")

class Display:

    def show(self, text):
        # gui stuff to show something on the screen
```

To test this, we might write one solitary test per class:

```python
def test_calculator():
    mock_display = Mock(Display)
    calculator = Calculator(mock_display)
    calculator.add(3)
    calculator.add(2)
    assert mock_display.show.calls = [
        "Total = 3",
        "Total = 5",
    ]

def test_display():
    display = Display()
    display.show("hello")
    # assert that gui shows with text 'hello'
```

These two tests prove that the whole works. But I find them a bit fragile.
Especially in a dynamically typed language like Python.

What if we changed the signature of `show` to something like this:

```python
def show(self, text, color):
    ...
```

`test_display` would break, but what about `test_calculator`? At least in
Python, I don't think the type checking is able to catch that the signature has
changed. So `test_calculator` will continue to pass. But the application will
not work.

The same thing is true for micro-testing. Your micro-tests can continue to pass,
but the methods they are calling are actually not called for real.

This aspect of mocking and micro-testing makes me less confident that my code
actually works.

# Move example

Ron continues to discuss asteroid movement:

> ... but I would argue that we don’t need a larger scale test to verify how
> they move.

I don't think so either. Movement is completely contained within an asteroid.
It does not depend on interactions with other flyers. We could test it by
instantiating an asteroid, calling its update method, and assert that it moved.

# Missile vs asteroid example

Ron continues to discuss another example where asteroids collide with missiles.
He wish he had a test for scoring in that situation and writes this test:

```python
def test_missile_vs_asteroid_scoring(self):
    fleets = Fleets()
    fi = FI(fleets)
    pos = Vector2(100, 100)
    vel = Vector2(0, 0)
    asteroid = Asteroid(2, pos)
    missile = Missile.from_ship(pos, vel)
    asteroid.interact_with_missile(missile, fleets)
    scores = fi.scores
    assert scores[0].score == u.MISSILE_SCORE_LIST[asteroid.size]
    asteroid.size = 1
    asteroid.interact_with_missile(missile, fleets)
    scores = fi.scores
    assert scores[1].score == u.MISSILE_SCORE_LIST[asteroid.size]
    asteroid.size = 0
    asteroid.interact_with_missile(missile, fleets)
    scores = fi.scores
    assert scores[2].score == u.MISSILE_SCORE_LIST[asteroid.size]
```

And notes that

> Now, since Rickard specifically mentions using interact_with in the tests
> he’s talking about, perhaps this is an example of just what he means. If
> that’s the case, we probably don’t have any real disagreement at all. That
> would be nice.

I'm not sure I explained myself clearly enough on Mastodon. I will do another
attempt here.

I would not call `interact_with_*` in tests. I would write the test above like
this:

```python
def test_missile_vs_asteroid_scoring_rickard(self):
    for asteroid_size in [2, 1, 0]:
        position = Vector2(100, 100)
        fleets = Fleets()
        fleets.append(Asteroid(
            size=asteroid_size,
            position=position
        ))
        fleets.append(Missile.from_ship(
            position=position,
            velocity=Vector2(0, 0)
        ))
        fleets.perform_interactions()
        (score,) = FI(fleets).scores
        assert score.score == u.MISSILE_SCORE_LIST[asteroid_size]
```

To be able to test scoring, we have to include both an asteroid and a missile
in the test. To also make sure that they interact correctly, I would write the
test by putting them in `Fleets` and have fleets `perform_interactions`.

> At this moment, I would ask Rickard whether he agrees that we can be sure
> that ship missile vs asteroid collisions score correctly. And if he said we
> cannot be sure, I’d try to devise another low-level test to help him be sure.

With my version of the test above, I would be certain that scoring works. With
Ron's version, I would worry about the test calling the wrong
`interact_with_*`. I would not worry that much, just a little. But I don't
think my version of the test has any drawbacks compared to Ron's version. And
it has the upside of verifying that interactions work properly.

I believe that the weakness of this implementation of a decentralized design is
that it is difficult to know if you implemented the correct `interact_with_*`
methods.

In Ron's test, `interact_with_missile` is called explicitly. So it will not
catch errors if the method should have been `interact_with_saucermissile` for
example.

My version of the test is at a higher level. It puts an asteroid and a missile
in the fleets and have them interact and assert that scoring happens correctly.
Had we implemented the wrong `interact_with_*`, this test would have failed.

If I were to take a more micro-test approach to this, I would write multiple
tests. First this one:

```python
def test_missile_vs_asteroid_scoring_rickard_micro_high(self):
    position = Vector2(100, 100)
    fleets = Fleets()
    fleets.append(Asteroid(
        size=2,
        position=position
    ))
    fleets.append(Missile.from_ship(
        position=position,
        velocity=Vector2(0, 0)
    ))
    fleets.perform_interactions()
    (score,) = FI(fleets).scores
    assert score.score == u.MISSILE_SCORE_LIST[2]
```

This test ensures that the interactions work with regards to scoring. But
it is only testing one score.

With that test in place, I feel pretty certain that `score_for_hitting` is
called. I can read the code and see that the only way the score would be
correct in the first test is if `score_for_hitting` is called.

To test that we get correct scores back, I therefore don't think we need to
test that via interactions, we can test it with a more lower-level,
micro-test:

```python
def test_missile_vs_asteroid_scoring_rickard_micro_low(self):
    for asteroid_size in [2, 1, 0]:
        position = Vector2(100, 100)
        assert Asteroid(
            size=asteroid_size,
            position=position
        ).score_for_hitting(
            Missile.from_ship(
                position=position,
                velocity=Vector2(0, 0)
            )
        ) == u.MISSILE_SCORE_LIST[asteroid_size]
```

This test has lots of irrelevant details though. The positions and
velocity don't matter, so it's annoying to have to specify them. It makes the
test harder to read. I would think about how I could refactor the code so that
the test could read something like this:

```python
def test_missile_vs_asteroid_scoring_rickard_micro_low(self):
    for asteroid_size in [2, 1, 0]:
        assert Asteroid(size=asteroid_size).score_for_hitting(Missile.from_ship()) \
            == u.MISSILE_SCORE_LIST[asteroid_size]
```

This is overlapping, sociable testing. An approach I've started appreciating
more and more over the last year or so.

The score for size 2 is tested in both tests. This might feel wrong to some
people. It certainly did to me in the beginning. But what it gives me (and
perhaps you as well) is greater confidence that stuff actually works. And that
trade-off is something that I've started to appreciate.

# Did this bring clarity?

Ron writes:

> I should say right off the bat that you’ve seen everything that Rickard and I
> have shared on the subject, so there’s no reason to be certain that we’re
> even talking about the same kind of tests. I’m thinking that he means a big
> story test with all the objects in it and a long series of time ticks and
> checks on the game state throughout. He might be thinking of tests like the
> one I just wrote, or even simpler ones, with just a few objects and a few
> interactions, but more than one test. We may not disagree at all on that.

I was thinking of a test like `test_missile_vs_asteroid_scoring_rickard`. You
only put the objects that are needed for a particular interaction into fleets
and have them interact.

I hope I managed to explain my thinking a bit more in this blog post. If it
sounds like I say "you should do this because it is better" I apologize. My
intention was to explain my current thinking and what I would do if I did it
now. And as Ron pointed out, if we actually did this together, the end result
would probably be something different.

# So high-level or micro?

> And if we got the code expressing each micro rule very clearly … would we
> still wonder about the bigger picture?

I think the game rules will be quite clear with micro-tests only.

My concern with this implementation is mainly that it is difficult to write
those correctly. (You have to implement and call the correct
`interact_with_*`.) I think that my suggestion to write a higher-lever tests
for interactions partly solves that problem.

If we can come up with a better scheme for this mechanism in Python, I might be
less inclined to write high-level test.

But currently, I think I still like higher-level tests. At least some. I like
overlapping, sociable tests. That means tests at many different levels. And if
testing at a higher level is not inconvenient (it quickly becomes), I *think* I
prefer that. First of all because it gives me greater confidence that the code
works, and second because it allows refactoring without changing tests.
