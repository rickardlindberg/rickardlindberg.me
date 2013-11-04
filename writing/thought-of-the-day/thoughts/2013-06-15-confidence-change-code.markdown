Today's thought is about what makes me confident in changing code.

Sometimes I am afraid when I change a piece of code. Other times I am not. What
is the difference?

Examples of when I am afraid to change code:

* When it is a critical part of the system
* When I am writing C
* When I am working on other's projects

Examples of when I am not so afraid:

* When it is not a critical part of the system
* When I am writing Haskell
* When I am working on my own projects

Let me examine them each.

The first point can be generalized to changing a part of a system that possibly
has a large effect on the rest of the system. That includes both critical parts
and parts that much code depend on. If the critical part is isolated from the
rest of the system, I find it easier to change with confidence, because I can
cover it with tests. So maybe the first point boils down to being afraid of
changing code that is intermingled with the rest of the system.

Secondly, I feel more confident in changing Haskell code than C code. If my
changed Haskell code compiles, I'm pretty sure that it works. That is probably
because of Haskell's rich type system. It feels like you get a lot of test for
free that you have to write manually in other languages. That can probably be
generalized to mean that I feel more confident in changing code that has good
test coverage.

Thirdly, I feel more confident in changing code in my own projects. In my own
projects, I am in charge of the requirements, and I know what is important and
what is not. If I am changing something that is less important, it doesn't
matter as much if it breaks. If I change someone else's code, I don't know if
it is important or not. And in my own projects, nothing is really that
important. And if it is important, and I break it, I will fix it quickly.

--

* [Comments on Google+](https://plus.google.com/u/0/112175093836850283531/posts/akGHDgL2b4t)
