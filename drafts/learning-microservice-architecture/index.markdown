---
title: Learning Microservice Architecture
---

I've watched Fred George's talks about microservice architecture and I find
them very interesting. It sounds like a different way to structure your
programs.  A different architecture that I've not come across before. It also
sounds a bit like the actor model. But the talks only give an overview. How do
you do it in practice? I want to learn. I will explore it by trying to
implement it and document the process here.

First of all I need a problem to solve. Is microservice architecture only
suited for some kinds of problems? It seems like you want to solve small
problems, like the string calculator kata, within a single microservice. But
let's try to break it up.  How can we partition the string calculator into
multiple microservices?

"" -> 0
"5" -> 5
"2,3" -> 5
"1,2,3" -> 6

Possible actors:

- add two numbers
- split string on numbers

## Framework

In order to test my ideas, I will build a small framework. I want to implement
a message bus where anyone can publish messages and anyone can subscribe to
messages. That way each microservice does not have to know about each other.
They just need to know how to connect to the bus.

I'm not sure if you always implement microservice architecture with a message
bus, but from Fred's presentation, I got the feeling that's how they did it.

$include index$
