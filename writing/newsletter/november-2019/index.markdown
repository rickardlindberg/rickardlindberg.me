---
title: Newsletter November 2019
date: 2019-12-03
tags: newsletter
---

This is what I've been up to in November 2019:

* I continued working on RLiterate. You can see the progress
  [here](/projects/rliterate/book2/index.html#359aefc7fbb54cb4b8e43182efb241a0).

    * When implementing the text wrap algorithm, it was helpful to be able to
      change the page width interactively by mouse drag. It gave me quick
      feedback on the algorithm.

    * I fleshed out more parts of the application, and the new architecture
      seems to scale for what I need. There are still details to figure out,
      but overall I'm quite pleased.

* I watched [Anders Hejlsberg on Modern Compiler
  Construction](https://channel9.msdn.com/Blogs/Seth-Juarez/Anders-Hejlsberg-on-Modern-Compiler-Construction).
  The compiler he describes feels similar to how RLiterate operate in the sense
  that every time an even happens, the whole state is re-created. But some
  parts are cached. It also sparked the idea that RLiterate can use a "language
  server" to provide a better IDE experience.
