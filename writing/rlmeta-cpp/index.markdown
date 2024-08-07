---
title: 'DRAFT: Porting RLMeta to C++'
date: 2020-04-10
tags: rlmeta,draft
---
**This is a work in progress that will change. Like to see it finished?
Let me know by sending me an email.**

In this article I will port RLMeta to C++. The main reason for doing it
is to learn C++. I have the impression that C++ is a large and complex
language, but I think I should be able to grasp the basics pretty
quickly because I know C quite well and I am also familiar with object
oriented programming. This article will describe my learning journey as
well as the port. Another reason for doing the port is to see how RLMeta
looks like when C++ is the target language. Will it fit well? Will
performance increase?

-   [Representing data](#f935efac99094d30b035430ba6c263cb)
-   [Misc](#8fe0f8cbf13f49c0a4d9aa4f92fa2540)

[]{#f935efac99094d30b035430ba6c263cb}Representing data
------------------------------------------------------

The first thing I think about is how to represent the data that RLMeta
needs. In particular I think about how to store the mapping between rule
names and their location in the program. The map in the C++ standard
library looks like a dict in Python. But there is also something about
an allocation strategy. I need to learn more about how memory is handled
in C++.

I read about classes, constructors, destructors and initialisation. I
create a small example to clarify my thinking. Here I also learn about
basic C++ syntax.

```text
#include <iostream>

class Foo {
    private:
        int id;
    public:
        Foo(int id) : id(id) {
            std::cout << "Creating foo " << id << "\n";
        }
        ~Foo() {
            std::cout << "Destructing foo " << id << "\n";
        }
};

int main() {
    std::cout << "Entering main\n";
    Foo foo1(1);
    Foo *foo2 = new Foo(2);
    std::cout << "Exiting main\n";
    return 0;
}
```

Output:

```text
Entering main
Creating foo 1
Creating foo 2
Exiting main
Destructing foo 1
```

This shows me that objects are destructed automatically if allocated on
the stack. But not when newed.

I go back to the map and make a small example to learn how to use it:

```text
#include <iostream>
#include <map>

int main() {
    std::map<std::string, int> rules;
    rules.insert(std::pair<std::string, int>("foo", 10));
    rules.insert(std::pair<std::string, int>("bar", 15));
    std::cout << "foo = " << rules["foo"] << "\n";
    std::cout << "bar = " << rules["bar"] << "\n";
    return 0;
}
```

Output:

```text
foo = 10
bar = 15
```

I think the type declarations make the code hard to read. But the map
seems to do what I want. So I\'m happy. I also conclude that the map
allocates memory to make space for all pairs. But at the end of `main`
it will all be freed automatically. So no memory will leak.

Next I\'m thinking about how to represent instructions. In the Python
version an instruction is represented as a tuple `(name, arg1, arg2)`.
Where the arguments can be of arbitrary type (string, integer, lambda,
etc.).

Getting stuck. I think about how to represent the values that RLMeta
works with instead. Lists, strings, integers, characters.

```text
#include <iostream>
#include <list>
#include <memory>

class RLMetaObject {
    public:
        virtual ~RLMetaObject() {
        }
};

class RLMetaInteger : public RLMetaObject {
    private:
        int value;
    public:
        RLMetaInteger(int value) : value(value) {
        }
        ~RLMetaInteger() {
            std::cout << "Destructing integer " << value << "\n";
        }
};

class RLMetaString : public RLMetaObject {
    private:
        std::string value;
    public:
        RLMetaString(std::string value) : value(value) {
        }
        ~RLMetaString() {
            std::cout << "Destructing string " << value << "\n";
        }
};

int main() {
    std::list<RLMetaObject> objects;
    objects.push_back(RLMetaInteger(5));
    objects.push_back(RLMetaString("hello"));
    std::list<std::unique_ptr<RLMetaObject>> objectPointers;
    objectPointers.push_back(
        std::make_unique<RLMetaInteger>(6)
    );
    objectPointers.push_back(
        std::make_unique<RLMetaString>("world")
    );
    return 0;
}
```

```text
Destructing integer 5
Destructing string hello
Destructing integer 6
Destructing string world
```

Since C++ containers can not store objects of arbitrary type, I
introduce a base class. I also experiment with smart pointers. I find
out that a virtual destructor is needed to make proper destruction. That
really confused me.

Dynamic cast can work as `isinstance` in Python.

```text
#include <iostream>
#include <list>
#include <memory>

class RLMetaObject {
    virtual bool matches(const RLMetaObject &other) {
        return false;
    }
};

class RLMetaInteger : public RLMetaObject {
    private:
        int value;
    public:
        RLMetaInteger(int value) : value(value) {
        }
        bool matches(const RLMetaObject &other) {
            try {
                const RLMetaInteger &otherInt = dynamic_cast<const RLMetaInteger&>(other);
                return otherInt.value == value;
            } catch (std::bad_cast) {}
            return false;
        }
};

int main() {
    RLMetaObject obj;
    RLMetaInteger int5(5);
    RLMetaInteger int6(6);
    std::cout << "Integer(5) matches Object?     " << int5.matches(obj)  << "\n";
    std::cout << "Integer(5) matches Integer(6)? " << int5.matches(int6)  << "\n";
    std::cout << "Integer(6) matches Integer(6)? " << int6.matches(int6)  << "\n";
    return 0;
}
```

```text
Integer(5) matches Object?     0
Integer(5) matches Integer(6)? 0
Integer(6) matches Integer(6)? 1
```

For this to work, there has to be some virtual member in the base class.

[]{#8fe0f8cbf13f49c0a4d9aa4f92fa2540}Misc
-----------------------------------------

```
1.  runcpp.sh
```

```sh
cat > tmp.cpp && g++ tmp.cpp -o tmp && ./tmp && rm tmp.cpp tmp
```
