# FLEX Language

A toy language designed to flex

## Goals

1. Flex on peasants who didn't implement their own language yet
2. Figure out how far I can go with implementing my own language
3. Learn and have fun

## Features
- variable bindings
```
>> let a = 1;
>> let b = 2;

>> a + b
3
```
- integers and booleans (floats? never heard of it)
```
>> !(0 + 1) == false
true
```
- arithmetic expressions
```
>> 1 + 1 * 4
5
```
- conditions (they are expressions!)
```
let a = if (1 < 10) {
    true;
} else { false; }

>> a;
true
```
- first-class and higher-order functions / closures
```
let a = fn(x) {
    return fn(y) {
        x * y;
    };
}

>> a(10)(5);
50
```
- strings and operations on them
```
// Concantination
let a = "hello";
let b = " world";

>> a + b + "!";
"hello world!"

// String repeat
let a = "hi! ";

>> a * 2;
"hi! hi! "
```


## TODO

- [ ] More friendly errors handling
- [ ] JIT Compiler
- [ ] Comments
- [ ] LOOPS!!! For Gods sake
