# Introduction
We recognize that one of the biggest challenges to aspiring programmers is the first step: getting started. To those who have never written a computer program in their lives, the alien syntax and complex logical flows make the task seem especially daunting.  Therefore, the goal of Stark is to create an approachable, beginner-friendly programming language targeted towards those learning to code for the first time, without the burdens imposed by the scale and complexity of modern-day programming languages.

First, syntax based on natural language and a multitude of intuitive control flow structures provide a sense of approachability and direction to the programmer as they develop familiarity with structured and procedural programming paradigms.

Second, while it also supports non-primitives like arrays, Stark is built around primitive data types and their operations. Together with the basic elements of control flow and function calls, Stark presents a condensed set of the fundamentals of an imperative programming language and a space within which students can practice these concepts.

Third, Stark’s strong and static type semantics compel the beginning programmer to carefully consider every variable they declare, giving them a deeper understanding of the instructions they execute while preparing them for more sophisticated languages like Java and C which they will encounter later in their programming careers.

Finally, the addition of features like tilde (~) logging serve as accessible tools that are convenient to use, simple for users to grasp, and allow for easily traceable code.

We hope that Stark is able to fulfill its purpose as a learning tool in the classroom or as a springboard to other languages, and in doing so make programming, software engineering, and computer science more accessible to students and learners.

# Language Design

## Types and Variable Declaration

Stark supports five primitive variable types:
| Type      | Size | Description | Example |
| ----------- | ----------- |----------- |----------- |
| int      | 32 bits  | stores whole number values | 0       |
| float    | 32 bits  | stores fractional number values | 3.14       |
| bool     | 1 bit    | represents true or false | true       |
| char     | 8 bits   | represents a single character using single quotes| 'a'       |
| string   | 8 bits * | represents a sequence of characters using double quotes | "stark"       |

*\* Strings are implemented as 8 bit pointers to character arrays.*

Variables are declared through define statements. Variables created through declarations are valid from the point of declaration until the end of its enclosing scope:
```
define x as int;
```

Stark also supports declaring arrays of primitive types with a fixed length. These arrays are allocated on the stack at the time of declaration and persist throughout its enclosing scope.
```
define arr as int[3];
```

## Operators and Casting

The unary arithmetic operators are `+` and `-`. The binary arithmetic operators are `+`, `-`, `*`, `/`, and `%`. The `+` and `-` operators have the same precedence, lower than the precedence of `*`, `/`, `%`. All arithmetic operators associate left to right.
```
2 * -(5 + 3.0)
```

The relational operators are `<`, `<=`, `>`, `>=`. They all have equal precedence. Below them in precedence are `==` and `!=`. All have lower precedence than arithmetic operators. The logical operators are `not`, `and`, and `or` (in order from greatest to least precedence).
```
not (x >= 2) or (x < 3)
```

The assignment operator is `<-`.  It has the lowest precedence and assigns an expression to a variable.  The `incr`, `decr`, `mult`, and `divi` assignment operators are syntactic sugar combining binary arithmetic operators with variable assignment.
```
~x <- 10;
incr ~x by x / 2;
```

Stark supports explicit type casting between certain variable types using the `as` keyword. Casting has the highest precedence, except for the tilde (~) logging operator.
```
define c as char;
c <- ((‘a’ as int) + 2) as char;
```

Stark only supports implicit type casting when promoting from an `int` to a `float` type.  Truncation from a `float` to an `int` must be performed explicitly.
```
define f as float;
f <- 2.0 + 3;
```

## Arrays

The declaration and instantiation of arrays is described above. Arrays can be accessed and written using square brackets containing the desired index.
```
define arr as int[3];
arr[0] <- 2;
```

A built-in `len` function is provided to access the length of a given array.
```
define arr_len as int;
arr_len <- len(arr);
```

## Control Flow and Functions

**Conditionals:** The `if` block runs if the conditional expression is `true`. Otherwise, if we have an optional `else` block, that will be executed instead.
```
if (expr) {
  (block);
}
else {
  (block);
}
```

**While:** A `while` loop first looks at the conditional expression given, and if it evaluates to `true`, the block runs until the condition is `false`.
```
while (expr) {
  (block);
}
```

**For:** A `for` loop runs the block as an iterating variable is incremented (or decremented) from a starting value or expression until it reaches or surpasses an ending value or expressions. An optional `every` clause specifies the amount to increment or decrement by.
```
for (id) from (expr) to (expr) every (expr) {
  (block);
}
```

**For-each:** A `for`-`each` loop is a specialized loop that iterates through an array. Each element of the array is copied into an iterating variable, then the block is run.
```
for each (id) in (id) {
  (block);
}
```

**Repeat-Until:** A `repeat`-`until` loop runs the block then checks the conditional expression. If it evaluates to `true`, the block runs again. Otherwise, it stops running.
```
repeat { 
  (block); 
}
until (expr);
```

**Functions:** A function defines a block of statements and a final expression, along with a name, a set of parameters and their types, and the output type of the final expression. Only the name and return type are required. Functions are declared with the keyword `function`.  The `return` keyword is used to mark the final expression that the function will return.
```
function my_add (a as int, b as int) as int {
  define sum as int;
  sum <- a + b;
  return sum;
}
```

Functions can be called with the following syntax:
```
define i as int;
i <- my_add(3, 5);
```

## Runtime Errors

Stark monitors for a set of runtime errors as the program is being executed. These include array out-of-bounds errors, divide by zero errors, and null pointer errors. After a runtime error is encountered, the program prints an error message and terminates.

## Tilde Logging (~)
For debugging, the tilde operator `~` can be placed to the left of a variable, literal, or expression to log its value on the console.
```
~x <- 10 + ~x;
~"Hello World!";
```

Tilde logging is designed to be a convenient debugging tool with the following features that distinguish it from Stark’s built-in printing functions:
* It performs automatic type inference to remove the need for relying on a different print (e.g., `print`, `printf`, `printb`, `printc`, `prints`) function for each data type.
* It can be nested, repeated multiple times in an expression, or included on both sides of an assignment statement, helping debug complex expressions.
* It prints the underlying data without processing (e.g., (null) for a null string pointer instead of throwing a null pointer error), which is useful for debugging.
* It uses convenient syntax, which is easy to add or remove and reduces clutter.


# Architectural Design

The above block diagram shows the major components of our translator, and the inputs and outputs for specific steps. `Stark.ml` takes in a `.stark`  source program file and runs it through the Scanner, Parser, Semantics Checker, and IR Generator (IRGen).

The Scanner reads the `.stark` file to generate tokens from a sequence of characters. Single and multi-line comments are also handled (and ignored) by the Scanner.

The Parser sets the context-free grammar of the language, including precedence and associativity rules for the tokens. These tokens are then parsed into an abstract syntax tree (AST). Some elements of syntactic sugar are also handled at this level, e.g., decomposing of increment statements into separate addition and assignment statements.

The Semantics Checker then checks the static semantics of the syntax tree and forms a semantically checked AST (SAST), throwing type or scope errors if any are found. The Semantics Checker also handles implicit type casting, e.g., from `int` to `float` types. The SAST can be pretty-printed to a `.out` file in a C-like style for testing and debugging.

Finally, IRGen uses this SAST to generate a LLVM intermediate representation (`.ll`) file, using OCaml’s `Llvm` module. This file can be run with `lli` to execute the program.

# Test Plan

Below, we include two examples of source language programs and their corresponding compiled LLVM IR code. We also describe our process for automated testing.

## Example 1: Hello World
**Source language program (Stark)**
```
function main() as int {
    prints("Hello, world!");
}
```

**Target language program (LLVM IR)**
```
; ModuleID = 'Stark'
source_filename = "Stark"
@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@fmt.1 = private unnamed_addr constant [4 x i8] c"%f\0A\00", align 1
@fmt.2 = private unnamed_addr constant [4 x i8] c"%c\0A\00", align 1
@fmt.3 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@str = private unnamed_addr constant [14 x i8] c"Hello, world!\00", align 1
@0 = private unnamed_addr constant [29 x i8] c"[runtime error] null pointer\00", align 1
declare i32 @printf(i8*, ...)
define i32 @main() {
entry:
  %0 = alloca i8*, align 8
  store i8* getelementptr inbounds ([29 x i8], [29 x i8]* @0, i32 0, i32 0), i8** %0, align 8
  br i1 false, label %err, label %no_err
err:                                              ; preds = %entry
  %1 = load i8*, i8** %0, align 8
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.3, i32 0, i32 0), i8* %1)
  ret i32 0
no_err:                                           ; preds = %entry
  %printf1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.3, i32 0, i32 0), i8* getelementptr inbounds ([14 x i8], [14 x i8]* @str, i32 0, i32 0))
  ret i32 0
}
```

## Example 2: Euclidean Algorithm
**Source language program (Stark)**
```
function main () as int {
    define x as int;
    define y as int;
    define a as int;
    define i as int;
    define res as int;
    x <- 15;
    y <- 5;
    a <- 0;
    i <- 1;
    res <- 0;
    if (x > y) {
        a <- y;
    }
    else {
        a <- x;
    }
    a <- a + 1;
    for i from 1 to a every 1 {
        if ((x % i == 0) and (y % i == 0)) {
            res <- i;
        }
    }
    ~res;
}
```

**Target language program (LLVM IR)**
```
; ModuleID = 'Stark'
source_filename = "Stark"
@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@fmt.1 = private unnamed_addr constant [4 x i8] c"%f\0A\00", align 1
@fmt.2 = private unnamed_addr constant [4 x i8] c"%c\0A\00", align 1
@fmt.3 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
declare i32 @printf(i8*, ...)
define i32 @main() {
entry:
  %0 = alloca i8*, align 8
  %x = alloca i32, align 4
  store i32 0, i32* %x, align 4
  %y = alloca i32, align 4
  store i32 0, i32* %y, align 4
  %a = alloca i32, align 4
  store i32 0, i32* %a, align 4
  %i = alloca i32, align 4
  store i32 0, i32* %i, align 4
  %res = alloca i32, align 4
  store i32 0, i32* %res, align 4
  store i32 15, i32* %x, align 4
  store i32 5, i32* %y, align 4
  store i32 0, i32* %a, align 4
  store i32 1, i32* %i, align 4
  store i32 0, i32* %res, align 4
  %x1 = load i32, i32* %x, align 4
  %y2 = load i32, i32* %y, align 4
  %tmp = icmp sgt i32 %x1, %y2
  br i1 %tmp, label %then, label %else
err:                                              ; No predecessors!
  %1 = load i8*, i8** %0, align 8
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.3, i32 0, i32 0), i8* %1)
  ret i32 0
then:                                             ; preds = %entry
  %y3 = load i32, i32* %y, align 4
  store i32 %y3, i32* %a, align 4
  br label %if_end
else:                                             ; preds = %entry
  %x4 = load i32, i32* %x, align 4
  store i32 %x4, i32* %a, align 4
  br label %if_end
if_end:                                           ; preds = %else, %then
  %a5 = load i32, i32* %a, align 4
  %tmp6 = add i32 %a5, 1
  store i32 %tmp6, i32* %a, align 4
  %a7 = load i32, i32* %a, align 4
  store i32 1, i32* %i, align 4
  br label %while
while:                                            ; preds = %if_end19, %if_end
  %2 = load i32, i32* %i, align 4
  %3 = icmp sle i32 1, %a7
  %4 = icmp sge i32 1, %a7
  %5 = icmp sle i32 %2, %a7
  %6 = icmp sge i32 %2, %a7
  %7 = and i1 %4, %6
  %8 = and i1 %3, %5
  %9 = or i1 %8, %7
  br i1 %9, label %while_body, label %while_end
while_body:                                       ; preds = %while
  %x8 = load i32, i32* %x, align 4
  %i9 = load i32, i32* %i, align 4
  %tmp10 = srem i32 %x8, %i9
  %tmp11 = icmp eq i32 %tmp10, 0
  %y12 = load i32, i32* %y, align 4
  %i13 = load i32, i32* %i, align 4
  %tmp14 = srem i32 %y12, %i13
  %tmp15 = icmp eq i32 %tmp14, 0
  %tmp16 = and i1 %tmp11, %tmp15
  br i1 %tmp16, label %then17, label %if_end19
then17:                                           ; preds = %while_body
  %i18 = load i32, i32* %i, align 4
  store i32 %i18, i32* %res, align 4
  br label %if_end19
if_end19:                                         ; preds = %while_body, %then17
  %10 = load i32, i32* %i, align 4
  %11 = add i32 %10, 1
  store i32 %11, i32* %i, align 4
  br label %while
while_end:                                        ; preds = %while
  %res20 = load i32, i32* %res, align 4
  %printf21 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i32 %res20)
  ret i32 0
}
```

## Automated Testing
All test programs, test outputs, and correct outputs are located within the `test/` directory.

We wrote a Makefile for automated testing of our compiler. Running `make run` builds the compiler using `dune build` then compiles and executes a single `test.stark` program. Alternatively, running `make test` builds the compiler then executes a whole suite of test cases. The test cases cover features like variable declarations, conditional statements and loops, runtime errors, function calls, tilde logging, type casting, etc.

We used the `diff` command to check for the correctness of the test outputs against a set of pre-programmed outputs. If the program’s output is incorrect, the test harness will terminate and log an error message. Otherwise, if no error is thrown, this indicates that the program ran successfully and that the outputs were as expected.
