# Mini GO

CS4212 Compiler Design, AY 2016-17 Semester 1

## Group Members

- Liu Siyuan (A0153030B)
- Francesco Picciotti (A0153447A)
- Yohanes Yudhi (A0152954Y)

## Description

This is a compiler for the mini GO language.

## Progress

- [x] Stage 1: Lexical and syntax analysis
- [x] Stage 2: Type checking
- [x] Stage 3: Code generation

## Compilation

To compile the compiler, just run

```bash
$ make clean && make
```

To compile and run a mini-go source program, type the following command

```bash
$ ./calc /path/to/your/program
```

## Example programs

Example programs are available in the `examples` folder. Every example program in that folder tests one or more language features.

- arithmetic: tests arithmetic operations
- eqgt.go: tests the `==` and `>` operator
- ite.go: tests `if exp {} else {}` structure and variable declaration in inner scope
- while.go: tests while loop (it's a infinite loop, will cause stack overflow)
- func.go: tests calling function as statement (`FuncCall`)
- return.go: tests calling function as expression (`FuncExp`) and recursive function call

## Known Issues

### Negative integers not supported

Our compiler doesn't support negative numbers. However, negative number doesn't seems to be part of the language specification.

### Returning variables in inner scope not supported

Although variable declaration/redeclaration in inner scope is supported, returning a variable in the inner scope is not supported. This 
is because the return statement must be the last statement. Therefore, variables in inner scope cannot be accessed by the return statement.
