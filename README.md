# Mini GO

CS4212 Compiler Design, AY 2016-17 Semester 1

## Group Members

- Liu Siyuan (A0153030B)
- Francesco Picciotti (A0153447A)
- Yohanes Yudhi (A0152954Y)

## Description

This is a compiler for the mini GO language.
The compiler supports function calls assuming that if there's a return statement, it's the last instruction in the function body.
Integer numbers are defined only as positive numbers. 
Variable redeclarations in inner scopes are allowed and correctly implemented. Variables declared inside inner scopes in functions cannot be returned. Concurrency part not implemented yet.   

## Progress

- [x] Stage 1: Lexical and syntax analysis
- [x] Stage 2: Type checking
- [x] Stage 3: Intermediate code generation

## Compilation

To compile the compiler, just run

```bash
$ make clean && make
```

To compile a mini-go source program, type the following command

```bash
$ ./calc /path/to/your/program
```

## Example programs

Example programs are available in the `examples` folder. Every example program in that folder tests one or more language features.

- eqgt.go: tests the `==` and `>` operator
- func.go: tests calling function (FuncCall)
- ite.go: tests `if exp {} else {}` structure and variable declaration in inner scope
- return.go: tests calling function that returns a value (FuncExp) and recursive function call
- arithmetic: tests arithmetic operations
- while.go: tests while loop (it's a infinite loop, will cause stack overflow)

## Known Issues
