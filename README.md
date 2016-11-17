# Mini GO

CS4212 Compiler Design, AY 2016-17 Semester 1

## Group
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

## Testing

To test type checking, run the following command

```bash
$ make testchecker
```

## Compilation

To compile the compiler, just run

```bash
$ make
```

To compile a mini-go source program, type the following command

```bash
$ ./calc /path/to/your/program
```
