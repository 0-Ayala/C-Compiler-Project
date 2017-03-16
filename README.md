My compiler works for integers, global variables, conditionals (if-then-else), and loops of some kind while loops.
countdown.cs: prints the numbers between 1 and 100
factorial.cs: calculates and prints the factorial of numbers between 1 and 10
notFizzBuzz.cs: prints the numbers (in order) between 1 and 100 that are divisible by neither 3 nor 5

compiles and makes asm code using gcc -o (executable_name) (source_file.s)
all the .s files are written to the input folder

I have also implemented inline comments, block somments (using @'s as the opening and closing marks), string literals (which handles all letter and number chars as well as symbols), printing of strings, printing of expressions, return statements, blocks, methods, classes, and assignment expressions.

The compiler can not completely parse for loops, it gets stuck on the very last bit of a loop that increments or decrements the lower/upper bound. It can, however, parse everything else that is a part of the for loop if the increment/decrement part is left out...for now I have commented out both versions of the foor loop I have been working on so that it will not interfere with the rest of the compiler. It also can't handle arrays, objects, local variables, or do type checking.