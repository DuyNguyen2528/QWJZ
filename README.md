# QWJZ
A programing language written in Racket (lisp like lanaguage)
Featuring some built in function and primitive operation (condition, inequality, arithmetic, array, memory)
There are 3 .rkt file each file support unique feature:
- array-QWJZ: Suppose memory and built-in function to manipulate array (aset, aref, array, mutation with notation :=)
              The array has each element that point to location in memory
              Each element can be mutated by using notation :=
- read-input-QWJZ: Support reading an input
                   Support println that print message
                   Println support multiple argument inputs
- typed-QWJZ: Add typed checker
              Will error if there are typed error
To run this lanague:
- copy the program into Dr.Racket
- have QWJZ program written
- call top-interp ('your-program-here)
