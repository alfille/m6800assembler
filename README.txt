m6800assembler

20189 by Paul H Alfille

This is an assembler for the Motorola m6800 microprocessor.
It is a macro assembler (allows macros and functions to be defined)
and is a 2-pass assembler, so forward references to code positions
can be done.

The assembler is written in python (3+) and is under the MIT licence so easily modifiable.

-----------------

Command Line

$ python3 m6800assembler.py -h
usage: m6800assembler.py [-h] [-r [ROM]] [-p [PROG]] [-o [OUTPUT]] [-l [LIST]]
                         [-u [UNUSED]]

Motorola 6800 microprocessor assembly language compiler

optional arguments:
  -h, --help            show this help message and exit
  -r [ROM], --ROM [ROM]
                        Existing ROM file to compare with compiled code
  -p [PROG], --PROG [PROG]
                        Assembly language code to compile
  -o [OUTPUT], --OUTPUT [OUTPUT]
                        Assembly output (ROM file)
  -l [LIST], --LIST [LIST]
                        List all defined symbols and their values
  -u [UNUSED], --UNUSED [UNUSED]
                        List all unused symbols and their values
---------------------

Whirlwind tutorial

Here is an example of code:

Load_last_move_address:	LDX   PointerB2 ; DIRE ; DE B2    ; Load the Index Register
Downstream_byte_loop:	LDAB    0,X ; INDX ; E6 00    ; Load the B ACCUMULATOR from Memory
	STAB    1,X ; INDX ; E7 01    ; Store the B ACCUMULATOR in Memory
	DEX         ; ACCU ; 09       ; Decrement the Index Register
	DEC   B_AA ; EXTE ; 7A 00 AA ; Decrement the Memory Location
	BNE   Downstream_byte_loop ; RELA ; 26 F6    ; Z=0
	LDAB #  $0F ; IMM2 ; C6 0F    ; Load the B ACCUMULATOR from Memory
	STAB    1,X ; INDX ; E7 01    ; Store the B ACCUMULATOR in Memory
	LDAB  B_AB ; DIRE ; D6 AB    ; Load the B ACCUMULATOR from Memory
	STAB  B_AA ; DIRE ; D7 AA    ; Store the B ACCUMULATOR in Memory
	DECA        ; ACCU ; 4A       ; Decrement the A ACCUMULATOR
	BNE   Load_last_move_address ; RELA ; 26 E9    ; Z=0
Return_60C1:	RTS         ; IMPL ; 39       ; Return from Subroutine

Lines are in form of label: code ; comment
Labels are optional unless needed for jump or branch
Code follows standard format with examples of different addressing modes shown
Comments are anythng after a ';' 

Number format is decimal e.g. 16

Hexidecimal $10 or 0x10 or 10H

Binary %10000000

Character 'A

--------
Functions

Definition:
.FUNCTION Double(x) (2*x)

Usage:
CMPA # Double(3)

Any number of arguments
Must return a number (not line of code)
    note that the addressing mode is not given by the function

-----------
Macros

Definition:
.MACRO Double(x)
LDAA x
LSRA
STAA x
.ENDMACRO

Usage
Double( Range_for_sort )

Any number of arguemnts
Creaes lines of code (or data)

------------
Conditional compilation

.IF condition
.ELIF alternate
.ELSE
.ENDIF

Only .IF and .ENDIF are required
condition must resolve to 0 or non-zero
The condition must be evaluatable on the first pass, since otherwise the code size might
change on the second pass)

------------
Operators

()
+-
*/
**
^ & |
~ -
.AND .OR .XOR .NOT

------------
Symbols (variables)
Entrypoint .EQU value_or_expression

-------------
Arrays

TestStart[4] is 4 bytes after TestStart

-------------
Data

Non code can be entered as

.BYTE 1 3 $32 (2+4) ; 1 bytes values separated by spaces
.VECTOR $6400 ErrorRoutine ; 2 byte values separated by spaces
.WORD $1322 9999 ; 2 byte values separated by spaces

