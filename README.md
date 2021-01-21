# janE6502

Fork of http://home.foni.net/~sjanda/

janE 6502 is an emulator program which is able to interpret programs written for 6502-like microprocessors on a PC-based host system. It offers a very high degree of formalization and can be used for very thin and hopefully reliable applications, whenever old or vintage pieces of software shall be re-used. It is following a unique approach: it makes extensive use of the inherent numerical order contained in the instruction set itself, rather trying to compute as much as possible out of these given informations than simply gathering a bunch of sub-programs for every single instruction encountered. Thus it is possible to cover multiple machine instructions often in just 1 single line of pascal code. Possibly this could be useful for efficient hardware implementations or 'homebrew' computers. 
