# haskell_grammatical_langage_corrector

_Grammatical Language Correction Written in Haskell._

# Description

A grammatical language corrector that is able to find errors in a text and display possible corrections. The language correction mechanism has all been implemented in Haskell. No external API is used.

# Usernames and Passwords

| Username   | Password |
| ---------- | -------- |
| alex       | hello    |
| john       | doe      |
| iburzynski | teacher  |

# Correction mode

## Mode A - Start correcting the input.txt file

This correction modes takes what's written in the input.txt file and correct it.

## Mode B - Interactive correction mode

This is a interactive mode. You write any sentence you want and this sentence will be corrected.

# Correction algorytm

TODO

# Limitations

The program is essentially a grammar correction. So, everything not related to grammar are not taken into account when correcting words and sentences.

For example:

- The program does not correct punctuation errors
- The programs does not detect words that should be singular or plural

# Haskell Version Tested

| Program     | Tested and supported versions |
| ----------- | ----------------------------- |
| GHCup       | 0.1.18.0                      |
| Stack       | 2.9.1                         |
| HLS         | 1.8.0.0                       |
| Cabal       | 3.6.2.0                       |
| GHC         | 9.2.4 (base-4.16.3.0)         |