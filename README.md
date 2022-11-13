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
Grammatical mistakes in the text are found using different correction algorythms.
The process is pretty simple. 

1. Is the word in the dict.txt dictionnary ?
    - If yes -> No mistakes
    - If no  -> Continue


2. Create a list of all the possible words by replacing 1 char in the word.
   
   For example, if we take the word **hoskell** (instead of haskell), we would generate this list -> 
   
   ```
   [aoskell, boskell, coshell, doskell, ..., *haskell*, hbskell, hcskell, hdskell, heskell ...] 
   ```
   
   This is used to find if one of the character has been mispelled when typing.

3. Create a list of all the possible words by adding 1 character in every position of the word.
   
   For example, if we take the word **hakell** (instead of haskell), we would generate this list -> 
   
   ```
   [ahakell, bhakell, chakell, dahakell, ..., haakell, hbakell, hcakell, hdakell, heakell, ..., haqkell,  harkell, *haskell*, ...]
   ```
   This is to find if one of the character is missing when typing.

4. Create a list of all the possible words by switching 2 characters in every position of the word.
   
   For example, if we take the word **hsakell** (instead of haskell), we would generate this list -> 
   
    ```
    [shakell, *haskell*, hskaell, hsaekll hsaklel]
    ```

This is to find if 2 characters has been erronly switched when typing.

5. Create a list of all possible words with capital letters.

    For example, if we take the word **HASKell**, we would generate this list -> 

    ```
    [hASKell, HaSKell, HasKell, HASkell] -> No correction found for HASKell
    ```
6. Create a list like on step 5 but with possible words with lower capital letters.

7. We pass trhough all the possible words generation, and all the ones that are found in the dictionary Dict.txt are possible correction of the word. Those corrections will be displayed on the screen and saved in a Map for future access.


**Example:**

Input: 
```
Alexandre likos hoskell
```

Output:
```
Alexandre likos [likes] hoskell [haskell]
```

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