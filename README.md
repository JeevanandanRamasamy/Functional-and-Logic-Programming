# Functional and Logic Programming

This repository contains code and solutions for various assignments in Rutgers University’s CS314 course, focusing on functional and logic programming. It includes implementations in both Haskell and Prolog, tackling problems related to list manipulation, binary tree operations, and regular expression parsing. Each file is organized to demonstrate key programming concepts such as recursion, pattern matching, higher-order functions, and constraint logic.

---

## Haskell Guide

1. **Setting Up:**
   - To work with the Haskell code, ensure that you have GHC (Glasgow Haskell Compiler) installed on your system.
   - Place all relevant files (`DFA.hs`, `Regex.hs`, `Parsing.hs`, `RegexP.hs`) and the main solution file (e.g., `HW3.hs`) in the same directory.

2. **Compiling and Running:**
   - You can load the solution file in GHCi (Haskell's interactive shell) for testing and exploration.
   - Open your terminal and run the following command:
     ```bash
     ghci HW3.hs
     ```

3. **Example Usage in GHCi:**
   - After loading the file, you can directly interact with the functions. Here are some common examples:
   
     - **Parse a Regular Expression:**
       ```haskell
       parseRE "a*b+"  -- Parses a regular expression to check for correctness.
       ```

     - **Match a String Against a Regular Expression:**
       ```haskell
       match' (getRE "a*b+") "aaab"  -- Checks if the string matches the language of the regex.
       ```

     - **Generate Strings from a Regular Expression:**
       ```haskell
       take 10 (generate (getRE "a*b+"))  -- Generates the first 10 strings from the language described by the regex.
       ```

     - **Get Strings Ordered by Length (Lexicographically):**
       ```haskell
       generate' (getRE "a*b+")  -- Returns strings ordered by length in lexicographical order.
       ```

4. **Exploring and Testing:**
   - Use the functions provided in the various files to explore regular expressions, generate strings, or test whether strings match a given pattern.
   - Modify and experiment with the code to better understand the behavior of each function and the underlying concepts.

---

## Prolog Guide

1. **Setting Up:**
   - To work with the Prolog code, you need an environment like SWI-Prolog installed. You can download it from [here](https://www.swi-prolog.org/Download.html).
   - Ensure your Prolog file (e.g., `hw5.pl`) is in the same directory as your Prolog interpreter.

2. **Running the Code:**
   - Open the terminal and start SWI-Prolog by typing:
     ```bash
     swipl
     ```

   - Load the Prolog file into the SWI-Prolog environment:
     ```prolog
     ?- [hw5].
     ```

3. **Example Queries:**
   - After loading the file, you can run the following example queries to test the relations defined in the code:

     - **Test the `swap/2` relation:**
       ```prolog
       ?- swap(a-b, b-a).
       true.
       ```

     - **Test the `zip/3` relation:**
       ```prolog
       ?- zip([1, 2], [a, b], L).
       L = [1-a, 2-b].
       ```

     - **Check if a list is sorted:**
       ```prolog
       ?- sorted([1, 2, 3, 5, 10]).
       true.
       ```

     - **Test the `symmetric/1` relation on a binary tree:**
       ```prolog
       ?- symmetric(bin(bin(tip, 1, tip), 2, bin(tip, 3, tip))).
       false.
       ```

     - **Check the `numbered/4` relation:**
       ```prolog
       ?- t1(T), numbered(T, NT, 1, _).
       T = bin(bin(tip, a, tip), b, bin(tip, c, tip)),
       NT = bin(bin(tip, 1, tip), 2, bin(tip, 3, tip)).
       ```

4. **Modifying and Extending:**
   - You can modify the Prolog code or extend the queries to test additional cases or explore the binary tree operations, sorting, and constraint-solving behavior in more detail.
   - Prolog's backtracking mechanism allows you to explore multiple solutions for each query.
