Solving a Rubik's cube with an amb evaluator
===========================================

Implementation of a Rubik's cube in Racket.  

Includes a simple nondeterministic solver that uses the [Amb Evaluator](https://mitpress.mit.edu/sicp/full-text/sicp/book/node91.html) from SICP.

Solved cube representation
```bash
    front         180° ↺         180° ↶
   +w-w-w---+     +w-w-w---+     +y-y-y---+ 
  / w w w  /|    / w w w  /|    / y y y  /| 
 /  w w w / |   /  w w w / |   /  y y y / | 
+--------+ooo  +--------+rrr  +--------+rrr 
|        |ooo  |        |rrr  |        |rrr 
|b  b  b |ooo  |g  g  g |rrr  |b  b  b |rrr 
|b  b  b | /   |g  g  g | /   |b  b  b | /  
|b  b  b |/    |g  g  g |/    |b  b  b |/   
+--------+     +--------+     +--------+    
```

[Superflipped](https://en.wikipedia.org/wiki/Superflip) cube representation
```bash
    front         180° ↺         180° ↶
   +w-g-w---+     +w-b-w---+     +y-g-y---+ 
  / r w o  /|    / o w r  /|    / o y r  /| 
 /  w b w / |   /  w g w / |   /  y b y / | 
+--------+owo  +--------+rwr  +--------+ryr 
|        |bog  |        |grb  |        |brg 
|b  w  b |oyo  |g  w  g |ryr  |b  y  b |rwr 
|r  b  o | /   |o  g  r | /   |o  b  r | /  
|b  y  b |/    |g  y  g |/    |b  w  b |/   
+--------+     +--------+     +--------+    
```

