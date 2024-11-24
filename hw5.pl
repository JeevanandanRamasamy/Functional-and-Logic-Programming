:- use_module(library(clpfd)).


% Define the relations described below. You may test your code using SWI-Prolog
% by entering consult(hw5) or [hw5] as a query in swipl, when you have started
% swipl in the directory containing this file.

% a few pre-defined binary trees
t1(bin(bin(tip, a, tip), b, bin(tip, c, tip))).
t2(bin(tip, a, bin(tip, b, bin(tip, c, tip)))).
t3(bin(bin(bin(tip,a,tip),b,tip),c,bin(tip,d,tip))).

% ----
% Example. Define a relation swap/2 which relates a pair to its mirrored 
% version. We will use -/2 to represent pairs, e.g., X-Y.
%
% ?- swap(a-b, b-a).
% true.
% 
% ?- swap(1-2,X).
% X = 2-1.
% 
% ?- swap(X, hello-goodbye).
% X = goodbye-hello.
% 
% ?- swap(P, Q).
% P = _A-_B,
% Q = _B-_A.

swap(X-Y, Y-X).


% ----
% 1. Define a relation zip/3 that relates three lists of the same length, where
% each element of the third list is a pair containing the corresponding
% elements of the first two lists.
% 
% ?- zip([1,2],[a,b],[1-a,2-b]).
% true.
% 
% ?- zip([1,2],[a,b],L).
% L = [1-a, 2-b].
%
% Unlike Haskell's zip, we do not zip lists of different lengths
%
% ?- zip([1],[2,3],L).
% false.

zip([], [], []).
zip([A|T1],[B|T2],[A-B|T3]) :- zip(T1, T2, T3).

% ----
% 2. Define a relation sorted/1 that holds when its argument is a list of
% integers in non-decreasing order.
% 
% ?- sorted([]).
% true.
% 
% ?- sorted([1,2,100,255]).
% true .
% 
% ?- sorted([1,2,100,25]).
% false.
% 
% ?- sorted([1,X,1,20]).
% X = 1.

sorted([]).
sorted([_]).
sorted([A,B|T]) :- A #=< B, sorted([B|T]).

% ----
% 3. Define a relation symmetric/1 that holds when its argument is a binary
% tree that is equal to its mirror image (the tree created by switching the
% left and right children of every node).
%
% Suggestion: Consider defining a helper relation mirror/2.
%
% ?- symmetric(tip).
% true.
% 
% ?- symmetric(bin(tip, 1, bin(tip, 2, tip))).
% false.
% 
% ?- symmetric(bin(bin(tip, 0, tip), 1, bin(tip, 2, tip))).
% false.
% 
% ?- symmetric(bin(bin(tip, 1, bin(tip, 2, tip)), foo, bin(bin(tip, 2, tip), 1, tip))).
% true.
% 
% ?- symmetric(bin(bin(tip, 1, bin(tip, 2, tip)), foo, R)).
% R = bin(bin(tip, 2, tip), 1, tip).

symmetric(tip).
symmetric(bin(L, _, R)) :- mirror(L, R).
mirror(tip, tip).
mirror(bin(L1, N1, R1), bin(L2, N2, R2)) :- N1 = N2, mirror(L1, R2), mirror(R1, L2).

% ----
% 4. Define a relation numbered/4 that holds when its arguments are
%     1. a binary tree T
%     2. a binary tree NT with the same shape as T, but all elements have been replaced
%        with numbers that increase from left to right
%     3. the number of the left-most node of NT
%     4. the number of the right-most node of NT
%     
% Note that numbered/4 does not hold if T and NT are tip.

% ?- t1(T), numbered(T, NT, 1, _).
% T = bin(bin(tip, a, tip), b, bin(tip, c, tip)),
% NT = bin(bin(tip, 1, tip), 2, bin(tip, 3, tip)) ;
% false.
% 
% ?- t2(T), numbered(T, NT, 1, _).
% T = bin(tip, a, bin(tip, b, bin(tip, c, tip))),
% NT = bin(tip, 1, bin(tip, 2, bin(tip, 3, tip))) ;
% false.
% 
% ?- t3(T), numbered(T, NT, 1, _).
% T = bin(bin(bin(tip, a, tip), b, tip), c, bin(tip, d, tip)),
% NT = bin(bin(bin(tip, 1, tip), 2, tip), 3, bin(tip, 4, tip)) ;
% false.

sameshape(tip, tip).
sameshape(bin(L1, _, R1), bin(L2, _, R2)) :- sameshape(L1, L2), sameshape(R1, R2).

seqtree(Low, tip, Low).
seqtree(Low, bin(L, N, R), High) :- N1 #= N + 1, seqtree(Low, L, N), seqtree(N1, R, High).

numbered(tip, tip, N, N).
numbered(bin(L, _, R), NT, Smallest, Largest) :- 
    sameshape(bin(L, _, R), NT), 
    seqtree(Smallest, NT, LargestMinusOne),
    Largest #= LargestMinusOne - 1.
