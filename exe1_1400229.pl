myreverse([ ],[ ]).
myreverse([X|Xs],Ys):-myreverse(Xs,Rs),append(Rs,[X],Ys).

%question 1

mynumcount(K,M,0):- M-K<0.
mynumcount(K,M,X):- M1 is M-K+2,mynumcount(K,M1,X1),X is X1+1.

totalcount([],0,0,'').
totalcount([(A,B,C)|T],N,M,S):- mynumcount(B,C,X),totalcount(T,S1,M1,S2),M is max(M1,X),(M = X -> S = A;S = S2),N is S1+X.

pokemon(L,N,S):- totalcount(L,N,M,S), 0 =< M.

%question 2

mylunch4(S,N):- (S = f -> N is 1; S = e ->N is 2; S = d -> N is 3; S = a -> N is 4; S = b -> N is 5; S = c -> N is 6).

mylunch3(L, L).
mylunch3(S, L):- S =< L,T is S,Y is mod(T,2),Y is 0, A is S+3, mylunch3(A, L).
mylunch3(S, L):- S =< L,T is S+1,Y is mod(T,2),Y is 0, A is S+1, mylunch3(A, L).

mylunch(1, S, T) :- mylunch4(S, T).
mylunch(2, S, T) :- mylunch4(S,A),T is A + 6 + 1.
mylunch(R, S, T) :- mylunch3(1,R),mylunch4(S,A),T is (R-3)*7 + A +2.
mylunch(R, S, T) :- mylunch3(3,R),mylunch4(S,A),T is (R-3)*7 + A.

%question 3

is_palindrome(L) :- myreverse(L,L).

palindromic(L,0):- is_palindrome(L).
palindromic([H|T],N):- myreverse(T,T1),T1=[H1|T2],H>H1,T2=[H2|T3],myreverse(T3,T4),H3 is H1+H2,append(T4,[H3],T5),append([H],T5,T6),palindromic(T6,N1),N is N1+1.
palindromic([H|T],N):- myreverse(T,T1),T1=[H1|_],H<H1,T=[H2|T3],H3 is H+H2,append([H3],T3,T4),palindromic(T4,N1),N is N1+1.
palindromic([H|T],N):- myreverse(T,T1),T1=[H1|T2],H=H1,myreverse(T2,T3),palindromic(T3,N).


%question 4

returnlist1(0,_,[]).
returnlist1(N,X,L):- N1 is N-1,X1 is X+1,returnlist1(N1,X1,L1),L=[X1|L1].

matrix2(0,[],_).
matrix2(N,M,N2):- returnlist1(N2,0,L),N1 is N-1,matrix2(N1,M1,N2),M=[L|M1].

mymatrix(0,[]).
mymatrix(N,M):- T is N+1,Y is mod(T,2),Y is 0,matrix2(N,M,N).
