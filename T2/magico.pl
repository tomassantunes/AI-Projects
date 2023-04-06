%4*4

% |13|14|15|16|
% | 9|10|11|12|
% | 5| 6| 7| 8|
% | 1| 2| 3| 4|

tamanho(4).
 
% var(coordenada, dominio, valor)
estado_inicial(e([var((1,1), [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16], _),
                  var((2,1), [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16], _),
                  var((3,1), [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16], _),
                  var((4,1), [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16], _),
                  var((1,2), [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16], _),
                  var((2,2), [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16], _),
                  var((3,2), [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16], _),
                  var((4,2), [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16], _),
                  var((1,3), [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16], _),
                  var((2,3), [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16], _),
                  var((3,3), [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16], _),
                  var((4,3), [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16], _),
                  var((1,4), [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16], _),
                  var((2,4), [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16], _),
                  var((3,4), [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16], _),
                  var((4,4), [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16], _)],[])).

verifica_restricoes(e(_,I)):- verifica_diferentes(I), verifica_linhas(I), verifica_colunas(I), verifica_diagonais(I). 

verifica_diferentes([]).
verifica_diferentes([V|I]):- diferentes(V, I), verifica_diferentes(I).

diferentes(_, []).
diferentes(var(_,_,V), [var(_,_,V1)|I]):- V \= V1, diferentes(var(_,_,V), I).

verifica_linhas(I) :- 
    findall(V, member(var((_, 1), _, V), I), L1),
    findall(V, member(var((_, 2), _, V), I), L2), 
    findall(V, member(var((_, 3), _, V), I), L3), 
    findall(V, member(var((_, 4), _, V), I), L4),
    soma(L1, A), soma(L2, B), soma(L3, C), soma(L4, D),
    equals(A, B, C, D). 

verifica_colunas(I) :- 
    findall(V, member(var((1, _), _, V), I), L1),
    findall(V, member(var((2, _), _, V), I), L2), 
    findall(V, member(var((3, _), _, V), I), L3), 
    findall(V, member(var((4, _), _, V), I), L4),
    soma(L1, A), soma(L2, B), soma(L3, C), soma(L4, D),
    equals(A, B, C, D).

verifica_diagonais(I) :-
    findall(V, member(var((X, X), _, V), I), L1),
    tamanho(N), diagonal(I, L2, 1, N),
    soma(L1, A), soma(L2, B), equals(A, B).

diagonal([], [], _).
diagonal(I, L, X, Y) :- member(var((X, Y), _, V), I), diagonal(I, [V|L], X+1, Y-1). 

equals(A, A).
equals(A, A, A, A).

soma([], 0).
soma([A|R], B) :- soma(R, C), B is C + A.

sucessor(e([var(C, D, _)| R], E), e(R, [var(C, D, CX)| E])) :- member(CX, D).

forCheck(e(Lni,[var(N,D,V)|Li]), e(Lnii,[var(N,D,V)|Li])) :-  corta(V,Lni,Lnii).

corta(_,[],[]).
corta(V,[var(N,D,_)|Li], [var(N,D1,_)|Lii]) :- delete(D,V,D1), corta(V,Li,Lii).

esc(L) :- write(L).

:- dynamic(nos/1).

nos(0).

inc:- retract(nos(N)), N1 is N+1, asserta(nos(N1)).
  
p:- estado_inicial(E0), back(E0,A),  esc(A).
p_fc:- estado_inicial(E0), forCheck(E0,A),  esc(A).

back(e([],A),A).
back(E,Sol) :-
    sucessor(E,E1), inc, verifica_restricoes(E1),
    forCheck(E1,E2), back(E2,Sol).
