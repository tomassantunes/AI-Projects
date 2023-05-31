% cada posicao pode ter "x", "o" ou "v" (vazio)
%estado_inicial([[v,v,v,v,v],[v,v,v,v,v],[v,v,v,v,v],[v,v,v,v,v]]).

estado_inicial(e([[v,v,v,o,o],
                  [v,v,v,x,x],
                  [o,x,o,o,x],
                  [o,x,x,o,o]], x)).

terminal(G) :- colunas(G).
terminal(G) :- linhas(G).
terminal(G) :- diagonais(G).
terminal(G) :- cheio(G).

linhas(e([[X,X,X,_,_],_,_,_],X)) :- X \= v.
linhas(e([[_,X,X,X,_],_,_,_],X)) :- X \= v.
linhas(e([[_,_,X,X,X],_,_,_],X)) :- X \= v.

linhas(e([_,[X,X,X,_,_],_,_],X)) :- X \= v.
linhas(e([_,[_,X,X,X,_],_,_],X)) :- X \= v.
linhas(e([_,[_,_,X,X,X],_,_],X)) :- X \= v.

linhas(e([_,_,[X,X,X,_,_],_],X)) :- X \= v.
linhas(e([_,_,[_,X,X,X,_],_],X)) :- X \= v.
linhas(e([_,_,[_,_,X,X,X],_],X)) :- X \= v.

linhas(e([_,_,_,[X,X,X,_,_]],X)) :- X \= v.
linhas(e([_,_,_,[_,X,X,X,_]],X)) :- X \= v.
linhas(e([_,_,_,[_,_,X,X,X]],X)) :- X \= v.

colunas(e([[X,_,_,_,_],[X,_,_,_,_],[X,_,_,_,_], [_,_,_,_,_]],X)) :- X \= v.
colunas(e([[_,_,_,_,_],[X,_,_,_,_],[X,_,_,_,_], [X,_,_,_,_]],X)) :- X \= v.

colunas(e([[_,X,_,_,_],[_,X,_,_,_],[_,X,_,_,_], [_,_,_,_,_]],X)) :- X \= v.
colunas(e([[_,_,_,_,_],[_,X,_,_,_],[_,X,_,_,_], [_,X,_,_,_]],X)) :- X \= v.

colunas(e([[_,_,X,_,_],[_,_,X,_,_],[_,_,X,_,_], [_,_,_,_,_]],X)) :- X \= v.
colunas(e([[_,_,_,_,_],[_,_,X,_,_],[_,_,X,_,_], [_,_,X,_,_]],X)) :- X \= v.

colunas(e([[_,_,_,X,_],[_,_,_,X,_],[_,_,_,X,_], [_,_,_,_,_]],X)) :- X \= v.
colunas(e([[_,_,_,_,_],[_,_,_,X,_],[_,_,_,X,_], [_,_,_,X,_]],X)) :- X \= v.

colunas(([[_,_,_,_,X],[_,_,_,_,X],[_,_,_,_,X], [_,_,_,_,_]],X)) :- X \= v.
colunas(([[_,_,_,_,_],[_,_,_,_,X],[_,_,_,_,X], [_,_,_,_,X]],X)) :- X \= v.


diagonais(e([[X,_,_,_,_],[_,X,_,_,_],[_,_,X,_,_], [_,_,_,_,_]],X)) :- X \= v.
diagonais(e([[_,_,_,_,_],[_,X,_,_,_],[_,_,X,_,_], [_,_,_,X,_]],X)) :- X \= v.
diagonais(e([[_,_,_,_,_],[X,_,_,_,_],[_,X,_,_,_], [_,_,X,_,_]],X)) :- X \= v.
diagonais(e([[_,X,_,_,_],[_,_,X,_,_],[_,_,_,X,_], [_,_,_,_,_]],X)) :- X \= v.
diagonais(e([[_,_,_,_,_],[_,_,X,_,_],[_,_,_,X,_], [_,_,_,_,X]],X)) :- X \= v.
diagonais(e([[_,_,X,_,_],[_,_,_,X,_],[_,_,_,_,X], [_,_,_,_,_]],X)) :- X \= v.

diagonais(e([[_,_,X,_,_],[_,X,_,_,_],[X,_,_,_,_], [_,_,_,_,_]],X)) :- X \= v.
diagonais(e([[_,_,_,_,_],[_,_,X,_,_],[_,X,_,_,_], [X,_,_,_,_]],X)) :- X \= v.
diagonais(e([[_,_,_,X,_],[_,_,X,_,_],[_,X,_,_,_], [_,_,_,_,_]],X)) :- X \= v.
diagonais(e([[_,_,_,_,X],[_,_,_,X,_],[_,_,X,_,_], [_,_,_,_,_]],X)) :- X \= v.
diagonais(e([[_,_,_,_,_],[_,_,_,X,_],[_,_,X,_,_], [_,X,_,_,_]],X)) :- X \= v.
diagonais(e([[_,_,_,_,_],[_,_,_,_,X],[_,_,_,X,_], [_,_,X,_,_]],X)) :- X \= v.

cheio(e([C1,C2,C3,C4],_)) :-
    \+member(v, C1),
    \+member(v, C2),
    \+member(v, C3),
    \+member(v, C4).

%função de utilidade, retorna o valor dos estados terminais, 1 ganha -1 perde
valor(e(G, o), 1, _) :- linhas(e(G, o)); colunas(e(G, o)); diagonais(e(G, o)).
valor(e(G, x), -1, _) :- linhas(e(G, x)); colunas(e(G, x)); diagonais(e(G, x)).
valor(G, 0, _) :- verifica_continua(G).

verifica_continua(G) :-
    \+ linhas(G), \+ colunas(G), \+ diagonais(G).

% oper(estado,jogador,jogada,estado seguinte)
inv(x,o).
inv(o,x).
op1(e(Board, J), joga(C, J), e(Board1, J1)):- fd_domain(C, [1,2,3,4]),
    jogada_valida(e(Board, J), jogada(C, J), e(Board1, J1)).

jogada_valida(e([C1,C2,C3,C4],J), jogada(1, J), e([C11,C2,C3,C4],J1)):-
    coloca1(C1, J, C11), inv(J, J1).
jogada_valida(e([C1,C2,C3,C4],J), jogada(2, J), e([C1,C22,C3,C4],J2)):-
    coloca1(C2, J, C22), inv(J, J2).
jogada_valida(e([C1,C2,C3,C4],J), jogada(3, J), e([C1,C2,C33,C4],J3)):-
    coloca1(C3, J, C33), inv(J, J3).
jogada_valida(e([C1,C2,C3,C4],J), jogada(4, J), e([C1,C2,C3,C44],J4)):-
    coloca1(C4, J, C44), inv(J, J4).

coloca1([v, P | Resto], J, [J, P | Resto]) :- P \= v.
coloca1([v, v, P | Resto], J, [v, J, P | Resto]) :- P \= v.
coloca1([v, v, v, P | Resto], J, [v, v, J, P | Resto]) :- P \= v.
coloca1([v, v, v, v, P], J, [v, v, v, J, P]) :- P \= v.
coloca1([v, v, v, v, v], J, [v, v, v, v, J]).
