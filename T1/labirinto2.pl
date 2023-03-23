tamanho(6).

estado_inicial((1, 6, 1, 5)).
estado_final((_, _, 4, 0)).

bloqueado((0,1)).
bloqueado((2,0)).
bloqueado((2,1)).
bloqueado((3,3)).
bloqueado((3,4)).
bloqueado((3,5)).
bloqueado((6,1)).

lim(X, Y) :- 
    tamanho(T),
    X =< T,
    X >= 0,
    Y =< T,
    Y >= 0.

iguais(A, A).

op((X, Y, A, B), cima, (X, Y1, A, B1), 1) :-
    Y1 is Y - 1,
    (iguais((X, Y1), (A, B)) -> (
            B1 is B - 1,
            lim(A, Y1),
            \+ bloqueado((A, B1))
        );
        (
            B1 is B,
            lim(X, Y1),
            \+ bloqueado((X, Y1))
        )
    ).

op((X, Y, A, B), direita, (X1, Y, A1, B), 1) :-
    X1 is X+1,
    (iguais((X1, Y), (A, B)) -> (
            A1 is A+1,
            lim(A1, B), lim(X1, Y),
            \+ bloqueado((A1, B))
        );
        (
            A1 is A,
            lim(X1, Y),
            \+ bloqueado((X1, Y))
        )
    ).

op((X, Y, A, B), baixo, (X, Y1, A, B1), 1) :-
    Y1 is Y+1,
    (iguais((X, Y1), (A, B)) -> (
            B1 is B+1,
            lim(A, B1), lim(X, Y1),
            \+ bloqueado((A, B1))
        );
        (
            B1 is B,
            lim(X, Y1),
            \+ bloqueado((X, Y1))
        )
    ).

op((X, Y, A, B), esquerda, (X1, Y, A1, B), 1) :-
    X1 is X-1,
    (iguais((X1, Y), (A, B)) -> (
            A1 is A-1,
            lim(A1, B), lim(X1, Y),
            \+ bloqueado((A1, B))
        );
        (
            A1 is A,
            lim(X1, Y),
            \+ bloqueado((X1, Y))
        )
    ).


% b)
:- dynamic(max_memoria/1).
:- dynamic(nos/1).

inc :- retract(nos(N)), N1 is N+1, assertz(nos(N1)).

actmax(N) :- max_memoria(N1), N1 >= N,!.
actmax(N) :- retract(max_memoria(_N1)), assertz(max_memoria(N)).

pesquisa_profundidade([no(E,Pai,Op,C,P)|_],no(E,Pai,Op,C,P)) :- estado_final(E), inc.
pesquisa_profundidade([E|R],Sol) :- 
    inc, expande(E,Lseg),
    insere_inicio(Lseg,R,LFinal),
    length(LFinal, L), actmax(L),
    pesquisa_profundidade(LFinal,Sol).

expande(no(E,Pai,Op,C,P),L) :-
    findall(no(En,no(E,Pai,Op,C,P),Opn,Cnn,P1),
    (op(E,Opn,En,Cn),P1 is P+1, Cnn is Cn+C), L).

escreve_seq_solucao(no(E,Pai,Op,Custo,Prof)) :- 
    write(custo(Custo)),nl,
    write(profundidade(Prof)),nl,
    escreve_seq_accoes(no(E,Pai,Op,_,_)).

escreve_seq_accoes([]).
escreve_seq_accoes(no(E,Pai,Op,_,_)) :- escreve_seq_accoes(Pai), write(e(Op,E)),nl.

insere_inicio(X, Y, L) :- append(X, Y, L).
insere_fim(X, Y, L) :- append(Y, X, L).

pesquisa_p :-
    assertz(max_memoria(0)), assertz(nos(0)),
    estado_inicial(S0),
    pesquisa_profundidade([no(S0,[],[],0,0)], Sol), nl,
    escreve_seq_solucao(Sol), nl,
    write('Estados visitados: '),
    retract(nos(X)),
    write(X), nl,
    write('Máximo em memória: '),
    retract(max_memoria(Y)),
    write(Y).

% e)
:- dynamic(percorridos/1).

nao_percorridos(A) :- 
    \+ percorridos(A).

h(h1, A, B) :- h1(A, B).
h(h2, A, B) :- h2(A, B).

h1(A, B) :- estado_final(E), distancia_manhattan(A, E, B).
h2(A, B) :- estado_final(E), euclidean_distance(A, E, B).

distancia_manhattan((_, _, X, Y), (_, _, W, Z), D) :-
    X1 is abs(X - W),
    Y1 is abs(Y - Z),
    D is X1+Y1.

euclidean_distance((_, _, X, Y), (_, _, A, B), R) :- R is sqrt((X-A)^2 + (Y-B)^2).

abs(X, X):- X > 0.
abs(X, R):- R is -X.

pesquisa_g(_, [no(E,Pai,Op,C,HC,P)|_], no(E,Pai,Op,C,HC,P)) :- estado_final(E), inc.
pesquisa_g(HEUR, [E|R], Sol) :- 
    inc, expande_g(HEUR, E,Lseg), assertz(percorridos(E)),
    insere_ordenado(Lseg,R,Resto), length(Resto,N), actmax(N),
    pesquisa_g(HEUR, Resto, Sol).

expande_g(HEUR, no(E,Pai,Op,C,HC,P), L) :- 
    findall(no(En,no(E,Pai,Op,C,HC,P),Opn,Cnn,H,P1),
    (op(E,Opn,En,Cn), nao_percorridos(no(En,_,_,_,_,_)), P1 is P+1, Cnn is Cn+C, h(HEUR, En,H)), L).

insere_ordenado([],L,L).
insere_ordenado([A|T], L, LF) :- 
    ins_ord(A,L,L1), insere_ordenado(T, L1, LF).

ins_ord(E, [], [E]).
ins_ord(no(E,Pai,Op,C,CH,P), [no(E1,Pai1,Op1,C1,CH1,P1)|T], [no(E,Pai,Op,C,CH,P),no(E1,Pai1,Op1,C1,CH1,P1)|T]) :- CH =< CH1.
ins_ord(no(E,Pai,Op,C,CH,P), [no(E1,Pai1,Op1,C1,CH1,P1)|T], [no(E1,Pai1,Op1,C1,CH1,P1)|T1]) :- ins_ord(no(E,Pai,Op,C,CH,P), T, T1).	

escreve_seq_solucao_g(no(E,Pai,Op,Custo,_HC,Prof)) :-
    write(custo(Custo)),nl,
    write(profundidade(Prof)),nl,
    escreve_seq_accoes_g(no(E,Pai,Op,_,_,_)).

escreve_seq_accoes_g([]).
escreve_seq_accoes_g(no(E,Pai,Op,_,_,_)) :- escreve_seq_accoes_g(Pai), write(e(Op,E)),nl.

pesquisa_g :-
    write('Pesquisa greedy com distância de Manhattan.'), nl,
    assertz(max_memoria(0)), assertz(nos(0)),
    estado_inicial(S0),
    pesquisa_g(h1, [no(S0,[],[],0,0,0)], Sol1), nl,
    escreve_seq_solucao_g(Sol1), nl,
    write('Estados visitados: '),
    retract(nos(X)),
    write(X), nl,
    write('Máximo em memória: '),
    retract(max_memoria(Y)),
    write(Y), nl,
    write('Pesquisa greedy com distância Euclidiana.'), nl,
    retractall(max_memoria(_)), retractall(percorridos(_)), retractall(nos(_)),
    assertz(max_memoria(0)), assertz(nos(0)),
    pesquisa_g(h2, [no(S0,[],[],0,0,0)], Sol2), nl,
    escreve_seq_solucao_g(Sol2), nl,
    write('Estados visitados: '),
    retract(nos(X)),
    write(X), nl,
    write('Máximo em memória: '),
    retract(max_memoria(Y)),
    write(Y).
