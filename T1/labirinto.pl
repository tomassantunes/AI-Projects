:- dynamic(percorridos/1).

% 0 - 6
tamanho(6).

% a)
estado_inicial((6, 1)).
estado_final((0, 4)).

bloqueado((0,2)).
bloqueado((1,0)).
bloqueado((1,2)).
bloqueado((1,6)).
bloqueado((3,3)).
bloqueado((3,4)).
bloqueado((3,5)).

percorridos((3,1)).

nao_percorridos(A) :- 
    \+ percorridos(A),
    assertz(percorridos(A)).

op((X, Y), sobe, (X, B), 1) :-
    tamanho(T), Y < T, B is Y + 1, \+ bloqueado((X,B)), nao_percorridos((X,B)).

op((X, Y), desce, (X, B), 1) :-
    Y > 0, B is Y - 1, \+ bloqueado((X,B)), nao_percorridos((X,B)).

op((X, Y), direita, (A, Y), 1) :-
    tamanho(T), X < T, A is X + 1, \+ bloqueado((A,Y)), nao_percorridos((A,Y)).

op((X, Y), esquerda, (A, Y), 1) :-
    X > 0, A is X - 1, \+ bloqueado((A,Y)), nao_percorridos((A,Y)).

% b)
:- dynamic(max_memoria/1).
:- dynamic(nos/1).

max_memoria(0).
nos(0).

inc:- retract(nos(N)), N1 is N+1, assertz(nos(N1)).

actmax(N):- max_memoria(N1), N1 >= N,!.
actmax(N):- retract(max_memoria(_N1)), assertz(max_memoria(N)).

pesquisa_largura([no(E,Pai,Op,C,P)|_],no(E,Pai,Op,C,P)):- estado_final(E), inc.
pesquisa_largura([E|R],Sol):- 
    inc, expande(E,Lseg),
    insere_fim(Lseg,R,Resto),
    length(Resto,N), actmax(N),
    pesquisa_largura(Resto,Sol).

pesquisa_profundidade([no(E,Pai,Op,C,P)|_],no(E,Pai,Op,C,P)) :- estado_final(E), inc.
pesquisa_profundidade([E|R],Sol):- 
	inc, expande(E,Lseg),
    insere_inicio(Lseg,R,LFinal),
    length(LFinal, L), actmax(L),
    pesquisa_profundidade(LFinal,Sol).

expande(no(E,Pai,Op,C,P),L):-
            findall(no(En,no(E,Pai,Op,C,P),Opn,Cnn,P1),
            (op(E,Opn,En,Cn),P1 is P+1, Cnn is Cn+C), L).

escreve_seq_solucao(no(E,Pai,Op,Custo,Prof)):- write(custo(Custo)),nl,
                                          write(profundidade(Prof)),nl,
                                          escreve_seq_accoes(no(E,Pai,Op,_,_)).

escreve_seq_accoes([]).
escreve_seq_accoes(no(E,Pai,Op,_,_)):- escreve_seq_accoes(Pai), write(e(Op,E)),nl.

pesquisa_l :-
    estado_inicial(S0),
    pesquisa_largura([no(S0,[],[],0,0)], Sol), nl,
    max_memoria(M), write(lista(M)), nl,
    escreve_seq_solucao(Sol), nl,
    write('Estados visitados: '),
    retract(nos(X)),
    write(X), nl,
    write('Máximo em memória: '),
    retract(max_memoria(Y)),
    write(Y).

pesquisa_p:-
    estado_inicial(S0),
	pesquisa_profundidade([no(S0,[],[],0,0)], Sol), nl,
    max_memoria(M), write(lista(M)), nl,
    escreve_seq_solucao(Sol), nl,
    write('Estados visitados: '),
    retract(nos(X)),
    write(X), nl,
    write('Máximo em memória: '),
    retract(max_memoria(Y)),
    write(Y).

insere_fim([], L, L).
insere_fim(L, [], L).
insere_fim(R, [A|S], [A|L]) :- insere_fim(R, S, L).
insere_inicio(A,B,C) :- append(A, B, C).

max(X, X, X).
max(X, Y, Z) :-
    X > Y, Z is X;
    Y > X, Z is Y.
