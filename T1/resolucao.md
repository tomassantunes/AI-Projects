
## a)
```prolog
estado_inicial((1, 6), [(0,2), (1,0), (1, 2), (1, 6), (3, 3), (3, 4), (3, 5)]).
estado_final((0, 4), _).

verificar_limite(_, []).
verificar_limite(A, [B|L]) :- \+ A = B, verificar_limite(A, L).

op((X, Y), sobe, (X, B), 1) :-
    tamanho(T), Y < T, B is Y + 1, \+ bloqueado((X,B)), nao_percorridos((X,B)).

op((X, Y), esquerda, (A, Y), 1) :-
    X > 0, A is X - 1, \+ bloqueado((A,Y)), nao_percorridos((A,Y)).

op((X, Y), desce, (X, B), 1) :-
    Y > 0, B is Y - 1, \+ bloqueado((X,B)), nao_percorridos((X,B)).

op((X, Y), direita, (A, Y), 1) :-
    tamanho(T), X < T, A is X + 1, \+ bloqueado((A,Y)), nao_percorridos((A,Y)).

```

## b)
```prolog
:- dynamic(max_memoria/1).
:- dynamic(nos/1).

max_memoria(0).
nos(0).

inc:- retract(nos(N)), N1 is N+1, assertz(nos(N1)).

actmax(N):- max_memoria(N1), N1 >= N,!.
actmax(N):- retract(max_memoria(_N1)), assertz(max_memoria(N)).

pesquisa_profundidade([no(E,Pai,Op,C,P)|_],no(E,Pai,Op,C,P)) :- estado_final(E), inc.
pesquisa_profundidade([E|R],Sol):- 
	inc, expande(E,Lseg),
    insere_inicio(Lseg,R,LFinal),
    length(LFinal, L), actmax(L),
    pesquisa_profundidade(LFinal,Sol).

expande(no(E,Pai,Op,C,P),L):-
            findall(no(En,no(E,Pai,Op,C,P),Opn,Cnn,P1),
            (op(E,Opn,En,Cn),P1 is P+1, Cnn is Cn+C), L).

escreve_seq_solucao(no(E,Pai,Op,Custo,Prof)):- 
    write(custo(Custo)),nl,
    write(profundidade(Prof)),nl,
    escreve_seq_accoes(no(E,Pai,Op,_,_)).

escreve_seq_accoes([]).
escreve_seq_accoes(no(E,Pai,Op,_,_)):- escreve_seq_accoes(Pai), write(e(Op,E)),nl.

insere_inicio(X, Y, L) :- append(X, Y, L).
insere_fim(X, Y, L) :- append(Y, X, L).

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
```

## c)
+ i. 15.
+ ii. 11.

## d)
As duas heurísticas que propomos são: Distância de Manhattan e a Distância Euclidiana.

## e)
