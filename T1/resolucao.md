## 1.
### a)
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

### b)
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

### c)
* i)
    * 15
* ii)
    * 11

### d)
As duas heurísticas que propomos são: Distância de Manhattan e a Distância Euclidiana.

### e)
``` prolog
h1((X, Y), R):- 
    estado_final(PF),
    distancia_manhattan((X, Y), PF, R).

h2(P1, D) :- 
    estado_final(PF),  
    euclidean_distance(P1, PF, D).

% distância de Manhattan
distancia_manhattan((X, Y), (W, Z), D) :-
    X1 is X-W,
    abs(X1, AX),
    Y1 is Y-Z,
    abs(Y1, AY),
    D is AX+AY.

euclidean_distance((X1, Y1), (X2, Y2), R) :- R is sqrt((X2-X1)^2 + (Y2-Y1)^2).

abs(X, X):- X > 0.
abs(X, R):- R is -X.

pesquisa_g([no(E,Pai,Op,C,HC,P)|_],no(E,Pai,Op,C,HC,P)) :- estado_final(E).
pesquisa_g([E|R],Sol) :- 
    inc, expande_g(E,Lseg),
    insere_ordenado(Lseg,R,Resto), length(Resto,N), actmax(N),
    pesquisa_g(Resto,Sol).

expande_g(no(E,Pai,Op,C,HC,P),L) :- 
    findall(no(En,no(E,Pai,Op,C,HC,P),Opn,Cnn,H,P1),
	(op(E,Opn,En,Cn), P1 is P+1, Cnn is Cn+C, h1(En,H)), L).

insere_ordenado([],L,L).
insere_ordenado([A|T], L, LF) :- 
	ins_ord(A,L,L1), insere_ordenado(T, L1, LF).

ins_ord(E, [], [E]).
ins_ord(no(E,Pai,Op,C,CH,P), [no(E1,Pai1,Op1,C1,CH1,P1)|T], [no(E,Pai,Op,C,CH,P),no(E1,Pai1,Op1,C1,CH1,P1)|T]) :- CH =< CH1.
ins_ord(no(E,Pai,Op,C,CH,P), [no(E1,Pai1,Op1,C1,CH1,P1)|T], [no(E1,Pai1,Op1,C1,CH1,P1)|T1]) :- ins_ord(no(E,Pai,Op,C,CH,P), T, T1).	

escreve_seq_solucao_g(no(E,Pai,Op,Custo,_HC,Prof)) :-
    write(custo(Custo)),nl,
    write(profundidade(Prof)),nl,
    escreve_seq_accoes_a(no(E,Pai,Op,_,_,_)).

escreve_seq_accoes_g([]).
escreve_seq_accoes_g(no(E,Pai,Op,_,_,_)) :- escreve_seq_accoes_a(Pai), write(e(Op,E)),nl.

pesquisa_g :-
    assertz(max_memoria(0)), assertz(nos(0)),
    estado_inicial(S0),
	pesquisa_g([no(S0,[],[],0,0,0)], Sol), nl,
    escreve_seq_solucao_g(Sol), nl,
    write('Estados visitados: '),
    retract(nos(X)),
    write(X), nl,
    write('Máximo em memória: '),
    retract(max_memoria(Y)),
    write(Y).
```
### f)

#### Distância de Manhattan
* i)
    * 39
* ii)
    * 53

#### Distância Euclidiana
* i)
    * 9
* ii)
    * 13

## 2.
### a)
```prolog
tamanho(6).

estado_inicial((4, 6, 4, 5)).
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
```

### b)
```prolog
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
```

### c)
* i)
    * 6
* ii)
    * 12

### d)
As duas heurísticas que propomos são: Distância de Manhattan e a Distância Euclidiana.

### e)

