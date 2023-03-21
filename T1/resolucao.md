
## a)
```prolog
estado_inicial((1, 6), [(0,2), (1,0), (1, 2), (1, 6), (3, 3), (3, 4), (3, 5)]).
estado_final((0, 4), _).

verificar_limite(_, []).
verificar_limite(A, [B|L]) :- \+ A = B, verificar_limite(A, L).

op(((X, Y), LISTA), sobe, ((A, B), LISTA), 1) :-
    tamanho(T), Y < T, A is X, B is Y + 1, verificar_limite((A, B), LISTA).

op(((X, Y), LISTA), desce, ((A, B), LISTA), 1) :-
    Y > 0, A is X, B is Y - 1, verificar_limite((A, B), LISTA).

op(((X, Y), LISTA), direita, ((A, B), LISTA), 1) :-
    tamanho(T), Y < T, A is X + 1, B is Y, verificar_limite((A, B), LISTA).

op(((X, Y), LISTA), esquerda, ((A, B), LISTA), 1) :-
    Y > 0, A is X - 1, B is Y, verificar_limite((A, B), LISTA).
```

## b)
```prolog
:- dynamic(maxNL/1).
:- dynamic(nos/1).

maxNL(0).
nos(0).

limpa:-retractall(maxNL(A)),
    retractall(nos(A)), asserta(maxNL(0)),
    asserta(nos(0)).

inc:- retract(nos(N)), N1 is N+1, asserta(nos(N1)).

actmax(N):- maxNL(N1), N1 >= N,!.
actmax(N):- retract(maxNL(_N1)), asserta(maxNL(N)).

pesquisa(Alg):-
    limpa,
    estado_inicial(S0),
    pesquisa(Alg,[no(S0,[],[],0,0)],Solucao),
    nos(Ns),maxNL(NL),
    write(nos(visitados(Ns),lista(NL))),
    limpa,
    escreve_seq_solucao(Solucao).

pesquisa(largura,Ln,Sol):- pesquisa_largura(Ln,Sol).

pesquisa_largura([no(E,Pai,Op,C,P)|_],no(E,Pai,Op,C,P)):- estado_final(E), inc.
pesquisa_largura([E|R],Sol):- inc,
                              expande(E,Lseg),
                              insere_fim(Lseg,R,Resto),
                              length(Resto,N), actmax(N),
                              pesquisa_largura(Resto,Sol).

expande(no(E,Pai,Op,C,P),L):-
            findall(no(En,no(E,Pai,Op,C,P),Opn,Cnn,P1),
            (op(E,Opn,En,Cn),P1 is P+1, Cnn is Cn+C), L).

insere_fim([],L,L).
insere_fim(L,[],L).
insere_fim(R,[A|S],[A|L]):- insere_fim(R,S,L).

escreve_seq_solucao(no(E,Pai,Op,Custo,Prof)):- write(custo(Custo)),nl,
                                          write(profundidade(Prof)),nl,
                                          escreve_seq_accoes(no(E,Pai,Op,_,_)).

escreve_seq_accoes([]).
escreve_seq_accoes(no(E,Pai,Op,_,_)):- escreve_seq_accoes(Pai), write(e(Op,E)),nl.
```

## c)
+ i. 39.
+ ii. 6.
