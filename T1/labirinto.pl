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
    tamanho(T), Y < T, B is Y + 1, nao_percorridos((X,B)), \+ bloqueado((X,B)).

op((X, Y), desce, (X, B), 1) :-
    Y > 0, B is Y - 1, nao_percorridos((X,B)), \+ bloqueado((X,B)).

op((X, Y), direita, (A, Y), 1) :-
    tamanho(T), X < T, A is X + 1, nao_percorridos((A,Y)), \+ bloqueado((A,Y)).

op((X, Y), esquerda, (A, Y), 1) :-
    X > 0, A is X - 1, nao_percorridos((A,Y)), \+ bloqueado((A,Y)).

% b)
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
