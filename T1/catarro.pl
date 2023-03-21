:- dynamic(visitadas/1).

% indica o tamanho do tabuleiro
tamanho(7).

% verifica se a sala ja foi visitada, e caso nao tenha sido, marca como tal
nao_visitadas(X):-
    \+ visitadas(X),
    asserta(visitadas(X)).

% tuplo com as coordenadas onde o agente se encontra
estado_inicial((2,1)).
visitadas((2,1)).
% tuplo com as coordenadas onde o agente pretende chegar
estado_final((5,7)).

% portas bloqueadas entre salas
% bloqueada(sala_1, sala_2).
bloqueada((1,5), (1,6)).
bloqueada((2,6), (1,6)).
bloqueada((1,7), (1,6)).

bloqueada((2,7), (3,7)).
bloqueada((4,7), (3,7)).

bloqueada((2,6), (3,6)).
bloqueada((4,6), (3,6)).
bloqueada((3,5), (3,6)).

bloqueada((3,2), (4,2)).
bloqueada((4,1), (4,2)).
bloqueada((5,2), (4,2)).

bloqueada((3,3), (4,3)).
bloqueada((5,3), (4,3)).

bloqueada((3,4), (4,4)).
bloqueada((5,4), (4,4)).
bloqueada((4,5), (4,4)).

bloqueada((7,5), (7,6)).
bloqueada((6,6), (7,6)).
bloqueada((7,7), (7,6)).


% Regra que utiliza as operacoes que o agente pode usar
% op(estado_atual, operacao, estado_seguinte, custo).

op((X, Y), direita, (W, Y), 1):-
    tamanho(T),
    X < T,
    W is X+1,
    \+ bloqueada((X, Y), (W, Y)),
    nao_visitadas((W, Y)).

op((X, Y), cima, (X, Z), 1):- 
    tamanho(T),
    Y < T,
    Z is Y+1,
    \+ bloqueada((X, Y), (X,Z)),
    nao_visitadas((X,Z)).

op((X, Y), baixo, (X, Z), 1):-
    Y > 1,
    Z is Y-1,
    \+ bloqueada((X, Y), (X,Z)),
    nao_visitadas((X,Z)).

op((X, Y), esquerda, (W, Y), 1):-
    X > 1,
    W is X-1,
    \+ bloqueada((X, Y), (W, Y)),
    nao_visitadas((W, Y)).

% Predicado que servirá para contar os estados visitados e em memória
:- dynamic(max_em_memoria/1).
:- dynamic(visitados/1).

% Pesquisa em Profundidade
pesquisa_profundidade([no(E,Pai,Op,C,P)|_],no(E,Pai,Op,C,P)) :- 
    retract(visitados(X)),
    Y is X + 1,
    asserta(visitados(Y)), 
	estado_final(E).
pesquisa_profundidade([E|R],Sol):- 
	expande(E,Lseg),
    insere_inicio(Lseg,R,LFinal),
    retract(max_em_memoria(X)),
    length(LFinal, L),
    max(X, L, Z),
    asserta(max_em_memoria(Z)),
    pesquisa_profundidade(LFinal,Sol).

expande(no(E,Pai,Op,C,P),L):- 
	findall(no(En,no(E,Pai,Op,C,P), Opn, Cnn, P1), (op(E,Opn,En,Cn), P1 is P+1, Cnn is Cn+C), L).

pesquisa_p:-
    asserta(max_em_memoria(0)),
    asserta(visitados(0)),
    estado_inicial(S0),
	pesquisa_profundidade([no(S0,[],[],0,0)], S), nl,
    write(S), nl,
    write('Visitados: ' ),
    retract(visitados(Y)),
    write(Y), nl,
    write('Em Memoria: ' ),
    retract(max_em_memoria(X)),
    write(X).
    
insere_inicio(A,B,C) :- append(A, B, C).
insere_fim(A,B,C) :- append(B, A, C).

% Funcao que retorna o maximo
max(A, A, A).
max(A, B, C):-
    A > B,
    C is A.
max(A, B, C):-
    A < B,
    C is B.
