%:- discontiguous accao/4.
% Estados
% estado_inicial([vizinhos(0,1), vizinhos(1,0), vizinhos(1,2), vizinhos(2,1), vizinhos(2,3), vizinhos(3,2),
%                 mao_livre, robot_na_casa(0), bloco_na_casa(a,0), bloco_na_casa(b,1), bloco_na_casa(c,2), casa_livre(3)]).

% estado_final([vizinhos(0,1), vizinhos(1,0), vizinhos(1,2), vizinhos(2,1), vizinhos(2,3), vizinhos(3,2),
%               robot_na_casa(0), casa_livre(0), bloco_na_casa(c,1), bloco_na_casa(a,2), bloco_na_casa(b, 3)]).

% estado_inicial([a_direita(1,0), a_direita(2,1), a_direita(3,2), a_esquerda(0,1), a_esquerda(1,2), a_esquerda(3,2),
%              mao_livre, robot_na_casa(0), bloco_na_casa(a,1), bloco_na_casa(b,2), bloco_na_casa(c,3), casa_livre(0)]).
%              
% estado_final([a_direita(1,0), a_direita(2,1), a_direita(3,2), a_esquerda(0,1), a_esquerda(1,2), a_esquerda(3,2),
%              mao_livre, robot_na_casa(1), bloco_na_casa(a,0), bloco_na_casa(b,2), bloco_na_casa(c,3), casa_livre(1)]).

% Acções
% accao(a1,Precond,AddList,DeleteList).
% Problema: robot nao anda depois de largar
%accao(andar(C1,C2), [robot_na_casa(C1), vizinhos(C1,C2)], [robot_na_casa(C2)], [robot_na_casa(C1)]):- member(C1, [0,1,2,3]), member(C2, [0,1,2,3]), C1\=C2.

% accao(andar_para_direita(C1,C2), [robot_na_casa(C1), a_direita(C2,C1)], [robot_na_casa(C2)], [robot_na_casa(C1)]):- member(C1, [0,1,2,3]), member(C2, [0,1,2,3]), C1\=C2.
% accao(andar_para_esquerda(C1,C2),[robot_na_casa(C1), a_esquerda(C2,C1)], [robot_na_casa(C2)], [robot_na_casa(C1)]):- member(C1, [0,1,2,3]), member(C2, [0,1,2,3]), C1\=C2.
%
% estado_inicial([vizinhos(0,1), vizinhos(1,0), vizinhos(1,2), vizinhos(2,1), vizinhos(2,3), vizinhos(3,2),
%             mao_livre, robot_na_casa(1), bloco_na_casa(a,1), bloco_na_casa(b,2), bloco_na_casa(c,3), casa_livre(0)]).
%             
% estado_final([vizinhos(0,1), vizinhos(1,0), vizinhos(1,2), vizinhos(2,1), vizinhos(2,3), vizinhos(3,2),
%             mao_livre, robot_na_casa(1), bloco_na_casa(a, 0), bloco_na_casa(b,2), bloco_na_casa(c,3), casa_livre(1)]).

% estado_inicial([vizinho(0,1), vizinho(1,0), vizinho(1,2), vizinho(2,1), vizinho(2,3), vizinho(3,2),
%     robot_na_casa(0), mao_livre, na_casa(a,0), na_casa(b, 1), na_casa(c,2), casa_vazia(3)]).
%estado_inicial([vizinho(0,1), vizinho(1,0), vizinho(1,2), vizinho(2,1), vizinho(2,3), vizinho(3,2),
%    robot_na_casa(2), na_mao(b), na_casa(a,0), casa_vazia(1), na_casa(c,2), casa_vazia(3)]).

% estado_final([vizinho(0,1), vizinho(1,0), vizinho(1,2), vizinho(2,1), vizinho(2,3), vizinho(3,2),
%     robot_na_casa(0), mao_livre, casa_vazia(0), na_casa(c,1), na_casa(a, 2), na_casa(b,3)]).
%estado_final([vizinho(0,1), vizinho(1,0), vizinho(1,2), vizinho(2,1), vizinho(2,3), vizinho(3,2),
%    robot_na_casa(3), mao_livre, na_casa(a,1), casa_vazia(0), na_casa(c, 2), na_casa(b,3)]).

%accao(agarra(B,C), [mao_livre, robot_na_casa(C), na_casa(B,C)], [na_mao(B), casa_vazia(C)], [mao_livre, na_casa(B,C)]) :- member(C,[0,1,2,3]), member(B,[a,b,c]).
%accao(larga(B,C), [na_mao(B), casa_vazia(C), robot_na_casa(C)], [na_casa(B,C), mao_livre], [na_mao(B), casa_vazia(C)]) :- member(C,[0,1,2,3]), member(B,[a,b,c]).
%accao(move(A,B), [vizinho(A,B), robot_na_casa(A)], [robot_na_casa(B)], [robot_na_casa(A)]) :- member(A,[0,1,2,3]), member(B,[0,1,2,3]), A\=B.

accao(andar(C1, C2), [robot_na_casa(C1), vizinhos(C1, C2)], [robot_na_casa(C2)], [robot_na_casa(C1)]) :- member(C1, [0,1,2,3]), member(C2, [0,1,2,3]), C1\=C2.
accao(apanhar(B,C), [mao_livre, bloco_na_casa(B,C), robot_na_casa(C)], [na_mao(B), casa_livre(C)], [mao_livre, bloco_na_casa(B,C)]) :- member(B, [a,b,c]), member(C, [0,1,2,3]).
accao(largar(B,C), [na_mao(B), robot_na_casa(C), casa_livre(C)], [bloco_na_casa(B,C), mao_livre], [na_mao(B), casa_livre(C)]) :- member(B, [a,b,c]), member(C, [0,1,2,3]).

estado_inicial([vizinho(1,0), vizinho(2,1), vizinho(3,2), vizinho(0,1), vizinho(1,2), vizinho(3,2),
              mao_livre, robot_na_casa(0), bloco_na_casa(a,1), bloco_na_casa(b,2), bloco_na_casa(c,3), casa_livre(0)]).
              
estado_final([vizinho(1,0), vizinho(2,1), vizinho(3,2), vizinho(0,1), vizinho(1,2), vizinho(3,2),
              mao_livre, robot_na_casa(0), bloco_na_casa(a,0), bloco_na_casa(b,2), bloco_na_casa(c,3), casa_livre(1)]).