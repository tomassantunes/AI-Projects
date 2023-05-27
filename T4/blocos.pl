accao(andar(C1, C2), [robot_na_casa(C1), vizinho(C1, C2)], [robot_na_casa(C2)], [robot_na_casa(C1)]) :- member(C1, [0,1,2,3]), member(C2, [0,1,2,3]), C1\=C2.
accao(apanhar(B,C), [mao_livre, bloco_na_casa(B,C), robot_na_casa(C)], [na_mao(B), casa_livre(C)], [mao_livre, bloco_na_casa(B,C)]) :- member(B, [a,b,c]), member(C, [0,1,2,3]).
accao(largar(B,C), [na_mao(B), robot_na_casa(C), casa_livre(C)], [bloco_na_casa(B,C), mao_livre], [na_mao(B), casa_livre(C)]) :- member(B, [a,b,c]), member(C, [0,1,2,3]).

%estado_inicial([vizinho(0,1), vizinho(1,0), vizinho(1,2), vizinho(2,1), vizinho(2,3), vizinho(3,2),
%                mao_livre, robot_na_casa(0), bloco_na_casa(a,0), bloco_na_casa(b,1), bloco_na_casa(c,2), casa_livre(3)]).
%
%estado_final([vizinho(0,1), vizinho(1,0), vizinho(1,2), vizinho(2,1), vizinho(2,3), vizinho(3,2),
%              robot_na_casa(0), bloco_na_casa(c,1), bloco_na_casa(a,2), bloco_na_casa(b, 3), casa_livre(0)]).

%estado_inicial([vizinho(1,0), vizinho(2,1), vizinho(3,2), vizinho(0,1), vizinho(1,2), vizinho(3,2),
%                mao_livre, robot_na_casa(3), bloco_na_casa(a,1), bloco_na_casa(b,2), bloco_na_casa(c,3), casa_livre(0)]).
%              
%estado_final([vizinho(1,0), vizinho(2,1), vizinho(3,2), vizinho(0,1), vizinho(1,2), vizinho(3,2),
%              mao_livre, robot_na_casa(1), bloco_na_casa(a,0), bloco_na_casa(b,2), bloco_na_casa(c,3), casa_livre(1)]).

estado_inicial([vizinho(0,1), vizinho(1,0), vizinho(1,2), vizinho(2,1), 
                vizinho(2,3), vizinho(3,2), mao_livre, robot_na_casa(0), 
                bloco_na_casa(a,0), bloco_na_casa(b,1), bloco_na_casa(c,2), casa_livre(3)]).

estado_final([vizinho(0,1), vizinho(1,0), vizinho(1,2), vizinho(2,1),
              vizinho(2,3), vizinho(3,2), mao_livre, robot_na_casa(0),
              bloco_na_casa(c,1), bloco_na_casa(a,2), bloco_na_casa(b, 3), casa_livre(0)]).