size(3).

estado_inicial(e([
    v((1,1),[1,2,3,4,5,6,7,8,9],_), v((1,2),[1,2,3,4,5,6,7,8,9],_),v((1,3),[1,2,3,4,5,6,7,8,9],_),
    v((2,1),[1,2,3,4,5,6,7,8,9],_), v((2,2),[1,2,3,4,5,6,7,8,9],5), v((2,3),[1,2,3,4,5,6,7,8,9],_)
],[ v((3,1),[1,2,3,4,5,6,7,8,9],6), v((3,2),[1,2,3,4,5,6,7,8,9],1), v((3,3),[1,2,3,4,5,6,7,8,9],8) ])).

sucessor(e([v(N,D,_)|R],E),e(R,[v(N,D,V)|E])):- member(V,D).

ver_restricoes(e(R,[v((X,Y),D,V)|F])) :- findall(Vs, member(v((_,_),_,Vs),F), L), diferentes(L),size(K), checkd(v((X,Y),D,V), 0, K), diagonal2([v((X,Y),D,V)|F]),
                            linha(F), coluna(F), diagonal(F).

ver_restricoes(e(R,[v((X,Y),D,V)|F])) :- findall(Vs, member(v((_,_),_,Vs),F), L), diferentes([V|L]), size(K), \+ checkd(v((X,Y),D,V), 0, K),
                            linha([v((X,Y),D,V)|F]), coluna([v((X,Y),D,V)|F]), diagonal([v((X,Y),D,V)|F]).

linha([v((X,Y),D,V)|F]) :- findall(Vi, member(v((X,_),_,Vi), F), L), diferentes([V|L]), length([V|L], Tamanho), size(Sz),Tamanho =:= Sz, soma([V|L], S), tsoma(K), Z is truncate(K), S == Z.
linha([v((X,Y),D,V)|F]) :- findall(Vi, member(v((X,_),_,Vi), F), L), diferentes([V|L]), length([V|L], Tamanho), size(Sz), Tamanho \= Sz.

coluna([v((X,Y),D,V)|F]) :- findall(Vi, member(v((_,Y),_,Vi), F), L), diferentes([V|L]), length([V|L], Tamanho), size(Sz), Tamanho =:= Sz, soma([V|L], S), tsoma(K), Z is truncate(K), S == Z.
coluna([v((X,Y),D,V)|F]) :- findall(Vi, member(v((_,Y),_,Vi), F), L), diferentes([V|L]), length([V|L], Tamanho), size(Sz),Tamanho \= Sz.

diagonal([v((X,Y),D,V)|F]) :- X \= Y.
diagonal([v((X,Y),D,V)|F]) :- X =:= Y, findall(Vs, member(v((O,O),_,Vs),F), L), length([V|L], Tamanho), size(Sz), Tamanho =:= Sz, soma([V|L], S), tsoma(K), Z is truncate(K), S == Z.
diagonal([v((X,Y),D,V)|F]) :- X =:= Y, findall(Vs, member(v((O,O),_,Vs),F), L), length([V|L], Tamanho), size(Sz), Tamanho \= Sz.


checkd(v((I,J),D,V), X, K) :- Xi is X+1, Xi =< K,  Y is K - Xi + 1, I =:= Xi,J =:= Y,!. 
checkd(v((I,J),D,V), X, K) :- Xi is X+1, Xi =< K,  Y is K - Xi + 1, (I \= Xi; J \= Y), checkd(v((I,J),D,V), Xi, K),!.

diagonal2(F) :- size(K), emptyL(B), diagonal2(F, 0, K, B).

diagonal2([v((I,J),D,V)|F], K, K, B) :- length(B, Tamanho), size(Sz), Tamanho =:= Sz, soma(B, S), tsoma(T), Z is truncate(T), S == Z.
diagonal2([v((I,J),D,V)|F], K, K, B) :- length(B, Tamanho), size(Sz), Tamanho \= Sz.

diagonal2([v((I,J),D,V)|F], X, K, B) :- Xi is X+1, Xi =< K,  Y is K - Xi + 1, member(v((Xi,Y),_,Vi), [v((I,J),D,V)|F]), diagonal2([v((I,J),D,V)|F], Xi, K, [Vi|B]).
diagonal2([v((I,J),D,V)|F], X, K, B) :- Xi is X+1, Xi =< K,  Y is K - Xi + 1,\+ member(v((Xi,Y),_,Vi), [v((I,J),D,V)|F]), diagonal2([v((I,J),D,V)|F], Xi, K, B).

emptyL([]).

tsoma(K) :- size(X), K is X*(X*X+1)/2.

soma(L, S) :- soma(L, 0, S).
soma([], A, S) :- S is A.
soma([X|L], A, S) :- A1 is X + A, soma(L, A1, S).

diferentes([]).
diferentes([A|F]) :- \+member(A,F), diferentes(F).


esc(L) :- sort(L, L1), esc1(L1,1).
esc1([], _).
esc1([v((_,_),_,X)|L], P) :- size(K), P =:= K, write(X), nl, esc1(L, 1).
esc1([v((_,_),_,X)|L], P) :- write(X), write(' '), Pi is P+1, esc1(L, Pi).
