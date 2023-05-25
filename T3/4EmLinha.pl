% Each column is a term col(Num,Free,TP,TN,Ps), where:
% Num: column number
% Free: yes/no, whether a piece can be placed "on top" (= at the end)
% TP: Colour of topmost piece
% TN: max. number of consecutive topmost pieces of same colour
% Ps: Pieces in this column
empty(-).

estado_inicial(e([col(1,yes, empty, 0, Empty),
                  col(2,yes, empty, 0, Empty),
                  col(3,yes, empty, 0, Empty), 
                  col(4,yes, empty, 0, Empty),
                  col(5,yes, empty, 0, Empty),
                  col(6,yes, empty, 0, Empty)])).

terminal(Board, Player):- (four_in_a_row(Board, Player) ; diagonal(Board, Player)).

four_in_a_row([Col1,Col2,Col3,Col4|Cs], Player):-
    (   four_in_a_row(Col1, Col2, Col3, Col4, Player)
    ;   four_in_a_row([Col2,Col3,Col4|Cs], Player)
    ).

four_in_a_row([C1|Cs1], [C2|Cs2], [C3|Cs3], [C4|Cs4], P) :-
    empty(E),
    Firsts = [C1,C2,C3,C4],
    maplist(dif(E), Firsts),
    (   maplist(=(P), Firsts)
    ;   four_in_a_row(Cs1, Cs2, Cs3, Cs4, P)
    ).

diagonal(Board, Player) :-
    Board = [_,_,_,_|_],
    (   diagonal_down(Board, Player)
    ;   diagonal_up(Board, Player)
    ;   Board = [_|Rest],
        diagonal(Rest, Player)
    ).

diagonal_down([Col1,Col2,Col3,Col4|_], Player) :-
        Col2 = [_|Rot2],
        Col3 = [_,_|Rot3],
        Col4 = [_,_,_|Rot4],
        four_in_a_row(Col1, Rot2, Rot3, Rot4, Player).

diagonal_up([Col1,Col2,Col3,Col4|_], Player) :-
        Col1 = [_,_,_|Rot1],
        Col2 = [_,_|Rot2],
        Col3 = [_|Rot3],
        four_in_a_row(Rot1, Rot2, Rot3, Col4, Player).