accao(move_chao(A),[livre(A), sobre(A,B)], [chao(A), livre(B),livre(A)], [sobre(A,B)]):- member(A,[a,b,c]), member(B,[a,b,c]), A\= B.

accao(move(A,B),[livre(A), livre(B),chao(A)], [sobre(A,B), livre(A)], [livre(B), chao(A)]):- member(A,[a,b,c]), member(B,[a,b,c]), A\= B.

accao(move(A,B),[livre(A), livre(B),sobre(A,C)], [sobre(A,B), livre(A), livre(C)], [livre(B), sobre(A,C)]):- member(A,[a,b,c]), member(B,[a,b,c]), A\= B,member(C,[a,b,c]), A\=C, B\=C.

estado_final([chao(a),chao(c), sobre(b,c), livre(b), livre(a)]).

%estado_final([chao(c), livre(a), sobre(b,c),sobre(a,b)]).

estado_inicial([chao(a),chao(c), sobre(b,a), livre(b), livre(c)]).