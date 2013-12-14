getElement(Board, RowNumber, ColumnNumber, Value) :-
    nth1(RowNumber, Board, Row),
    element(ColumnNumber, Row, Value).


changeElement(P, B, B1, L, C) :-
    changeElementAuxLine(P, B, B1, C, L, 1, 1).

changeElementAuxLine(P, [], [], C, L, NL, NC).
changeElementAuxLine(P, [H|T], [H|T1], C, L, NL, NC) :-
    NL \= L,
    NL1 is NL + 1,
    changeElementAuxLine(P, T, T1, C, L, NL1, NC).
changeElementAuxLine(P, [H|T], [H1|T1], C, L, NL, NC) :-
    NL = L,
    changeElementAuxColumn(P, H, H1, C, L, NC, NL),
    NL1 is NL + 1,
    changeElementAuxLine(P, T, T1, C, L, NL1, NC).

changeElementAuxColumn(P, [], [], C, L, NC, NL).
changeElementAuxColumn(P, [H|T], [H|T1], C, L, NC, NL) :-
    NC \= C,
    NC1 is NC + 1,
    changeElementAuxColumn(P, T, T1, C, L, NC1, NL).
changeElementAuxColumn(P, [H|T], [P|T1], C, L, NC, NL) :-
    NC = C,
    NC1 is NC + 1,
    changeElementAuxColumn(P, T, T1, C, L, NC1, NL).


mod(N, D, D) :-
    X is N mod D,
    X =:= 0, !.
mod(N, D, Result) :-
    Result is N mod D.


shuffleList(List, ResultList) :-
    shuffleList(List, [], ResultList).
shuffleList([], TList, TList).
shuffleList(List, TList, ResultList) :-
    length(List, N),
    N1 is N+1,
    random(1, N1, P),
    element(P, List, Value),
    delete(List, Value, NList),
    append(TList, [Value], NTList),
    shuffleList(NList, NTList, ResultList).
