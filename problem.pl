getProblem(N, Problem) :-
    getRandomBoard(N, Board),
    getCages(N, Cages),
    write('Gerado'), nl,
    getProblemAux(Board, Cages, Problem),
    write('Compilado'),nl.

getProblemAux(Board, [], []).
getProblemAux(Board, [H|T], [H1|T1]) :-
    length(H, N),
    N < 3,
    (foreach(Space,H), foreach(V,Values), param(Board) do [X, Y] = Space, getElement(Board, X, Y, V)),
    random(1, 5, Operation),
    append([Operation], [H], TList),
    operate(Operation, Values, Result), !,
    append(TList, [Result], H1),
    getProblemAux(Board, T, T1).
getProblemAux(Board, [H|T], [H1|T1]) :-
    length(H, N),
    N < 3, !,
    (foreach(Space,H), foreach(V,Values), param(Board) do [X, Y] = Space, getElement(Board, X, Y, V)),
    random(1, 4, Operation),
    append([Operation], [H], TList),
    operate(Operation, Values, Result),
    append(TList, [Result], H1),
    getProblemAux(Board, T, T1).
getProblemAux(Board, [H|T], [H1|T1]) :-
    length(H, N),
    N > 2,
    random(1, 3, Operation),
    append([Operation], [H], TList),
    (foreach(Space,H), foreach(V,Values), param(Board) do [X, Y] = Space, getElement(Board, X, Y, V)),
    operate(Operation, Values, Result),
    append(TList, [Result], H1),
    getProblemAux(Board, T, T1).

operate(1, [], 0).
operate(1, [H|T], Result) :-
    operate(1, T, PreResult),
    Result is H + PreResult.
operate(2, [], 1).
operate(2, [H|T], Result) :-
    operate(2, T, PreResult),
    Result is H * PreResult.
operate(3, [X, Y], Result) :-
    Result is X-Y.
operate(4, [X, Y], Result) :-
    N is X mod Y,
    N = 0,
    Result is X//Y.

getRandomBoard(N, Board) :-
    % Build square board
    length(Board, N),
    buildBoard(Board, N),

    % Build unified list
    append(Board, UniBoard), % mete todos os elementos do board numa só lista

    % SETUP Domain of each number
    domain(UniBoard, 1, N), %todos os elementos têm que ser números de 1 a 9

    % FILL diagonal of board with random numbers
    fillDiag(Board),

    % SETUP each row different
    setupDifferent(Board),

    % SETUP each column different
    transpose(Board, TBoard),
    setupDifferent(TBoard),
    labeling([], UniBoard).

fillDiag(Board) :-
    ( foreach(Row, Board),
      fromto(1, In, Out, N),
      param(Board)
      do(
            length(Board, N),
            N1 is N + 1,
            random(1, N1, Value),
            getElement(Board, In, In, Value),
            Out is In+1
        )).

getCages(N, Cages) :-
    length(Board, N),
    buildBoard(Board, N),
    (foreach(Row, Board)
    do (foreach(Element, Row)
        do(Element is 0))),
    getCages(Board, [], Cages).

getCages(Board, Cages, Cages):-
    findFreeElement(Board, Lin, Col),
    Lin = 0.
getCages(Board, Cages, RCages):-
    findFreeElement(Board, Lin, Col),
    %write(Lin),nl,
    %write(Col),
    Lin > 0,
    Col > 0,
    changeElement(1, Board, NBoard, Lin, Col),
    length(Board, N),
    N1 is N+1,
    random(2, N1, Max),
    buildPathFromPoint(NBoard, NNBoard, Lin, Col, [], RCage, 1, Max),
    append(Cages, [RCage], NCages),
    getCages(NNBoard, NCages, RCages).

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

findFreeElement(Board, Lin, Col) :-
    append(Board, UniBoard),
    length(Board, N),
    search(UniBoard, 1, R),
    Lin is ceiling(R/N),
    mod(R, N, Col).

mod(N, D, D) :-
    X is N mod D,
    X =:= 0, !.
mod(N, D, Result) :-
    Result is N mod D.

search([], I, 0).
search([0|T], I, I) :- !.
search([H|T], I, R) :-
    I1 is I+1,
    search(T, I1, R).


getNextPoint(Board, Lin, Col, 0, 0, []).
getNextPoint(Board, Lin, Col, NLin, NCol, [1 | T]) :-
    Lin > 1,
    NLin is Lin-1,
    NCol is Col,
    getElement(Board, NLin, NCol, E),
    E = 0, !.
getNextPoint(Board, Lin, Col, NLin, NCol, [2 | T]) :-
    length(Board, N),
    Col < N,
    NLin is Lin,
    NCol is Col+1,
    getElement(Board, NLin, NCol, E),
    E = 0, !.
getNextPoint(Board, Lin, Col, NLin, NCol, [3 | T]) :-
    length(Board, N),
    Lin < N,
    NLin is Lin+1,
    NCol is Col,
    getElement(Board, NLin, NCol, E),
    E = 0, !.
getNextPoint(Board, Lin, Col, NLin, NCol, [4 | T]) :-
    Col > 0,
    NLin is Lin,
    NCol is Col-1,
    getElement(Board, NLin, NCol, E),
    E = 0, !.
getNextPoint(Board, Lin, Col, NLin, NCol, [H | T]) :-
    % Falhou
    getNextPoint(Board, Lin, Col, NLin, NCol, T).

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

buildPathFromPoint(Board, Board, Lin, Col, Cage, NewCage, N, N) :-
    append(Cage, [[Lin, Col]], NewCage), !.
buildPathFromPoint(Board, Board, Lin, Col, Cage, NewCage, N, Max) :-
    append(Cage, [[Lin, Col]], NewCage),
    shuffleList([1,2,3,4], Directions),
    getNextPoint(Board, Lin, Col, NLin, NCol, Directions),
    NLin = 0,
    NCol = 0, !.
buildPathFromPoint(Board, FBoard, Lin, Col, Cage, RCage, N, Max):-
    append(Cage, [[Lin, Col]], NewCage),
    shuffleList([1,2,3,4], Directions),
    getNextPoint(Board, Lin, Col, NLin, NCol, Directions),
    NLin > 0,
    NCol > 0,
    changeElement(1, Board, NBoard, NLin, NCol),
    print(NLin), write(NCol), nl,
    N1 is N+1,
    buildPathFromPoint(NBoard, FBoard, NLin, NCol, NewCage, RCage, N1, Max).




%checkSum([], X, R):-
%	X = R.
%
%checkSum([H|T], X, R):-
%	X1 = X + H,
%	checkSum(T, X, R).
%
%checkMult([H|T], X, R):-
%	X1 = X * H,
%	checkMult(T, X, R).
%
%checkDiv([H|T], X, R):-
%	X1 = X / H,
%	checkDiv(T, X, R).
%
%checkSub([H|T], X, R):-
%	X1 = X - H,
%	checkSub(T, X, R).
%
%
