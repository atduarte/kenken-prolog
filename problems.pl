% ===================================================
% =============== Generator =====================
% ===================================================

getProblem(0, N, Problem) :-
    getRandomBoard(N, Board),
    getCages(N, Cages),
    getProblemAux(Board, Cages, Problem).


% ===================================================
% ================= Generated =======================
% ===================================================

%[2,1]
%[1,2]

getProblem(1, 2, [
    [4,[[1,1],[1,2]],2],
    [1,[[2,1],[2,2]],3]
]).

%[2,1,3]
%[1,3,2]
%[3,2,1]

getProblem(1, 3, [
    [4,[[1,1],[1,2]],2],
    [1,[[1,3],[2,3],[2,2]],8],
    [1,[[2,1],[3,1],[3,2]],6],
    [2,[[3,3]],1]
]).

%[2,3,1,4]
%[3,1,4,2]
%[1,4,2,3]
%[4,2,3,1]

getProblem(1, 4, [
    [1,[[1,1],[1,2],[2,2]],6],
    [2,[[1,3],[2,3],[3,3]],8],
    [1,[[1,4],[2,4],[3,4]],9],
    [1,[[2,1],[3,1],[4,1]],8],
    [2,[[3,2],[4,2],[4,3],[4,4]],24]
]).

%[4,1,2,3,5]
%[2,4,5,1,3]
%[1,5,3,4,2]
%[5,3,1,2,4]
%[3,2,4,5,1]

getProblem(1, 5, [
    [1,[[1,1],[1,2],[1,3]],7],
    [1,[[1,4],[2,4],[2,3]],9],
    [1,[[1,5],[2,5],[3,5]],10],
    [3,[[2,1],[3,1]],1],
    [1,[[2,2],[3,2],[4,2],[4,3]],13],
    [2,[[3,3],[3,4],[4,4],[4,5],[5,5]],96],
    [2,[[4,1],[5,1],[5,2],[5,3],[5,4]],600]
]).

%[5,2,1,3,6,4]
%[2,1,3,6,4,5]
%[1,6,2,4,5,3]
%[4,5,6,2,3,1]
%[6,3,4,5,1,2]
%[3,4,5,1,2,6]

getProblem(1, 6, [
    [1,[[1,1],[2,1],[3,1]],8],
    [2,[[1,2],[2,2],[2,3],[3,3],[3,2]],72],
    [1,[[1,3],[1,4],[2,4]],10],
    [2,[[1,5],[1,6],[2,6],[3,6],[4,6],[4,5]],1080],
    [1,[[2,5],[3,5],[3,4],[4,4]],15],
    [2,[[4,1],[5,1],[5,2],[6,2]],288],
    [2,[[4,2],[4,3],[5,3],[6,3]],600],
    [1,[[5,4],[6,4],[6,5],[5,5],[5,6],[6,6]],17],
    [1,[[6,1]],3]
]).

% [1 3 2 4 5 6 7]
% [3 2 1 5 4 7 6]
% [2 1 5 6 7 3 4]
% [4 5 6 7 1 2 3]
% [6 4 7 2 3 5 1]
% [5 7 4 3 6 1 2]
% [7 6 3 1 2 4 5]

getProblem(1, 7, [
    [2,[[1,1],[2,1],[3,1],[4,1],[5,1],[6,1]],720],
    [2,[[1,2],[2,2]],6],
    [2,[[1,3],[2,3],[2,4]],10],
    [3,[[1,4],[1,5]],-1],
    [2,[[1,6],[1,7],[2,7],[3,7],[4,7]],3024],
    [1,[[2,5],[3,5],[4,5],[4,4],[3,4],[3,3]],30],
    [2,[[2,6],[3,6],[4,6],[5,6]],210],
    [2,[[3,2],[4,2],[5,2],[5,3],[4,3]],840],
    [1,[[5,4],[5,5],[6,5],[6,6],[7,6]],16],
    [1,[[5,7],[6,7],[7,7]],8],
    [2,[[6,2],[6,3],[6,4]],84],
    [2,[[7,1],[7,2],[7,3],[7,4],[7,5]],252]
]).

%[5,1,2,3,4,6,7,8]
%[1,6,3,2,5,4,8,7]
%[2,3,8,4,1,7,5,6]
%[3,2,7,1,6,8,4,5]
%[4,5,6,8,7,1,2,3]
%[6,4,5,7,8,2,3,1]
%[7,8,1,5,2,3,6,4]
%[8,7,4,6,3,5,1,2]

% ou

%[1,4,8,2,5,6,7,3]
%[2,6,3,1,4,5,8,7]
%[5,3,2,8,1,7,4,6]
%[3,7,6,4,2,8,1,5]
%[4,5,1,6,7,3,2,8]
%[6,1,5,7,8,4,3,2]
%[7,8,4,3,6,2,5,1]
%[8,2,7,5,3,1,6,4]


getProblem(1, 8, [
    [1,[[1,1],[2,1],[2,2],[1,2]],13],
    [2,[[1,3],[2,3],[3,3],[3,4],[2,4]],384],
    [1,[[1,4],[1,5],[1,6]],13],
    [2,[[1,7],[1,8],[2,8],[3,8],[4,8],[5,8]],35280],
    [2,[[2,5],[2,6]],20],
    [1,[[2,7],[3,7],[3,6],[4,6],[4,7],[5,7],[5,6],[6,6]],37],
    [1,[[3,1],[4,1],[5,1],[6,1],[6,2]],19],
    [2,[[3,2],[4,2],[4,3],[5,3],[6,3],[7,3],[8,3],[8,2]],35280],
    [2,[[3,5],[4,5],[4,4],[5,4],[6,4],[6,5]],2688],
    [1,[[5,2]],5],
    [1,[[5,5]],7],
    [2,[[6,7],[6,8],[7,8],[8,8]],24],
    [1,[[7,1],[8,1]],15],
    [2,[[7,2]],8],
    [2,[[7,4],[8,4],[8,5],[8,6],[8,7],[7,7],[7,6],[7,5]],16200]
]).


% ===================================================
% ================= AUXILIARY =======================
% ===================================================

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
fillDiag(Board) :-
    fillDiag(Board).

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
    Lin > 0,
    Col > 0,
    changeElement(1, Board, NBoard, Lin, Col),
    length(Board, N),
    N1 is N+1,
    random(2, N1, Max),
    buildPathFromPoint(NBoard, NNBoard, Lin, Col, [], RCage, 1, Max),
    append(Cages, [RCage], NCages),
    getCages(NNBoard, NCages, RCages).

findFreeElement(Board, Lin, Col) :-
    append(Board, UniBoard),
    length(Board, N),
    search(UniBoard, 1, R),
    Lin is ceiling(R/N),
    mod(R, N, Col).

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
    N1 is N+1,
    buildPathFromPoint(NBoard, FBoard, NLin, NCol, NewCage, RCage, N1, Max).
