kenken(N) :-
    % Build square board
    length(Board, N),
    buildBoard(Board, N),

    % Aux Print

    ( foreach(Row,Board)
    do
      write(Row),nl
    ),

    % Problem Generate
    problem(6, Problem),

    % Build unified list
    append(Board, UniBoard), % mete todos os elementos do board numa só lista

    % SETUP Domain of each number
    domain(UniBoard, 1, N), %todos os elementos têm que ser números de 1 a 9

    % SETUP each row different
    setupDifferent(Board),

    % SETUP each column different
    transpose(Board, TBoard),
    setupDifferent(TBoard),

    % SETUP groups
    ( foreach(P,Problem),
      param(Board)
    do
        setupGroups(Board, P)
    ),

    % Get Result
    labeling([], UniBoard),

    % Aux Print
    ( foreach(Row,Board)
    do
      write(Row),nl
    ).

% create

buildBoard([], N).
buildBoard([H|T], N):-
	length(H, N),
	buildBoard(T, N).
setupDifferent([]).
setupDifferent([H|T]):-
	all_distinct(H),
	setupDifferent(T).

setupGroups(Board, []).
setupGroups(Board, [H | T]) :-
    setupGroup(Board, H),
    setupGroups(Board, T).

setupGroup(Board, [Operation, Spaces, Result]) :-
    length(Spaces, Length),
    Length =:= 2, % TODO - Ver operador
    [[Row1,Column1],[Row2,Column2]] = Spaces,
    getElement(Board, Row1, Column1, Position1),
    getElement(Board, Row2, Column2, Position2),
    checkGroup(Operation, Position1, Position2, Result),
    indomain(Position1),
    indomain(Position2).
setupGroup(Board, [Operation, Spaces, Result]) :-
    length(Spaces, Length),
    Length > 2,
    (foreach([Row,Column],Spaces),
     foreach(Value, UniSpaces),
     param(Board)
     do
         getElement(Board, Row, Column, Value)
    ),
    checkGroup(Operation, UniSpaces, Result).


checkGroup(1, Position1, Position2, Result) :- Position1 + Position2 #= Result.
checkGroup(2, Position1, Position2, Result) :- Position1 - Position2 #= Result.
checkGroup(2, Position1, Position2, Result) :- Position2 - Position1 #= Result.
checkGroup(3, Position1, Position2, Result) :- Position1 * Position2 #= Result.
checkGroup(3, Position1, Position2, Result) :- Position1 * Result #= Position2.
checkGroup(4, Position1, Position2, Result) :- Position1 * Result #= Position2.
checkGroup(4, Position1, Position2, Result) :- Position2 * Result #= Position1.

% length > 2
checkGroup(1, Positions, Result) :- sum(Positions,#=,Result).
checkGroup(3, Positions, Result) :- prod(Positions,Result).

% product of a list
prod(List, Result) :-
        (foreach(L, List),
         fromto(1, In, Out, Result) do
             Out #= In * L
        ).

getElement(Board, RowNumber, ColumnNumber, Value) :-
    nth1(RowNumber, Board, Row),
    element(ColumnNumber, Row, Value).

setElement()

problem(6, [
            [1, [[1,1], [2,1]], 11],
            [4, [[1,2], [1,3]],  2],
            [3, [[1,4], [2,4]], 20],
            [3, [[1,5], [1,6], [2,6], [3,6]],  6],
            [2, [[2,2], [2,3]],  3],
            [4, [[2,5], [3,5]],  3],
            [3, [[3,1], [3,2], [4,1], [4,2]],240],
            [3, [[3,3], [3,4]],  6],
            [3, [[4,3], [5,3]],  6],
            [1, [[4,4], [5,4], [5,5]],  7],
            [3, [[4,5], [4,6]], 30],
            [3, [[5,1], [5,2]],  6],
            [1, [[5,6], [6,6]],  9],
            [1, [[6,1], [6,2], [6,3]],  8],
            [4, [[6,4], [6,5]],  2]
           ]).
