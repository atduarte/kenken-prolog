setupDifferent([]).
setupDifferent([H|T]):-
    all_distinct(H),
    setupDifferent(T).

setupGroup(Board, [Operation, Spaces, Result]) :-
    length(Spaces, Length),
    Length =:= 1,
    [[Row,Column]] = Spaces,
    getElement(Board, Row, Column, Position),
    Position is Result.
setupGroup(Board, [Operation, Spaces, Result]) :-
    length(Spaces, Length),
    Length =:= 2,
    [[Row1,Column1],[Row2,Column2]] = Spaces,
    getElement(Board, Row1, Column1, Position1),
    getElement(Board, Row2, Column2, Position2),
    checkGroup(Op, Position1, Position2, Result).
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

% length = 1
checkGroup(1, Position1, Position2, Result) :- Position1 + Position2 #= Result.
checkGroup(2, Position1, Position2, Result) :- Position1 * Position2 #= Result.
checkGroup(3, Position1, Position2, Result) :- Position1 - Position2 #= Result.
%checkGroup(3, Position1, Position2, Result) :- Position2 - Position1 #= Result.
checkGroup(4, Position1, Position2, Result) :- Position2 * Result #= Position1.
%checkGroup(4, Position1, Position2, Result) :- Position1 * Result #= Position2.

% length > 2
checkGroup(1, Positions, Result) :- sum(Positions,#=,Result).
checkGroup(2, Positions, Result) :- prod(Positions,Result).

% product of a list
prod(List, Result) :-
        (foreach(L, List),
         fromto(1, In, Out, Result) do
             Out #= In * L
        ).
