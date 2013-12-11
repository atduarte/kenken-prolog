


kenken(B, N) :-
    % Build square board
    length(Board, N),
    buildBoard(Board, N),

    % Build unified list
    append(Board, UniBoard), % mete todos os elementos do board numa só lista

    % SETUP Domain of each number
    domain(UniBoard, 1, N), %todos os elementos têm que ser números de 1 a 9

    % SETUP each row different
    setupDifferent(B),

    % SETUP each column different
    transpose(Board, TBoard),
    setupDifferent(TBoard),

    % SETUP groups
    setupGroups(Board, Problem),

    labeling([], R).

% create

buildBoard([], N).
buildBoard([H|T], N):-
	length(H, N),
	buildBoard(T,N).


%setupUnifiedList([], []).
%setupUnifiedList([H|T], R):-
%	setupUnifiedList(T, R1),
%	append(H, R1, R).

setupDifferent([]).
setupDifferent([H|T]):-
	all_distinct(H),
	setupDifferent(T).

setupGroups(Board, []). % TODO
setupGroups(Board, [[Result, Spaces] | T]) :-
    length(Spaces, Length),
    Length =:= 2, % TODO - Ver operador


getElement(Board, RowNumber, ColumnNumber, Value) :-
    nth1(RowNumber, Board, Row),
    element(ColumnNumber, Row, Value).
