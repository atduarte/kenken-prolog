kenken(N) :-
    kenken(1, N).

% T = 0 -> Problem Generator
% T = 1 -> Generated Problems

kenken(T, N) :-
    % Build square board
    length(Board, N),
    buildBoard(Board, N),

    % Problem Generate
    getProblem(T, N, Problem), !,

    write('Problem:'), nl,
    ( foreach(Row,Problem)
    do
      [OpNumber, Spaces, Result] = Row,
      opSymbol(OpNumber, Op),
      append([], [Op, Spaces, Result], RowMod),
      write(RowMod),nl
    ),nl,!,

    now(StartTime),

    % Build unified list
    append(Board, UniBoard), % mete todos os elementos do board numa só lista

    % SETUP Domain of each number
    domain(UniBoard, 1, N), % todos os elementos têm que ser números de 1 a 9

    % SETUP each row different
    setupDifferent(Board),

    % SETUP each column different
    transpose(Board, TBoard),
    setupDifferent(TBoard),

    % SETUP groups
    ( foreach(P,Problem),
      param(Board)
    do
        setupGroup(Board, P)
    ),

    % Get Result
    labeling([bisect], UniBoard), !,

    now(EndTime),

    ExecutionTime is EndTime - StartTime,

    % Aux Print
    write('Resolution:'), nl,
    ( foreach(Row,Board)
    do
      write(Row),nl
    ),nl,

    write('Statistics: '), nl,
    write('Execution time: '), write(ExecutionTime), write(' seconds'),nl,
    fd_statistics,
    nl.

opSymbol(1, '+').
opSymbol(2, '*').
opSymbol(3, '-').
opSymbol(4, '/').
