kenken(N) :-
    % Build square board
    length(Board, N),
    buildBoard(Board, N),

    % Problem Generate
    getProblem(N, Problem),

    % Build unified list
    append(Board, UniBoard), % mete todos os elementos do board numa só lista

    % SETUP Domain of each number
    domain(UniBoard, 1, N), %todos os elementos têm que ser números de 1 a 9

    % SETUP each row different
    setupDifferent(Board),

    % SETUP each column different
    transpose(Board, TBoard),
    setupDifferent(TBoard),

    write('Diferentes'),nl,

    % SETUP groups
    ( foreach(P,Problem),
      param(Board)
    do
        setupGroup(Board, P)
    ),

    write('Setupped'),nl,

    % Get Result
    labeling([], UniBoard),

    % Aux Print
    ( foreach(Row,Board)
    do
      write(Row),nl
    ).

problem(6, [
            [1, [[1,1]], 5],
            [1, [[2,1]], 6],
            [4, [[1,2], [1,3]],  2],
            [2, [[1,4], [2,4]], 20],
            [2, [[1,5], [1,6], [2,6], [3,6]],  6],
            [3, [[2,2], [2,3]],  3],
            [4, [[2,5], [3,5]],  3],
            [2, [[3,1], [3,2], [4,1], [4,2]],240],
            [2, [[3,3], [3,4]],  6],
            [2, [[4,3], [5,3]],  6],
            [1, [[4,4], [5,4], [5,5]],  7],
            [2, [[4,5], [4,6]], 30],
            [2, [[5,1], [5,2]],  6],
            [1, [[5,6], [6,6]],  9],
            [1, [[6,1], [6,2], [6,3]],  8],
            [4, [[6,4], [6,5]],  2]
           ]).
