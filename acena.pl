/*

  KenKen in SICStus Prolog.

  http://en.wikipedia.org/wiki/KenKen
  """
  KenKen or KEN-KEN is a style of arithmetic and logical puzzle sharing
  several characteristics with sudoku. The name comes from Japanese and
  is translated as "square wisdom" or "cleverness squared".
  ...
  The objective is to fill the grid in with the digits 1 through 6 such that:

    * Each row contains exactly one of each digit
    * Each column contains exactly one of each digit
    * Each bold-outlined group of cells is a cage containing digits which
      achieve the specified result using the specified mathematical operation:
        addition (+),
        subtraction (-),
        multiplication (x),
        and division (ÃƒÆ’Ã‚Â·).
        (Unlike in Killer sudoku, digits may repeat within a group.)

  ...
  More complex KenKen problems are formed using the principles described
  above but omitting the symbols +, -, x and /, thus leaving them as
  yet another unknown to be determined.
  """


  Compare with the following models:
  * Comet   : http://www.hakank.org/comet/kenken.co
  * Comet   : http://www.hakank.org/comet/kenken2.co (more general version).
  * ECLiPSe : http://www.hakank.org/eclipse/kenken2.ecl
  * MiniZinc: http://www.hakank.org/minizinc/kenken2.mzn

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


go :-
        problem(1,N, Problem),
        kenken(N, Problem).


kenken(N, Problem) :-

        % decision variables
        matrix(X,[N,N]),
        append(X,XList),
        domain(XList,1,N),

        % all rows and columns must be unique
        ( foreach(Row,X) do
              all_distinct(Row)
        ),
        transpose(X,XTransposed),
        ( foreach(Col,XTransposed) do
              all_distinct(Col)
        ),

        % Handle the hints
        ( foreach(P,Problem),
          param(X)
        do
          [Result,Coeffs] = P,
          calc(Result,Coeffs,X)
        ),

        % search
        !, labeling([ff, bisect, up], XList),

        % output
        pretty_print(X).


% special handling for size 2
calc(Result, Coeffs,X) :-
        length(Coeffs, Len),
        Len =:= 2,
        [[AR,AC],[BR,BC]] = Coeffs,
        matrix_element(X,AR,AC,A),
        matrix_element(X,BR,BC,B),
        check2(A,B,Result),
        indomain(A),
        indomain(B).

% length > 2
calc(Result,Coeffs, X) :-
        length(Coeffs, Len),
        Len > 2,
        (foreach([R,C],Coeffs),
         foreach(XRC, CoeffRes),
         param(X) do
             matrix_element(X,R,C,XRC)
        ),
        check_many(Result, CoeffRes).

%
% all alternatives for 2 argument
% I assume that only segments with 2 cells can be
% used with minus or div.
%
check2(A,B,Result) :- A * B #= Result.
check2(A,B,Result) :- A * Result #= B. % B/A = Result
check2(A,B,Result) :- B * Result #= A. % A/B = Result
check2(A,B,Result) :- A + B #= Result.
check2(A,B,Result) :- A - B #= Result.
check2(A,B,Result) :- B - A #= Result.


% either sum or product
check_many(Result, CoeffRes) :- prod(CoeffRes,Result).
check_many(Result, CoeffRes) :- sum(CoeffRes,#=,Result).

% product of a list
prod(List, Product) :-
        (foreach(L, List),
         fromto(1,In,Out,Product) do
             Out #= In * L
        ).

pretty_print(X) :-
        ( foreach(Row,X)
        do
          write(Row),nl
        ).


matrix_element(X, I, J, Val) :-
        nth1(I, X, Row),
        element(J, Row, Val).


% From Mats Carlsson.
matrix(_, []) :- !.
matrix(L, [Dim|Dims]) :-
        length(L, Dim),
        (   foreach(X,L),
            param(Dims)
        do  matrix(X, Dims)
        ).




%
% State the problem, i.e. the hints.
%
% For a better view of the problem, see
%  http://en.wikipedia.org/wiki/File:KenKenProblem.svg
%
%   The solution is:
%     5 6 3 4 1 2
%     6 1 4 5 2 3
%     4 5 2 3 6 1
%     3 4 1 2 5 6
%     2 3 6 1 4 5
%     1 2 5 6 3 4
%
problem(1, 7, [ [1,[[1,1],[2,1]]],
                [12,[[1,2],[1,3],[2,3]]],
                [5040,[[1,4],[2,4],[2,5],[2,6],[2,7],[3,7]]],
                [18,[[1,5],[1,6],[1,7]]],
                [19,[[2,2],[3,2],[3,1],[4,1],[4,2]]],
                [20,[[3,3],[4,3],[4,4],[4,5],[4,6]]],
                [14,[[3,4],[3,5],[3,6]]],
                [24,[[4,7],[5,7],[6,7],[6,6],[5,6],[5,5]]],
                [34,[[5,1],[6,1],[7,1],[7,2],[6,2],[5,2],[5,3]]],
                [24,[[5,4],[6,4],[6,3],[7,3]]],
                [84,[[6,5],[7,5],[7,4]]],
                [4,[[7,6],[7,7]]]]
).
