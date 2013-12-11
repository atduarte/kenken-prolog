:-use_module(library(clpfd)).
:-use_module(library(lists)).
% create

buildBoard([], N).
buildBoard([H|T], N):-
	length(H, N),
	buildBoard(T,N).


setupUnifiedList([], []).
setupUnifiedList([H|T], R):-
	setupUnifiedList(T, R1),
	append(H, R1, R).

setupDifferent([]).
setupDifferent([H|T]):-
	all_distinct(H),
	setupDifferent(T).


kenken(B, N) :-
	length(B, N),
	buildBoard(B, N),
	setupUnifiedList(B, R), %mete todos os elementos do board numa só lista
	domain(R, 1, N), %todos os elementos têm que ser números de 1 a 9
	setupDifferent(B), %mete as colunas com numeros diferentes
	transpose(B, NB), %faz a transposta para agora mexer nas linhas
	setupDifferent(NB), %linhas com números todos diferentes
	setupUnifiedList(B, R1), %volta a juntar tudo para o labeling
	labeling([], R1).