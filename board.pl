% create

buildBoard([], N).
buildBoard([H|T], N):-
	length(H, N),
	buildBoard(T, N).
