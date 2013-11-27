checkSum([], X, R):-
	X = R.

checkSum([H|T], X, R):-
	X1 = X + H,
	checkSum(T, X, R).

checkMult([H|T], X, R):-
	X1 = X * H,
	checkMult(T, X, R).

checkDiv([H|T], X, R):-
	X1 = X / H,
	checkDiv(T, X, R).

checkSub([H|T], X, R):-
	X1 = X - H,
	checkSub(T, X, R).

