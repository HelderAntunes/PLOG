:- include('tents.pl').

test1 :-
	launch(6, 8).
	
test2 :-
	launch(7, 9).

% o gerador as vezes demora um pouco.
test3 :-
	launch(15, 14).