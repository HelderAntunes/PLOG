:- use_module(library(clpfd)).
:- use_module(library(random)).
:- use_module(library(lists)).

% Coloca a restricao: o tent esta a tocar a sua arvore horizontalmente ou verticalmente.
tentIsAroundHisTree_restriction([], [], _).
tentIsAroundHisTree_restriction([Tree|Trees], [Tent|Tents], SizeBoard):-
	%Toca horizontalmente
((Tree #= Tent - 1 #\/ Tree #= Tent + 1) #/\ Tree / SizeBoard #= Tent / SizeBoard ) 
	%Toca Verticalmente
    #\/ (Tree #= Tent - SizeBoard) #\/ (Tree #= Tent + SizeBoard), 
    tentIsAroundHisTree_restriction(Trees, Tents, SizeBoard).

% Colaca a restricao: tents nao sao vizinhos entre si.
tentsDontTouchEachOther_restriction([], _, _).
tentsDontTouchEachOther_restriction([T|Ts], Tents, SizeBoard) :-
    tentIsIsolated(T, Tents, SizeBoard),
    tentsDontTouchEachOther_restriction(Ts, Tents, SizeBoard).

% Coloca a restricao: um tent nao e vizinho de nenhum outro tent.
tentIsIsolated(_, [], _).
tentIsIsolated(Tent, [T|Ts], SizeBoard) :-
	% toques horizontais
    (((Tent #= T + 1 #\/ Tent #= T - 1) #/\ Tent / SizeBoard #= T / SizeBoard) 
	% toques verticais
     #\/ Tent #= T + SizeBoard #\/ Tent #= T - SizeBoard #\/   
	% toques diagonais
    (Tent #= T + SizeBoard + 1 #/\ T / SizeBoard #= Tent / SizeBoard - 1) #\/      
    (Tent #= T + SizeBoard - 1 #/\ T / SizeBoard #= Tent / SizeBoard - 1) #\/     
    (T #= Tent + SizeBoard + 1 #/\ Tent / SizeBoard #= T / SizeBoard - 1) #\/      
    (T #= Tent + SizeBoard - 1 #/\ Tent / SizeBoard #= T / SizeBoard - 1)) #<=> B, 
	B #= 0,
    tentIsIsolated(Tent, Ts, SizeBoard).

% Coloca a restricao: Na linha x existem y tents.
numTentsInRows_restriction(_, _, Row, SizeBoard) :- Row > SizeBoard.
numTentsInRows_restriction(RowTents, Tents, Row, SizeBoard) :-
	Row =< SizeBoard,
	nth1(Row, RowTents, -1), !, % numero de tents nao determinado
	NextRow is Row + 1,
	numTentsInRows_restriction(RowTents, Tents, NextRow, SizeBoard).
numTentsInRows_restriction(RowTents, Tents, Row, SizeBoard) :-
	Row =< SizeBoard,
	countTentsInRow(Tents, Row, SizeBoard, Total),
	element(Row, RowTents, Total),
	NextRow is Row + 1,
	numTentsInRows_restriction(RowTents, Tents, NextRow, SizeBoard).

% Conta o numero de tents na linha Row.
countTentsInRow([], _, _, 0).	
countTentsInRow([Tent|Tents], Row, SizeBoard, Total) :-
	Tent / SizeBoard + 1 #= Row #<=> B, 
	Total #= B + TotalAux,
	countTentsInRow(Tents, Row, SizeBoard, TotalAux).
	
% Coloca a restricao: Na coluna x existem y tents.
numTentsInCols_restriction(_, _, Col, SizeBoard) :- Col > SizeBoard.
numTentsInCols_restriction(ColTents, Tents, Col, SizeBoard) :-
	Col =< SizeBoard,
	nth1(Col, ColTents, -1), !, % numero de tents nao determinado
	NextCol is Col + 1,
	numTentsInCols_restriction(ColTents, Tents, NextCol, SizeBoard).
numTentsInCols_restriction(ColTents, Tents, Col, SizeBoard) :-
	Col =< SizeBoard,
	countTentsInCol(Tents, Col, SizeBoard, Total),
	element(Col, ColTents, Total),
	NextCol is Col + 1,
	numTentsInCols_restriction(ColTents, Tents, NextCol, SizeBoard).

% Conta o numero de tents na coluna Col.
countTentsInCol([], _, _, 0).	
countTentsInCol([Tent|Tents], Col, SizeBoard, Total) :- 
	Tent mod SizeBoard + 1 #= Col #<=> B, 
	Total #= B + TotalAux,
	countTentsInCol(Tents, Col, SizeBoard, TotalAux).