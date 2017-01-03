:- use_module(library(clpfd)).
:- use_module(library(random)).
:- use_module(library(lists)).
:- include('restrictions.pl').

:- dynamic numPositions/1.


/*
 * FEUP - 2016, Programacao em logica
 * Autores: Helder Antunes e Ines Proenca
 * Utilizacao: executar o comando "launch(TamanhoDoTabuleiro, NumeroDeArvores)".
 */

launch(SizeBoard, NumTrees) :-
	printLegend,
	write('Generator started'), nl, nl, 
	generatePuzzle(SizeBoard, NumTrees, Trees, RowTents, ColTents),
	write('Solver started'), nl, 
	solvePuzzle(SizeBoard, NumTrees, Trees, RowTents, ColTents).
	
generatePuzzle(SizeBoard, NumTrees, Trees, RowTents, ColTents) :-
    length(Trees, NumTrees),
    length(Tents, NumTrees),
    NumPositions is SizeBoard * SizeBoard - 1,
	append([Trees, Tents], Vars),
    domain(Vars, 0, NumPositions),
	
    % restricoes
	all_distinct(Vars),
    tentIsAroundHisTree_restriction(Trees, Tents, SizeBoard),
    tentsDontTouchEachOther_restriction(Tents, Tents, SizeBoard),
	labeling([value(mySelValores)], Vars),!,
	
	% preparar tabuleiro
    createEmptyBoard(SizeBoard, EmptyBoard),
	putTreesInBoard(EmptyBoard, Trees, SizeBoard, _, Board),
	length(RowTentsAux, SizeBoard),
	length(ColTentsAux, SizeBoard),
	setToZero(RowTentsAux),
	setToZero(ColTentsAux),
	
	% determinar o numero de tents nas linhas e colunas
	setNumTentsInRows(RowTentsAux, RowTentsAux2, Tents, SizeBoard),
	setNumTentsInCols(ColTentsAux, ColTentsAux2, Tents, SizeBoard),
	makeDisappearSomeValues(RowTentsAux2, RowTents),	
	makeDisappearSomeValues(ColTentsAux2, ColTents), 
	
    printBoard(Board, RowTents, ColTents), nl.
	
solvePuzzle(SizeBoard, NumTrees, Trees, RowTents, ColTents) :-
	length(Tents, NumTrees),
	NumPositions is SizeBoard * SizeBoard - 1,
	domain(Tents, 0, NumPositions),
	
	% restricoes
	all_distinct(Tents),
	tentIsAroundHisTree_restriction(Trees, Tents, SizeBoard),
	tentsDontTouchEachOther_restriction(Tents, Tents, SizeBoard),
	numTentsInRows_restriction(RowTents, Tents, 1, SizeBoard),
	numTentsInCols_restriction(ColTents, Tents, 1, SizeBoard),
	
	reset_timer,
	labeling([ffc, bisect], Tents),
	print_time,
	fd_statistics, nl, 
	
	% preparar tabuleiro
	createEmptyBoard(SizeBoard, EmptyBoard),
	putTentsAndTreesInBoard(EmptyBoard, Trees, Tents, SizeBoard, _, Board),
	
	printBoard(Board, RowTents, ColTents).
	
reset_timer :- statistics(walltime,_).	
print_time :-
	statistics(walltime,[_,T]),
	TS is ((T//10)*10)/1000,
	write('Solver ended'), nl,
	write('Time: '), write(TS), write('s'), nl, nl.
	
mySelValores(Var, _Rest, BB, BB1) :-
	fd_set(Var, Set),
	select_best_value(Set, Value),
	(   
	    first_bound(BB, BB1), Var #= Value
        ;   
	    later_bound(BB, BB1), Var #\= Value
    ).
	
select_best_value(Set, BestValue):-
	fdset_to_list(Set, Lista),
	length(Lista, Len),
	random(0, Len, RandomIndex),
	nth0(RandomIndex, Lista, BestValue).

% Cria um tabuleiro SizeBoard x SizeBoard com celulas a zero.
createEmptyBoard(SizeBoard, EmptyBoard) :-
    length(EmptyBoard, SizeBoard),
    createEmptyBoardAux(SizeBoard, EmptyBoard).

createEmptyBoardAux(_, []).
createEmptyBoardAux(SizeBoard, [H|T]) :-
    length(H, SizeBoard),
    fillRow(H),
    createEmptyBoardAux(SizeBoard, T).

fillRow([]).
fillRow([0|T]) :- fillRow(T).

% Coloca t's (tents) e T's (trees) num tabuleiro vazio.
putTentsAndTreesInBoard(BoardIn, [], [], _, _, BoardIn).
putTentsAndTreesInBoard(BoardIn, [Tree|Trees], [Tent|Tents],
                        SizeBoard, BoardAux, BoardOut) :-
    RowTree is Tree div SizeBoard + 1,
    ColTree is Tree mod SizeBoard + 1,
    RowTent is Tent div SizeBoard + 1,
    ColTent is Tent mod SizeBoard + 1,
    setPiece(RowTree, ColTree, 'T', BoardIn, BoardInAux),
    setPiece(RowTent, ColTent, 't', BoardInAux, BoardAux),
    putTentsAndTreesInBoard(BoardAux, Trees, Tents, SizeBoard, _, BoardOut).

% Coloca T's (trees) num tabuleiro vazio.
putTreesInBoard(BoardIn, [], _, _, BoardIn).
putTreesInBoard(BoardIn, [Tree|Trees],
                        SizeBoard, BoardAux, BoardOut) :-
    RowTree is Tree div SizeBoard + 1,
    ColTree is Tree mod SizeBoard + 1,
    setPiece(RowTree, ColTree, 'T', BoardIn, BoardAux),
    putTreesInBoard(BoardAux, Trees, SizeBoard, _, BoardOut).

% Coloca uma peca numa celula do tabuleiro.
setPiece(1, Column, Piece, [B|Bs], [ModRow|Bs]):-
	setPieceInRow(Column, Piece, B, ModRow), !.
setPiece(Row, Column, Piece, [B|Bs], [B|Rs]):-
	Row > 1,
	R1 is Row -1,
	setPiece(R1, Column, Piece, Bs, Rs).

setPieceInRow(1, Piece, [_|RowIn], [Piece|RowIn]):- !.
setPieceInRow(Col, Piece, [R|RowIn], [R|RowOut]):-
	Col > 1,
	C1 is Col -1,
	setPieceInRow(C1, Piece, RowIn, RowOut).
	
% Coloca uma lista a zero	
setToZero([]).
setToZero([0|T]) :- setToZero(T).

% Set number of tents in each row.
setNumTentsInRows(RowIn, RowIn, [], _).
setNumTentsInRows(RowIn, RowOut, [Tent|Tents], SizeBoard) :-
	Row is Tent // SizeBoard,
	nth0(Row, RowIn, Val), 
	NewVal is Val + 1,
	setValInList(Row, RowIn, RowAux, NewVal),
	setNumTentsInRows(RowAux, RowOut, Tents, SizeBoard).

% Set number of tents in each collumn.
setNumTentsInCols(ColIn, ColIn, [], _).
setNumTentsInCols(ColIn, ColOut, [Tent|Tents], SizeBoard) :-
	Col is Tent mod SizeBoard,
	nth0(Col, ColIn, Val), 
	NewVal is Val + 1,
	setValInList(Col, ColIn, ColAux, NewVal),
	setNumTentsInCols(ColAux, ColOut, Tents, SizeBoard).
	
% Cria uma lista Out, que e copia de uma lista In em todos os indices
% exceto o especificado em Index que vai ter o valor Value. 
setValInList(0, [_|Tin], [Value|Tin], Value).
setValInList(Index, [Hin|Tin],[Hin|Tout], Value) :-
		Index > 0,
		NextIndex is Index - 1,
		setValInList(NextIndex, Tin, Tout, Value).

% Substitui aleatoriamente alguns valores da lista por -1.	
makeDisappearSomeValues([], []).	
makeDisappearSomeValues([H|T], [H_out|T_out]) :-
	random(0, 3, Num),
	(Num =:= 0,
		H_out = -1;
	Num >= 1,
		H_out = H
	),
	makeDisappearSomeValues(T, T_out).

% Desenha o tabuleiro
printBoard(Board, RowTents, ColTents) :-
    printAuxBoard(Board, RowTents),
	write('['), printDownBorder(ColTents).

printAuxBoard([], []).
printAuxBoard([H|T], [TentsRow|TentsRows]) :-
    write('|'), printRow(H, TentsRow),
    printAuxBoard(T, TentsRows).

printRow([], -1) :- write('['), write('?'), write(']'), nl.
printRow([], TentsRow) :- write('['), write(TentsRow), write(']'), nl.
printRow([H|T], TentsRow) :-
    write(H), write('|'),
    printRow(T, TentsRow).

printDownBorder([-1]) :- write('?]'), nl.
printDownBorder([H]) :- write(H), write(']'), nl. 
printDownBorder([-1|T]) :-
	write('?,'),
	printDownBorder(T).
printDownBorder([H|T]) :-
	write(H), write(','),
	printDownBorder(T).
	
printLegend :-
	write('Legend:'), nl,
	write('T -> tree'), nl,
	write('t -> tent'), nl,
	write('0 -> empty'), nl, nl.

	

