:- use_module(library(lists)).

board(
	[
	[empty, empty, empty, empty], 
	[[w, 0, 0], empty, empty, empty, empty],
	[empty, empty, empty, empty, [b, 3, 1], empty],
	[empty, empty, [b, 0, 6], empty, empty, empty, empty],
	[empty, empty, empty, empty, empty, empty],
	[empty, empty, empty, [w, 0, 2], empty],
	[empty, [w, 1, 1], empty, empty] 
	]).
		
		
testPrintBoard(1) :- board(X), printBoard(X).

printBoard(B) :- 
	printBlanckChars(15), 
	printTopBoard(4), 
	nl,
	printBoardRows(B, 1), 
	printBlanckChars(14), 
	printBottomBoard(4).
					
printTopBoard(0).
printTopBoard(N) :-  
	N > 0,
	NumColumn is 5 - N,
	write('  / \\ c'), write(NumColumn),
	N1 is N-1, 
	printTopBoard(N1).

printBottomBoard(0).
printBottomBoard(N) :- 
	N > 0, 
	write('   \\ /  '), 
	N1 is N-1, 
	printBottomBoard(N1).

printBoardRows([], _).
printBoardRows([R|Rs], NumRow) :- 
	printRow(R, NumRow),
	nl,
	N1 is NumRow + 1,
	printBoardRows(Rs, N1).
									
printRow(R, NumRow) :- 
	NumRow < 4,
	Nspaces is (4 - NumRow) * 4 + 2,
	printBlanckChars(Nspaces),
	printTopCells(R, NumRow),
	nl,
	N2spaces is (4 - NumRow) * 4,
	printBlanckChars(N2spaces), 
	write('r'), write(NumRow), write('|'),
	printMidCells(R), 
	nl,
	N3spaces is (4 - NumRow - 1) * 4 + 2,
	printBlanckChars(N3spaces), 
	write('   / \\ '),
	printDownCells(R, NumRow),
	NumColumn is 5 + NumRow - 1,
	write('c'), write(NumColumn).
printRow(R, NumRow) :- 
	NumRow =:= 4,
	printBlanckChars(2), printTopCells(R, NumRow),
	nl,
	write('r'), write(NumRow), write('|'),
	printMidCells(R),
	nl,
	printBlanckChars(2), printDownCells(R, NumRow).
printRow(R, NumRow) :- 
	NumRow > 4,
	Nspaces is (NumRow - 5) * 4 + 2,
	printBlanckChars(Nspaces),
	write('   \\ / '),
	printTopCells(R, NumRow),
	nl,
	Nspaces2 is (NumRow - 4) * 4,
	printBlanckChars(Nspaces2),
	write('r'), write(NumRow), write('|'),
	printMidCells(R),
	nl,
	Nspaces3 is Nspaces2 + 2, 
	printBlanckChars(Nspaces3), 
	printDownCells(R, NumRow).
					
printTopCells([], _).							
printTopCells([Cell|Cells], NumRow) :- 
	NumRow < 4,
	write(' / '), 
	printTopCell(Cell),
	write(' \\'), 
	printTopCells(Cells, NumRow).											
printTopCells([Cell|Cells], NumRow) :- 
	NumRow =:= 4,
	write(' / '),
	printTopCell(Cell),
	write(' \\'),
	printTopCells(Cells, NumRow).										
printTopCells([Cell|Cells], NumRow) :- 
	NumRow > 4,
	printTopCell(Cell),
	write(' \\ / '),
	printTopCells(Cells, NumRow).
									
printMidCells([]).										
printMidCells([Cell|Cells]) :- 
	write('  '),
	printMidCell(Cell),
	write('  |'), 
	printMidCells(Cells).
					
					
printDownCells([], _).
printDownCells([Cell|Cells], NumRow) :- 
	NumRow < 4,
	printDownCell(Cell),
	write(' / \\ '), 
	printDownCells(Cells, NumRow).																
printDownCells([Cell|Cells], NumRow) :- 
	NumRow =:= 4,
	write(' \\ '),
	printDownCell(Cell),
	write(' /'),
	printDownCells(Cells, NumRow).										
printDownCells([Cell|Cells], NumRow) :- 
	NumRow > 4,
	write(' \\ '),
	printDownCell(Cell),
	write(' /'),
	printDownCells(Cells, NumRow).
												
printTopCell(empty) :- write('   ').
printTopCell([_, Legs, _]) :-
	write('L='), write(Legs). 

printMidCell(empty) :- write('   ').
printMidCell([Color, _, _]) :- 
	write(' '), 
	write(Color),
	write(' ').
																
printDownCell(empty) :- write('   ').
printDownCell([_, _, Pencils]) :- 
	write('P='), write(Pencils).
	
printBlanckChars(0).
printBlanckChars(N) :- 
	N > 0, 
	write(' '), 
	N1 is N-1, 
	printBlanckChars(N1).

% Obter uma peca do tabuleiro
%	      +      +      +      -
getPiece(Row, Column, Board, Piece):-
	nth1(Row, Board, R),
	nth1(Column, R, Piece).

% Substituir uma casa do tabuleiro (vazia ou preenchida) por uma peca
%         +      +       +       +       -
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

%TODO: Verificar se a peça pode mover-se para as novas coordenadas/registar a captura do inimigo
% Mover uma peca no tabuleiro e capturar, se possivel, pecas inimigas
%                +     +        +      +     +     +        -
moveAndCapture(Color,RowFrom,ColFrom,RowTo,ColTo,BoardIn,BoardOut):-
    getPiece(RowFrom,ColFrom,BoardIn, Piece),
    Piece = [Color|_], !,
    validDestination(Piece,RowFrom,ColFrom,RowTo,ColTo,BoardIn, BoardOut).
    
validDestination(Piece,RowFrom,ColFrom,RowTo,ColTo,BoardIn, BoardOut):-
    getPiece(RowTo,ColTo,BoardIn, OtherPiece),
    OtherPiece  = empty,
    setPiece(RowFrom, ColFrom, empty, BoardIn, Board),
    setPiece(RowTo, ColTo, Piece, Board, BoardOut).
    
validDestination(Piece,RowFrom,ColFrom,RowTo,ColTo,BoardIn, BoardOut):-
    getPiece(RowTo,ColTo,BoardIn, OtherPiece),
    Piece = [C1,_,P1],
    OtherPiece = [C2,_,P2],
    C1 \= C2,
    P1 > P2,
    setPiece(RowFrom, ColFrom, empty, BoardIn, Board),
    setPiece(RowTo, ColTo, Piece, Board, BoardOut).
    
validDestination(Piece,RowFrom,ColFrom,RowTo,ColTo,BoardIn, BoardOut):-
    getPiece(RowTo,ColTo,BoardIn, OtherPiece),
    Piece = [C1,_,P1],
    OtherPiece = [C2,_,P2],
    C1 \= C2,
    P1 =:= P2,
    setPiece(RowFrom, ColFrom, empty, BoardIn, Board),
    setPiece(RowTo, ColTo, empty, Board, BoardOut).

calcDist(Row,Col,Row,Col, 0).
calcDist(Row,ColFrom,Row,ColTo, Dist):-
    ColFrom < ColTo,
    Col is ColFrom + 1,
    write(Row), write(' '), write(Col),
    nl,
    calcDist(Row, Col, Row, ColTo, D),
    Dist is D+1.
calcDist(Row,ColFrom,Row,ColTo, Dist):-
    ColFrom > ColTo,
    Col is ColFrom - 1,
    write(Row), write(' '), write(Col),
    nl,
    calcDist(Row, Col, Row, ColTo, D),
    Dist is D+1.
    
calcDist(RowFrom,ColFrom,RowTo,ColTo, Dist):-
    RowFrom < RowTo,
    R is RowFrom+1,
    RowFrom < 4,
    ColFrom > ColTo,
    write(R), write(' '), write(ColFrom),
    nl,
    calcDist(R, ColFrom, RowTo, ColTo, D),
    Dist is D+1.

calcDist(RowFrom,ColFrom,RowTo,ColTo, Dist):-
    RowFrom < RowTo,
    R is RowFrom+1,
    RowFrom < 4,
    ColFrom =< ColTo,
    Col is ColFrom+1,
    write(R), write(' '), write(Col),
    nl,
    calcDist(R, Col, RowTo, ColTo, D),
    Dist is D+1.
    
calcDist(RowFrom,ColFrom,RowTo,ColTo, Dist):-
    RowFrom < RowTo,
    R is RowFrom+1,
    RowFrom >= 4,
    ColFrom > ColTo,
    Col is ColFrom -1,
    write(R), write(' '), write(Col),
    nl,
    calcDist(R, Col, RowTo, ColTo, D),
    Dist is D+1.

calcDist(RowFrom,ColFrom,RowTo,ColTo, Dist):-
    RowFrom < RowTo,
    R is RowFrom+1,
    RowFrom >= 4,
    ColFrom < ColTo,
    write(R), write(' '), write(ColFrom),
    nl,
    calcDist(R, ColFrom, RowTo, ColTo, D),
    Dist is D+1.
    
calcDist(RowFrom,ColFrom,RowTo,ColTo, Dist):-
    RowFrom > RowTo,
    R is RowFrom-1,
    RowFrom =< 4,
    ColFrom > ColTo,
    Col is Col - 1,
    write(R), write(' '), write(Col),
    nl,
    calcDist(R, Col, RowTo, ColTo, D),
    Dist is D+1.

calcDist(RowFrom,ColFrom,RowTo,ColTo, Dist):-
    RowFrom > RowTo,
    R is RowFrom-1,
    RowFrom =< 4,
    ColFrom =< ColTo,
    write(R), write(' '), write(ColFrom),
    nl,
    calcDist(R, ColFrom, RowTo, ColTo, D),
    Dist is D+1.
    
calcDist(RowFrom,ColFrom,RowTo,ColTo, Dist):-
    RowFrom > RowTo,
    R is RowFrom-1,
    RowFrom > 4,
    ColFrom =< ColTo,
    write(R), write(' '), write(ColFrom),
    nl,
    calcDist(R, ColFrom, RowTo, ColTo, D),
    Dist is D+1.
    
calcDist(RowFrom,ColFrom,RowTo,ColTo, Dist):-
    RowFrom > RowTo,
    R is RowFrom-1,
    RowFrom > 4,
    ColFrom > ColTo,
    write(R), write(' '), write(ColFrom),
    nl,
    calcDist(R, ColFrom, RowTo, ColTo, D),
    Dist is D+1.


    

% Criar um adaptoid basico de uma cor definida
%                +     +      +       +        -
createAdaptoid(Color, Row, Column, BoardIn, BoardOut):-
    getPiece(Row,Column,BoardIn, Piece),
    Piece = empty, !,
    isValidPosition(Color,Row,Column, BoardIn),
    setPiece(Row,Column,[Color,0,0],BoardIn,BoardOut).
    
isValidPosition(Color, Row, Column, Board):-
    Col is Column-1,
    getPiece(Row,Col,Board, Piece),
    Piece = [Color|_],
    write('Col-1').
isValidPosition(Color, Row, Column, Board):-
    Col is Column+1,
    getPiece(Row,Col,Board, Piece),
    Piece = [Color|_],
    write('Col+1').
isValidPosition(Color, Row, Column, Board):-
    R is Row-1,
    getPiece(R,Column,Board, Piece),
    Piece = [Color|_],
    write('Row-1').
isValidPosition(Color, Row, Column, Board):-
    Row > 4,
    R is Row-1,
    Col is Column+1,
    getPiece(R,Col,Board, Piece),
    Piece = [Color|_].
isValidPosition(Color, Row, Column, Board):-
    Row =< 4,
    R is Row-1,
    Col is Column-1,
    getPiece(R,Col,Board, Piece),
    Piece = [Color|_],
    write('Row-1 Col-1').
isValidPosition(Color, Row, Column, Board):-
    R is Row+1,
    getPiece(R,Column,Board, Piece),
    Piece = [Color|_],
    write('Row+1').
isValidPosition(Color, Row, Column, Board):-
    Row > 4,
    R is Row+1,
    Col is Column-1,
    getPiece(R,Col,Board, Piece),
    Piece = [Color|_],
    write('Row+1 Col-1').
isValidPosition(Color, Row, Column, Board):-
    Row =< 4,
    R is Row+1,
    Col is Column+1,
    getPiece(R,Col,Board, Piece),
    Piece = [Color|_],
    write('Row+1 Col+1').

% Adicionar uma tenaz de uma determinada cor a uma peca do tabuleiro
%           +     +      +       +        -
addPincer(Color, Row, Column, BoardIn, BoardOut):-
   getPiece(Row,Column,BoardIn, Piece),
   Piece = [Color, Legs, Pincers], !,
   Total is Legs + Pincers + 1,
   Total < 6, !,
   P1 is Pincers+1,
   P = [Color, Legs, P1],
   setPiece(Row, Column, P, BoardIn, BoardOut).

% Adicionar uma perna de uma determinada cor a uma peca do tabuleiro
%        +     +      +       +        -
addLeg(Color, Row, Column, BoardIn, BoardOut):-
   getPiece(Row,Column,BoardIn, Piece),
   Piece = [Color, Legs, Pincers], !, 
   Total is Legs + Pincers + 1,
   Total < 6, !,
   L is Legs+1,
   P = [Color, L, Pincers],
   setPiece(Row, Column, P, BoardIn, BoardOut).

% Capturar adaptoid's com uma determinada cor.
%                  +       +        -
captureAdpatoids(Color, BoardIn, BoardOut).