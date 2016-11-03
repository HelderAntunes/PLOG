:- use_module(library(lists)).

% Obter uma peca do tabuleiro
%	      +      +      +      -
getPiece(Row, Column, Board, Piece):-
	nth1(Row, Board, R),
	nth1(Column, R, Piece).
getPiece([Row,Col], Board, Piece) :- getPiece(Row, Col, Board, Piece).

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

%TODO: registar a captura do inimigo
% Mover uma peca no tabuleiro e capturar, se possivel, pecas inimigas
% moveAndCapture( + Color, + RowFrom, + ColFrom, + RowTo, + ColTo, + BoardIn, + BoardOut)
moveAndCapture(Color,RowFrom,ColFrom,RowTo,ColTo,BoardIn,BoardOut):-
    getPiece(RowFrom, ColFrom, BoardIn, PieceFrom),
    PieceFrom = [Color, Legs, _], !,
	thereIsPath([RowFrom,ColFrom], [RowTo,ColTo], Legs, BoardIn),
	getPiece(RowTo, ColTo, BoardIn, PieceTo),
	setPieceWithMorePincers(RowTo, ColTo, PieceFrom, PieceTo, BoardIn, BoardAux),
	setPiece(RowFrom, ColFrom, empty, BoardAux, BoardOut).

setPieceWithMorePincers(RowTo, ColTo, PieceFrom, PieceTo, BoardIn, BoardOut) :-
	PieceTo = empty,
	setPiece(RowTo, ColTo, PieceFrom, BoardIn, BoardOut).
setPieceWithMorePincers(RowTo, ColTo, PieceFrom, PieceTo, BoardIn, BoardOut) :-
	PieceTo \= empty,
	getPincersOfPiece(PieceFrom, PincersFrom), getPincersOfPiece(PieceTo, PincersTo),
	getColorOfPiece(PieceFrom, ColorFrom), getColorOfPiece(PieceTo, ColorTo),
	(PincersFrom > PincersTo, setPiece(RowTo, ColTo, PieceFrom, BoardIn, BoardOut),
	updateScoreOfPlayer(ColorFrom, 1);
	PincersTo > PincersFrom, setPiece(RowTo, ColTo, PieceTo, BoardIn, BoardOut),
	updateScoreOfPlayer(ColorTo, 1);
	PincersTo =:= PincersFrom, setPiece(RowTo, ColTo, empty, BoardIn, BoardOut),
	updateScoreOfPlayer(ColorFrom, 1), updateScoreOfPlayer(ColorTo, 1)).
	
getPincersOfPiece([_, _, Pincers], Pincers).

getColorOfPiece([Color|_], Color).

updateScoreOfPlayer(Color, PointsToSum) :-
	player(Color, Adaptoids, Legs, Pincers, Score),
	NewScore is Score + PointsToSum,
	retract(player(Color, Adaptoids, Legs, Pincers, Score)),
	assert(player(Color, Adaptoids, Legs, Pincers, NewScore)).

% Se existe um caminho entre 'NoInicio' e 'NoFim' com distancia menor ou igual
% a 'DistMax', retorna 'yes'.
% caminho( + NoInicio, + NoFim, - Lista, + DistMax, + Board)
thereIsPath(NoInicio, NoFim, DistMax, Board) :-
	getPiece(NoInicio, Board, P1),
	P1 \= empty,
	getPiece(NoFim, Board, P2),
	(P2 = empty; \+ piecesHaveSameColor(P1,P2)), 
	caminhoAux(NoInicio, NoFim, [NoInicio], _, DistMax, Board).

piecesHaveSameColor([Color1|_],[Color2|_]) :-
	Color1 = Color2.

caminhoAux(NoInicio, NoFim, Lista, ListaFim, N, Board) :-
	N >= 1,
	connected(NoInicio, NoFim, Board),
	append(Lista, [NoFim], ListaFim).
caminhoAux(NoInicio, NoFim, Lista, ListaFim, N, Board):-
	N > 1,
	connected(NoInicio, NoInterm, Board),
	NoInterm \= NoFim,
	getPiece(NoInterm, Board, Piece),
	Piece = empty,
	\+(member(NoInterm, Lista)),
	append(Lista, [NoInterm], Lista2),
	N2 is N - 1,
	caminhoAux(NoInterm, NoFim, Lista2, ListaFim, N2, Board).
	
connected([RowFrom, ColFrom], [RowTo, ColTo], Board) :-
	logicToCliCoords(RowFrom, ColFrom, RowFC, ColFC),
	generateEdges(RowFC, ColFC, RowTC, ColTC),
	cliToLogicCoords(RowTC, ColTC, RowAux, ColAux),
	RowTo is RowAux, ColTo is ColAux,
	validPosition(RowTo, ColTo, Board).
	
generateEdges(RowFromCli, ColFromCli, RowToCli, ColToCli) :-
	RowToCli is RowFromCli - 1, ColToCli is ColFromCli - 1.
generateEdges(RowFromCli, ColFromCli, RowToCli, ColToCli) :-
	RowToCli is RowFromCli - 1, ColToCli is ColFromCli.
generateEdges(RowFromCli, ColFromCli, RowToCli, ColToCli) :-
	RowToCli is RowFromCli, ColToCli is ColFromCli - 1.
generateEdges(RowFromCli, ColFromCli, RowToCli, ColToCli) :-
	RowToCli is RowFromCli, ColToCli is ColFromCli + 1.
generateEdges(RowFromCli, ColFromCli, RowToCli, ColToCli) :-
	RowToCli is RowFromCli + 1, ColToCli is ColFromCli.
generateEdges(RowFromCli, ColFromCli, RowToCli, ColToCli) :-
	RowToCli is RowFromCli + 1, ColToCli is ColFromCli + 1.
	
validPosition(R, C, Board) :- getPiece(R, C, Board, _).

% Criar um adaptoid basico de uma cor definida
% createAdaptoid( + Color, + Row, + Column, + BoardIn, - BoardOut)
createAdaptoid(Color, Row, Column, BoardIn, BoardOut):-
    getPiece(Row,Column,BoardIn, Piece),
    Piece = empty, !,
	neighborValid(Row, Column, _, _, Color, BoardIn),
    setPiece(Row,Column,[Color,0,0],BoardIn,BoardOut).
	
neighborValid(Row, Col, NeighborRow, NeighborCol, Color, Board) :-
	connected([Row,Col], [NeighborRow, NeighborCol], Board),
	getPiece(NeighborRow, NeighborCol, Board, [Color|_]).

% Adicionar uma tenaz de uma determinada cor a uma peca do tabuleiro
%           +     +      +       +        -
addPincer(Color, Row, Column, BoardIn, BoardOut):-
   getPiece(Row,Column,BoardIn, Piece),
   Piece = [Color, Legs, Pincers], !,
   Total is Legs + Pincers + 1,
   Total =< 6, !,
   P1 is Pincers+1,
   P = [Color, Legs, P1],
   setPiece(Row, Column, P, BoardIn, BoardOut).

% Adicionar uma perna de uma determinada cor a uma peca do tabuleiro
%        +     +      +       +        -
addLeg(Color, Row, Column, BoardIn, BoardOut):-
   getPiece(Row,Column,BoardIn, Piece),
   Piece = [Color, Legs, Pincers], !, 
   Total is Legs + Pincers + 1,
   Total =< 6, !,
   L is Legs+1,
   P = [Color, L, Pincers],
   setPiece(Row, Column, P, BoardIn, BoardOut).

% Capturar adaptoid's com fome de uma determinada cor.
% captureAdaptoids( + Color, + BoardIn, - BoardOut)
captureAdaptoids(Color, BoardIn, BoardOut) :-
	findall([R,C], getPiece(R,C,BoardIn,[Color|_]), Pieces),
	tryCaptureAPiece(Pieces, BoardIn, BoardOut).

tryCaptureAPiece([], Board, Board).
tryCaptureAPiece([[R,C]|Ps], BoardIn, BoardOut) :-
	findall([NR,NC], isFreeSpace(R,C,NR,NC,BoardIn), FreeSpacesList),
	length(FreeSpacesList, NumFreeSpaces),
	getNumExtremetiesOfAPiece(R,C,BoardIn,Extremeties),
	captureAdaptoid(R, C, Extremeties, NumFreeSpaces, BoardIn, BoardAux),
	tryCaptureAPiece(Ps, BoardAux, BoardOut).
	
captureAdaptoid(R, C, Extremeties, NumFreeSpaces, BoardIn, BoardOut) :-
	Extremeties > NumFreeSpaces,
	getPiece(R, C, BoardIn, Piece),
	getColorOfPiece(Piece, Color),
	getColorOfEnemy(Color, ColorEnemy),
	updateScoreOfPlayer(ColorEnemy, 1),
	setPiece(R, C, empty, BoardIn, BoardOut).
captureAdaptoid(_, _, Extremeties, NumFreeSpaces, BoardIn, BoardIn) :-
	Extremeties =< NumFreeSpaces.

getNumExtremetiesOfAPiece(R,C,Board,Extremeties) :-
	getPiece(R,C,Board,[_,Legs,Pincers]),
	Extremeties is Legs + Pincers.	
	
isFreeSpace(Row, Col, NeighborRow, NeighborCol, Board) :-
	connected([Row,Col], [NeighborRow, NeighborCol], Board),
	getPiece(NeighborRow, NeighborCol, Board, empty).
	
getColorOfEnemy(w, b).
getColorOfEnemy(b, w).
