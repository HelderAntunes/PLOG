:- use_module(library(lists)).

% Get piece from board
%	      +      +      +      -
getPiece(Row, Column, Board, Piece):-
	nth1(Row, Board, R),
	nth1(Column, R, Piece).
getPiece([Row,Col], Board, Piece) :- getPiece(Row, Col, Board, Piece).

% Replace board cell (empty ou not) by a piece
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

% Move a piece in board and capture, if possible, enemies
% moveAndCapture( + Color, + RowFrom, + ColFrom, + RowTo, + ColTo, + BoardIn, + BoardOut)
moveAndCapture(Color,RowFrom,ColFrom,RowTo,ColTo,BoardIn,BoardOut, PlayerFrom, PlayerTo):-
    getPiece(RowFrom, ColFrom, BoardIn, PieceFrom),
    PieceFrom = [Color, Legs, _], !,
	thereIsPath([RowFrom,ColFrom], [RowTo,ColTo], Legs, BoardIn),
	getPiece(RowTo, ColTo, BoardIn, PieceTo),
	setPieceWithMorePincers(RowTo, ColTo, PieceFrom, PieceTo, BoardIn, BoardAux, PlayerFrom, PlayerTo),
	setPiece(RowFrom, ColFrom, empty, BoardAux, BoardOut).

setPieceWithMorePincers(RowTo, ColTo, PieceFrom, PieceTo, BoardIn, BoardOut, PlayerFrom, PlayerTo) :-
	PieceTo = empty,
	setPiece(RowTo, ColTo, PieceFrom, BoardIn, BoardOut),
    getColorOfPiece(PieceFrom, ColorFrom), getColorOfEnemy(ColorFrom, ColorTo),
    player(ColorFrom, AFrom, LFrom, PFrom, SFrom),
    PlayerFrom = [ColorFrom, AFrom, LFrom, PFrom, SFrom],
    player(ColorTo, ATo, LTo, PTo, STo),
    PlayerTo = [ColorTo, ATo, LTo, PTo, STo].
setPieceWithMorePincers(RowTo, ColTo, PieceFrom, PieceTo, BoardIn, BoardOut, PlayerOutFrom, PlayerOutTo) :-
	PieceTo \= empty,
	getPincersOfPiece(PieceFrom, PincersFrom), getPincersOfPiece(PieceTo, PincersTo),
	getColorOfPiece(PieceFrom, ColorFrom), getColorOfPiece(PieceTo, ColorTo),
	(PincersFrom > PincersTo, setPiece(RowTo, ColTo, PieceFrom, BoardIn, BoardOut),
    player(ColorFrom, AFrom, LFrom, PFrom, SFrom),
    PlayerIn = [ColorFrom, AFrom, LFrom, PFrom, SFrom],
    player(ColorTo, ATo, LTo, PTo, STo),
    PlayerOutTo = [ColorTo, ATo, LTo, PTo, STo],
	updateScoreOfPlayer(1, PlayerIn, P1), updatePiecesOfPlayer(PieceTo, P1, PlayerOutFrom);
    
	PincersTo > PincersFrom, setPiece(RowTo, ColTo, PieceTo, BoardIn, BoardOut),
    player(ColorFrom, AFrom, LFrom, PFrom, SFrom),
    PlayerOutFrom = [ColorFrom, AFrom, LFrom, PFrom, SFrom],
    player(ColorTo, ATo, LTo, PTo, STo),
    PlayerIn = [ColorTo, ATo, LTo, PTo, STo],
	updateScoreOfPlayer(1,PlayerIn,P1), updatePiecesOfPlayer(PieceFrom, P1, PlayerOutTo);
    
	PincersTo =:= PincersFrom, setPiece(RowTo, ColTo, empty, BoardIn, BoardOut),
    player(ColorFrom, AFrom, LFrom, PFrom, SFrom),
    PlayerInFrom = [ColorFrom, AFrom, LFrom, PFrom, SFrom],
    player(ColorTo, ATo, LTo, PTo, STo),
    PlayerInTo = [ColorTo, ATo, LTo, PTo, STo],
	updateScoreOfPlayer(1, PlayerInFrom, P1), updateScoreOfPlayer(1, PlayerInTo, P2), 
	updatePiecesOfPlayer(PieceTo, P1, PlayerOutFrom), updatePiecesOfPlayer(PieceFrom, P2, PlayerOutTo)).
	
getPincersOfPiece([_, _, Pincers], Pincers).

getColorOfPiece([Color|_], Color).

updateScoreOfPlayer(PointsToSum, [Color, Adaptoids, Legs, Pincers, Score] ,[Color, Adaptoids, Legs, Pincers, NewScore]) :-
	NewScore is Score + PointsToSum.

updatePiecesOfPlayer([_, LegsToAdd, PincersToAdd], [Color, Adaptoids, Legs, Pincers, Score], [Color, NewAdaptoids, NewLegs, NewPincers, Score]) :-
	NewAdaptoids is Adaptoids + 1,
	NewLegs is Legs + LegsToAdd, 
	NewPincers is Pincers + PincersToAdd.

takePiecesFromPlayer([NAdaptoids, NLegs, NPincers], [Color, Adaptoids, Legs, Pincers, Score], [Color, NewAdaptoids, NewLegs, NewPincers, Score]) :-
	NewAdaptoids is Adaptoids - NAdaptoids,
	NewLegs is Legs - NLegs,
	NewPincers is Pincers - NPincers,
	NewAdaptoids >= 0, NewLegs >= 0, NewPincers >= 0.
	
% If there is a path between 'NoInicio' and 'NoFim' with distance less or equal
% to 'DistMax', returns 'yes'.
% thereIsPath( + NoInicio, + NoFim, - Lista, + DistMax, + Board)
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

% Create basic adaptoid of give color
% createAdaptoid( + Color, + Row, + Column, + BoardIn, - BoardOut)
createAdaptoid(Color, Row, Column, BoardIn, BoardOut, PlayerOut):-
    getPiece(Row,Column,BoardIn, Piece),
    Piece = empty, 
	neighborValid(Row, Column, _, _, Color, BoardIn),
    player(Color, Adaptoids, Legs, Pincers, Score),
    PlayerIn = [Color, Adaptoids, Legs, Pincers, Score],
	takePiecesFromPlayer([1, 0, 0], PlayerIn, PlayerOut), !, 
    setPiece(Row,Column,[Color,0,0],BoardIn,BoardOut).
	
neighborValid(Row, Col, NeighborRow, NeighborCol, Color, Board) :-
	connected([Row,Col], [NeighborRow, NeighborCol], Board),
	getPiece(NeighborRow, NeighborCol, Board, [Color|_]).

% Add pincer of given colour to a piece in board
%           +     +      +       +        -
addPincer(Color, Row, Column, BoardIn, BoardOut, PlayerOut):-
   getPiece(Row,Column,BoardIn, Piece),
   Piece = [Color, Legs, Pincers], !,
   Total is Legs + Pincers + 1,
   Total =< 6, 
   player(Color, Adaptoids, Leg, Pin, S),
   PlayerIn = [Color, Adaptoids, Leg, Pin, S],
   takePiecesFromPlayer([0, 0, 1], PlayerIn, PlayerOut), !,
   P1 is Pincers+1,
   P = [Color, Legs, P1],
   setPiece(Row, Column, P, BoardIn, BoardOut).


% Add leg of given colour to a piece in board
%        +     +      +       +        -
addLeg(Color, Row, Column, BoardIn, BoardOut, PlayerOut):-
   getPiece(Row,Column,BoardIn, Piece),
   Piece = [Color, Legs, Pincers], !, 
   Total is Legs + Pincers + 1,
   Total =< 6,
   player(Color, Adaptoids, Leg, Pin, S),
   PlayerIn = [Color, Adaptoids, Leg, Pin, S],
   takePiecesFromPlayer([0, 1, 0],PlayerIn,PlayerOut), !, 
   L is Legs+1,
   P = [Color, L, Pincers],
   setPiece(Row, Column, P, BoardIn, BoardOut).

% Capture starving adaptoids of a given colour
% captureAdaptoids( + Color, + BoardIn, - BoardOut)
captureAdaptoids(Color, BoardIn, BoardOut,PlayerOut) :-
	findall([R,C], getPiece(R,C,BoardIn,[Color|_]), Pieces),
    getColorOfEnemy(Color, ColorEnemy),
    player(ColorEnemy, Adaptoids, Legs, Pincers, Score),
    PlayerIn = [ColorEnemy, Adaptoids, Legs, Pincers, Score],
	tryCaptureAPiece(Pieces, BoardIn, BoardOut, PlayerIn, PlayerOut).

tryCaptureAPiece([], Board, Board, PlayerIn, PlayerIn).
tryCaptureAPiece([[R,C]|Ps], BoardIn, BoardOut, PlayerIn, PlayerOut) :-
	findall([NR,NC], isFreeSpace(R,C,NR,NC,BoardIn), FreeSpacesList),
	length(FreeSpacesList, NumFreeSpaces),
	getNumExtremetiesOfAPiece(R,C,BoardIn,Extremeties),
	captureAdaptoid(R, C, Extremeties, NumFreeSpaces, BoardIn, BoardAux, PlayerIn, P1),
	tryCaptureAPiece(Ps, BoardAux, BoardOut, P1, PlayerOut).
	
captureAdaptoid(R, C, Extremeties, NumFreeSpaces, BoardIn, BoardOut, PlayerIn, PlayerOut) :-
	Extremeties > NumFreeSpaces,
	getPiece(R, C, BoardIn, Piece),
	updateScoreOfPlayer(1, PlayerIn, P1),
	updatePiecesOfPlayer(Piece, P1, PlayerOut),
	setPiece(R, C, empty, BoardIn, BoardOut).
captureAdaptoid(_, _, Extremeties, NumFreeSpaces, BoardIn, BoardIn, PlayerIn, PlayerIn) :-
	Extremeties =< NumFreeSpaces.

getNumExtremetiesOfAPiece(R,C,Board,Extremeties) :-
	getPiece(R,C,Board,[_,Legs,Pincers]),
	Extremeties is Legs + Pincers.	
	
isFreeSpace(Row, Col, NeighborRow, NeighborCol, Board) :-
	connected([Row,Col], [NeighborRow, NeighborCol], Board),
	getPiece(NeighborRow, NeighborCol, Board, empty).
	
getColorOfEnemy(w, b).
getColorOfEnemy(b, w).