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

%TODO: Verificar se a peça pode mover-se para as novas coordenadas/registar a captura do inimigo
% Mover uma peca no tabuleiro e capturar, se possivel, pecas inimigas
%                +     +        +      +     +     +        -
moveAndCapture(Color,RowFrom,ColFrom,RowTo,ColTo,BoardIn,BoardOut):-
    getPiece(RowFrom, ColFrom, BoardIn, PieceFrom),
    PieceFrom = [Color, Legs, _], !,
	caminho([RowFrom,ColFrom], [RowTo,ColTo], Legs, BoardIn),
	getPiece(RowTo, ColTo, BoardIn, PieceTo),
	setPieceWithMorePincers(RowTo, ColTo, PieceFrom, PieceTo, BoardIn, BoardAux),
	setPiece(RowFrom, ColFrom, empty, BoardAux, BoardOut).

setPieceWithMorePincers(RowTo, ColTo, PieceFrom, PieceTo, BoardIn, BoardOut) :-
	PieceTo = empty,
	setPiece(RowTo, ColTo, PieceFrom, BoardIn, BoardOut).
	
setPieceWithMorePincers(RowTo, ColTo, PieceFrom, PieceTo, BoardIn, BoardOut) :-
	PieceTo \= empty,
	getPencilsOfPiece(PieceFrom, PencilsFrom),
	getPencilsOfPiece(PieceTo, PencilsTo),
	PencilsFrom > PencilsTo, 
	setPiece(RowTo, ColTo, PieceFrom, BoardIn, BoardOut).
	
setPieceWithMorePincers(RowTo, ColTo, PieceFrom, PieceTo, BoardIn, BoardOut) :-
	PieceTo \= empty,
	getPencilsOfPiece(PieceFrom, PencilsFrom),
	getPencilsOfPiece(PieceTo, PencilsTo),
	PencilsTo > PencilsFrom, 
	setPiece(RowTo, ColTo, PieceTo, BoardIn, BoardOut).

getPencilsOfPiece([_, _, Pencils], Pencils).

% Se existe um caminho entre 'NoInicio' e 'NoFim' com distancia menor ou igual
% a 'DistMax', retorna 'yes'.
% caminho( + NoInicio, + NoFim, - Lista, + DistMax, + Board)
caminho(NoInicio, NoFim, DistMax, Board) :-
	getPiece(NoInicio, Board, P1),
	P1 \= empty,
	getPiece(NoFim, Board, P2),
	(P2 = empty; \+ piecesHaveSameColor(P1,P2)), 
	caminhoAux(NoInicio, NoFim, [NoInicio], _, DistMax, Board).

piecesHaveSameColor([Color1|_],[Color2|_]) :-
	Color1 = Color2.

caminhoAux(NoInicio, NoFim, Lista, ListaFim, N, _) :-
	N >= 1,
	connected(NoInicio, NoFim),
	append(Lista, [NoFim], ListaFim).
caminhoAux(NoInicio, NoFim, Lista, ListaFim, N, Board):-
	N > 1,
	connected(NoInicio, NoInterm),
	NoInterm \= NoFim,
	getPiece(NoInterm, Board, Piece),
	Piece = empty,
	\+(member(NoInterm, Lista)),
	append(Lista, [NoInterm], Lista2),
	N2 is N - 1,
	caminhoAux(NoInterm, NoFim, Lista2, ListaFim, N2, Board).

% Tanto verifica se dois nos sao conexos, como tambem cria conexoes
connected([RowFrom, ColFrom], [RowTo, ColTo]) :- 
	RowFrom =< 3,
	RowTo is RowFrom + 1,
	ColTo is ColFrom,
	validPosition(RowTo, ColTo).
connected([RowFrom, ColFrom], [RowTo, ColTo]) :- 
	RowFrom =< 3,
	RowTo is RowFrom + 1,
	ColTo is ColFrom + 1,
	validPosition(RowTo, ColTo).
connected([RowFrom, ColFrom], [RowTo, ColTo]) :- 
	RowFrom =< 3,
	RowTo is RowFrom - 1,
	ColTo is ColFrom - 1,
	validPosition(RowTo, ColTo).
connected([RowFrom, ColFrom], [RowTo, ColTo]) :-
	RowFrom =< 3,
	RowTo is RowFrom - 1,
	ColTo is ColFrom,
	validPosition(RowTo, ColTo).
connected([RowFrom, ColFrom], [RowTo, ColTo]) :- 
	RowFrom > 4,
	RowTo is RowFrom + 1,
	ColTo is ColFrom - 1,
	validPosition(RowTo, ColTo).
connected([RowFrom, ColFrom], [RowTo, ColTo]) :- 
	RowFrom > 4,
	RowTo is RowFrom + 1,
	ColTo is ColFrom,
	validPosition(RowTo, ColTo).
connected([RowFrom, ColFrom], [RowTo, ColTo]) :- 
	RowFrom > 4,
	RowTo is RowFrom - 1,
	ColTo is ColFrom,
	validPosition(RowTo, ColTo).
connected([RowFrom, ColFrom], [RowTo, ColTo]) :-
	RowFrom > 4,
	RowTo is RowFrom - 1,
	ColTo is ColFrom + 1,
	validPosition(RowTo, ColTo).
connected([RowFrom, ColFrom], [RowTo, ColTo]) :-
	RowFrom =:= 4,
	RowTo is RowFrom + 1,
	ColTo is ColFrom - 1,
	validPosition(RowTo, ColTo).
connected([RowFrom, ColFrom], [RowTo, ColTo]) :-
	RowFrom =:= 4,
	RowTo is RowFrom + 1,
	ColTo is ColFrom,
	validPosition(RowTo, ColTo).
connected([RowFrom, ColFrom], [RowTo, ColTo]) :-
	RowFrom =:= 4,
	RowTo is RowFrom - 1,
	ColTo is ColFrom - 1,
	validPosition(RowTo, ColTo).
connected([RowFrom, ColFrom], [RowTo, ColTo]) :-
	RowFrom =:= 4,
	RowTo is RowFrom - 1,
	ColTo is ColFrom,
	validPosition(RowTo, ColTo).
connected([RowFrom, ColFrom], [RowTo, ColTo]) :- 
	RowTo is RowFrom,
	ColTo is ColFrom + 1,
	validPosition(RowTo, ColTo).
connected([RowFrom, ColFrom], [RowTo, ColTo]) :- 
	RowTo is RowFrom,
	ColTo is ColFrom - 1,
	validPosition(RowTo, ColTo).
	
validPosition(Row, Col) :-
	Row =:= 1,
	Col =< 4,
	Col >= 1.
validPosition(Row, Col) :-
	Row =:= 2,
	Col =< 5,
	Col >= 1.
validPosition(Row, Col) :-
	Row =:= 3,
	Col =< 6,
	Col >= 1.
validPosition(Row, Col) :-
	Row =:= 4,
	Col =< 7,
	Col >= 1.
validPosition(Row, Col) :-
	Row =:= 5,
	Col =< 6,
	Col >= 1.
validPosition(Row, Col) :-
	Row =:= 6,
	Col =< 5,
	Col >= 1.
validPosition(Row, Col) :-
	Row =:= 7,
	Col =< 4,
	Col >= 1.

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