%       +       +      +      -
value(Board, Player, Enemy, Value):-
    countPlayerPieces(Player, Board, NumPieces),
    countPlayerPieces(Enemy, Board, NumPiecesE),
    
    Player = [Color | _],
    Enemy = [ColorEnemy | _],
    countStarvingAdaptoids(ColorEnemy, Board, StarvingE),
    countStarvingAdaptoids(Color, Board, Starving),
    
    countEndangeredAdaptoids(Board, Player, Enemy, Num, NumE),
    
    Value is NumPieces - NumPiecesE + StarvingE - Starving + NumE - Num.

countStarvingAdaptoids(Color, Board, Num) :-
	findall([R,C], getPiece(R,C,Board,[Color|_]), Pieces),
	findStarvingAdptoids(Pieces, Board, Num).

findStarvingAdptoids([], _, 0).
findStarvingAdptoids([[R,C]|Ps], Board, N1) :-
	findall([NR,NC], isFreeSpace(R,C,NR,NC,Board), FreeSpacesList),
	length(FreeSpacesList, NumFreeSpaces),
	getNumExtremetiesOfAPiece(R,C,Board,Extremeties),
	isAdaptoidHungry(Extremeties, NumFreeSpaces, Flag),
	findStarvingAdptoids(Ps, Board, N),
    N1 is N + Flag.
    
isAdaptoidHungry(Extremeties, NumFreeSpaces, 1) :-
	Extremeties > NumFreeSpaces.
isAdaptoidHungry(Extremeties, NumFreeSpaces, 0) :-
	Extremeties =< NumFreeSpaces.
    
countPlayerPieces(Player, Board, NumPieces):-
    Player = [Color, Adaptoids, Legs, Pincers, Score],
    findall([L,P], getPiece(_,_,Board,[Color,L,P]), Pieces),
    countPiecesOnBoard(Pieces, NumPiecesOnBoard),
    NumPieces is Adaptoids + Legs + Pincers + Score + NumPiecesOnBoard .

countPiecesOnBoard([], 0).    
countPiecesOnBoard([[L,P]|Pieces], Num):-
    countPiecesOnBoard(Pieces, N1),
    Num is 1 + L + P + N1.

% conta o numero de pecas em perigo de serem capturadas:
% do jogador (Num);
% do jogador inimigo(NumE) 
% countEndangeredAdaptoids( + Board, + Player, + Enemy, - Num, - NumE)
countEndangeredAdaptoids(Board, Player, Enemy, Num, NumE):-
    Player = [Color | _],
    Enemy = [ColorEnemy | _],
    findall([R,C], getPiece(R,C,Board,[Color|_]), Pieces),
    findall([R2,C2], getPiece(R2,C2,Board,[ColorEnemy|_]), PiecesEnemy),
    countEndangeredAdaptoidsAux(Board, Pieces, PiecesEnemy, Num),
    countEndangeredAdaptoidsAux(Board, PiecesEnemy, Pieces, NumE).

% conta o numero de ataques das pecas inimigas as pecas do jogador
% countEndangeredAdaptoidsAux(+ Board, + Pieces, + PiecesEnemy, - Num)
countEndangeredAdaptoidsAux(_, [], _, 0).    
countEndangeredAdaptoidsAux(Board, [Piece|Pieces], Enemies, Num):-
    pieceInDanger(Board, Piece, Enemies),
    countEndangeredAdaptoidsAux(Board, Pieces, Enemies, N2),
    Num is 1 + N2.
countEndangeredAdaptoidsAux(Board, [_|Pieces], Enemies, Num):-
    countEndangeredAdaptoidsAux(Board, Pieces, Enemies, Num).

% retorna 'yes' se houver pelo menos 1 ataque direto a peca 'Piece'
% pieceInDanger(+ Board, + Piece, + Enemies)
pieceInDanger(Board, Piece, [Enemy|_]):-
    Piece = [RowTo, ColTo],
    Enemy = [RowFrom, ColFrom],
    checkIfIsPossibleAtack(RowFrom,ColFrom,RowTo,ColTo,Board).
pieceInDanger(Board, Piece, [_|Enemies]):-
	pieceInDanger(Board, Piece, Enemies).

% retorna 'yes' se e possivel uma peca capturar outra. caso contrario retorna 'no'.
checkIfIsPossibleAtack(RowFrom,ColFrom,RowTo,ColTo,Board):-
    getPiece(RowFrom, ColFrom, Board, PieceFrom),
    PieceFrom = [_, Legs, Pincers], !,
	thereIsPath([RowFrom,ColFrom], [RowTo,ColTo], Legs, Board),
	getPiece(RowTo, ColTo, Board, PieceTo),
	PieceTo = [_, _, PincersTo],
	Pincers > PincersTo.
    
testValue(Value):-
    boardToTestEndGame(Board),
    Player1 = [w, 12, 12, 12, 0], 
    Player2 = [b, 12, 12, 12, 0],
    
    countPlayerPieces(Player1, Board, NumPieces1),
    write(NumPieces1), nl,
    countPlayerPieces(Player2, Board, NumPieces2),
    write(NumPieces2), nl,
    
    countStarvingAdaptoids(w, Board, S1),
    write(S1), nl,
    countStarvingAdaptoids(b, Board, S2),
    write(S2), nl,
    
    countEndangeredAdaptoids(Board, Player1, Player2, Num, NumE),
    write(Num), nl,
    write(NumE), nl,

    value(Board, Player1, Player2, Value).

% valid_moves(+Board, +Player, -ListOfMoves). 
% Move = [RFrom, CFrom, RTo, CTo].
valid_moves(Board, [Color|_], ListOfMoves) :-
	findall([R,C], getPiece(R,C,Board,[Color|_]), Pieces),
	valid_moves_aux(Pieces, Board, ListOfMoves2),
	sort(ListOfMoves2, ListOfMoves).
	
valid_moves_aux([], _, []).
valid_moves_aux([[RFrom,CFrom]|Ps], Board, ListOfMoves) :-
	getPiece(RFrom, CFrom, Board, [_, Legs, _]),
	findall([RFrom, CFrom, RTo, CTo], thereIsPath([RFrom, CFrom], [RTo, CTo], Legs, Board), L1), 
	valid_moves_aux(Ps, Board, L2),
	append(L1, L2, ListOfMoves). 

% valid_moves_createAdaptoid(+ Board, + Player, - ListOfMoves)	
% Move = [Row, Col]
valid_moves_createAdaptoid(Board, Player, ListOfMoves) :-
	findall([R, C], createAdaptoidValid(R, C, Board, Player), ListOfMoves2),
	sort(ListOfMoves2, ListOfMoves).
	
createAdaptoidValid(Row, Column, Board, [Color, Adaptoids|_]) :-
	Adaptoids >= 1,
	getPiece(Row, Column, Board, Piece),
    Piece = empty, 
	neighborValid(Row, Column, _, _, Color, Board).

% valid_moves_addLeg(Board, Player, ListOfMoves)
% Move = [Row, Col]
valid_moves_addLeg(Board, Player, ListOfMoves) :-
	findall([R, C], addLegValid(R, C, Board, Player), ListOfMoves2),
	sort(ListOfMoves2, ListOfMoves).
	
addLegValid(Row, Column, Board, [Color, _, Legs |_]) :-
	Legs >= 1,
	getPiece(Row,Column,Board, Piece),
	Piece = [Color, Legs2, Pincers],
	Total is Legs2 + Pincers + 1,
	Total =< 6.
	
% valid_moves_addPincer(Board, Player, ListOfMoves)
% Move = [Row, Col]
valid_moves_addPincer(Board, Player, ListOfMoves) :-
	findall([R, C], addPincerValid(R, C, Board, Player), ListOfMoves2),
	sort(ListOfMoves2, ListOfMoves).

addPincerValid(Row, Column, Board, [Color, _, _, Pincers |_]) :-
	Pincers >= 1,
	getPiece(Row,Column,Board, Piece),
	Piece = [Color, Legs, Pincers2],
	Total is Legs + Pincers2 + 1,
	Total =< 6.
	
	
