%       +       +      +      -
value(Board, Player, Enemy, Value):-
    countPlayerPieces(Player, Board, NumPieces),
    countPlayerPieces(Enemy, Board, NumPiecesE),
    countStarvingAdaptoids(ColorEnemy, Board, StarvingE),
    countStarvingAdaptoids(Color, Board, Starving),
    Value is NumPieces - NumPiecesE + StarvingE - Starving.

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
    
testValue(Value):-
    boardToTestCaptureHungryAdaptoids(Board),
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

    value(Board, Player1, Player2, Value).

	
	
% valid_moves(+Board, +Player, -ListOfMoves).
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