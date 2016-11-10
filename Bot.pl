%       +       +      +      -
value(Board, Player, Enemy, Value):-
    countPlayerPieces(Player, Board, NumPieces),
    countPlayerPieces(Enemy, Board, NumPiecesE),
    
    Player = [Color|_],
    Enemy = [ColorEnemy|_],
    countStarvingAdaptoids(ColorEnemy, Board, StarvingE),
    countStarvingAdaptoids(Color, Board, Starving),
    
    countEndangeredAdaptoids(Board, Player, Enemy, Num, NumE),
	
    Value is NumPieces - NumPiecesE + StarvingE - Starving + NumE - Num, !.


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
    Num is 6 + L + P + N1.

% conta o numero de pecas em perigo de serem capturadas:
% do jogador (Num);
% do jogador inimigo(NumE) 
% countEndangeredAdaptoids( + Board, + Player, + Enemy, - Num, - NumE)
countEndangeredAdaptoids(Board, Player, Enemy, Num, NumE):-
    Player = [Color | _],
    Enemy = [ColorEnemy | _],
    findall([R,C], getPiece(R,C,Board,[Color|_]), Pieces),
    findall([R,C], getPiece(R,C,Board,[ColorEnemy|_]), PiecesEnemy),
    countEndangeredAdaptoidsAux(Color, Board, Pieces, PiecesEnemy, NumE),
    countEndangeredAdaptoidsAux(ColorEnemy, Board, PiecesEnemy, Pieces, Num).

% conta o numero de ataques das pecas inimigas as pecas do jogador
% countEndangeredAdaptoidsAux(+Color, + Board, + Pieces, + PiecesEnemy, - Num)
countEndangeredAdaptoidsAux(_, _, _, [], 0).    
countEndangeredAdaptoidsAux(Color, Board, Pieces, [Enemy | PiecesEnemy], Num):-
    countDirectThreats(Color, Board, Pieces, Enemy, N1),
    countEndangeredAdaptoidsAux(Color, Board, Pieces, PiecesEnemy, N2),
    Num is N1 + N2.
   
% conta o numero de ataques de uma peca inimiga contra as pecas do jogador
% countDirectThreats('Color, + Board, + Pieces, + Enemy, - Num)   
countDirectThreats(_, _, [], _, 0).
countDirectThreats(Color, Board, [Piece|Pieces], Enemy, Num):-
    Piece = [RowFrom, ColFrom],
    Enemy = [RowTo, ColTo],
    moveAndCapture(Color,RowFrom,ColFrom,RowTo,ColTo,Board,_, _, _),
    countDirectThreats(Color, Board, Pieces, Enemy, N1),
    Num is N1 + 1 .
countDirectThreats(Color, Board, [_|Pieces], Enemy, Num):-
    countDirectThreats(Color, Board, Pieces, Enemy, Num).
   
%For testing only  
testValue(Value):-
    assert(player(w, 12, 12, 12, 0)), 
    assert(player(b, 12, 12, 12, 0)),
    
    boardToTestEndGame(Board),
    Player1 = [w, 12, 12, 12, 0], 
    Player2 = [b, 12, 12, 12, 0],
    
    countPlayerPieces(Player1, Board, NumPieces1),
    write('Number of white pieces '), write(NumPieces1), nl,
    countPlayerPieces(Player2, Board, NumPieces2),
    write('Number of black pieces '), write(NumPieces2), nl,
    
    countStarvingAdaptoids(w, Board, S1),
    write('Number of starving white '), write(S1), nl,
    countStarvingAdaptoids(b, Board, S2),
    write('Number of starving black '), write(S2), nl,
    
    countEndangeredAdaptoids(Board, Player1, Player2, Num, NumE),
    write('Number of endagered white '), write(Num), nl,
    write('Number of endagered black '), write(NumE), nl,

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

	
% Encontra a melhor jogada possivel no primeiro movimento para o jogador 'Player'.
% Uma jogada e definida como: [RFrom, CFrom, RTo, CTo].
% findBestFirstMove( + Board, + Player, + Enemy, - RFrom, - CFrom, - RTo, - CTo) 
findBestFirstMove(Board, Player, Enemy, RFrom, CFrom, RTo, CTo) :-
	valid_moves(Board, Player, ListOfMoves),
	findBestFirstMoveAux(Board, Player, Enemy, ListOfMoves, 0, 1, 1, MaxValue, IndexOfBestMove),
	write(MaxValue), nl,
	nth1(IndexOfBestMove, ListOfMoves, [RFrom, CFrom, RTo, CTo]).

findBestFirstMoveAux(_, _, _, [], CurrMax, CurrIndex, _, CurrMax, CurrIndex).
findBestFirstMoveAux(Board, Player, Enemy, [Move|Ms], CurrMax, CurrIndexMax, CurrIndex, MaxValue, IndexOfBestMove) :-
	Player = [Color|_],
	Move = [RFrom, CFrom, RTo, CTo],
	moveAndCapture(Color, RFrom, CFrom, RTo, CTo, Board, BoardOut, _, _),
	value(BoardOut, Player, Enemy, Value),
	NewIndex is CurrIndex + 1,
	(Value > CurrMax,
	findBestFirstMoveAux(Board, Player, Enemy, Ms, Value, CurrIndex, NewIndex, MaxValue, IndexOfBestMove);
	Value =< CurrMax,
	findBestFirstMoveAux(Board, Player, Enemy, Ms, CurrMax, CurrIndexMax, NewIndex, MaxValue, IndexOfBestMove)).
	
test_findBestFirstMove :-
    assert(player(w, 12, 12, 12, 0)), 
    assert(player(b, 12, 12, 12, 0)),
    
    boardToTestEndGame(Board),
    Player1 = [w, 12, 12, 12, 0], 
    Player2 = [b, 12, 12, 12, 0],
  
    findBestFirstMove(Board, Player1, Player2, RFrom, CFrom, RTo, CTo),
	
	write(RFrom), write('-'), write(CFrom), 
	write(' -> '), 
	write(RTo), write('-'), write(CTo), nl, nl.

%Choses the best move to be done in this part of the game returns BestMove = [Type,Row,Column].
%bestMoveCreateOrUpdate(+Board, +Player, -BestMove   
bestMoveCreateOrUpdate(Board, Player,BestMove):-
    setof([Value, R, C], valueOfCreation(R, C, Board, Player, Value), ValuesOfCreation),
    last(ValuesOfCreation, BestCreation),
    setof([Value, R, C], valueOfAddLeg(R, C, Board, Player, Value), ValuesOfAddLeg),
    last(ValuesOfAddLeg, BestAddLeg),
    setof([Value, R, C], valueOfAddPincer(R, C, Board, Player, Value), ValuesOfAddPincer),
    last(ValuesOfAddPincer, BestAddPincer),
    chooseBestMoveCreateOrUpdate(BestCreation,BestAddLeg,BestAddPincer,BestMove).
%For the unlikely case all adaptoids have max evolution
bestMoveCreateOrUpdate(Board, Player,['creation',R,C]):-
    setof([Value, R, C], valueOfCreation(R, C, Board, Player, Value), ValuesOfCreation),
    getBest(ValuesOfCreation, BestCreation),
    BestCreation = [_,R,C].
%In case no adaptoids can be created
bestMoveCreateOrUpdate(Board, Player,BestMove):-
    setof([Value, R, C], valueOfAddLeg(R, C, Board, Player, Value), ValuesOfAddLeg),
    last(ValuesOfAddLeg, BestAddLeg),
    setof([Value, R, C], valueOfAddPincer(R, C, Board, Player, Value), ValuesOfAddPincer),
    last(ValuesOfAddPincer, BestAddPincer),
    chooseBestMoveCreateOrUpdate([-10000,0,0],BestAddLeg,BestAddPincer,BestMove).     
 
%Returns list of enemy player status
getEnemy(Color, Enemy):-
    getColorOfEnemy(Color, ColorEnemy),
    player(ColorEnemy, Adaptoids, Legs, Pincers, Score),
    Enemy = [ColorEnemy, Adaptoids, Legs, Pincers, Score].

%Given the coordinates the board and the player returns the value of the creation move
%valueOfCreation(+R, +C, +BoardIn, +Player, -Value)    
valueOfCreation(R, C, BoardIn, Player, Value):-
    Player = [Color | _],
    getEnemy(Color, Enemy),
    createAdaptoidValid(R, C, BoardIn, Player), 
    createAdaptoid(Color, R, C, BoardIn, BoardOut, PlayerOut),
    value(BoardOut, PlayerOut, Enemy, Value).	

%Given the coordinates the board and the player returns the value of adding a leg
%valueOfAddLeg(+R, +C, +BoardIn, +Player, -Value)       
valueOfAddLeg(R, C, BoardIn, Player, Value):-
    Player = [Color | _],
    getEnemy(Color, Enemy),
    addLegValid(R, C, BoardIn, Player), 
    addLeg(Color, R, C, BoardIn, BoardOut, PlayerOut),
    value(BoardOut, PlayerOut, Enemy, Value).	

%Given the coordinates the board and the player returns the value of adding a pincer
%valueOfAddPincer(+R, +C, +BoardIn, +Player, -Value)      
valueOfAddPincer(R, C, BoardIn, Player, Value):-
    Player = [Color | _],
    getEnemy(Color, Enemy),
    addPincerValid(R, C, BoardIn, Player), 
    addPincer(Color, R, C, BoardIn, BoardOut, PlayerOut),
    value(BoardOut, PlayerOut, Enemy, Value).

%choses the best among the best of each type of move returns BestMove=[Type,Row,Column]
%chooseBestMoveCreateOrUpdate(+BestCreation,+BestAddLeg,+BestAddPincer,-BestMove)	
chooseBestMoveCreateOrUpdate([ValCreation,R,C],[ValAddLeg | _],[ValAddPincer | _],['creation',R,C]):-
    ValCreation >= ValAddLeg, ValCreation >= ValAddPincer, ! .
chooseBestMoveCreateOrUpdate([ValCreation |_],[ValAddLeg,R,C],[ValAddPincer | _],['addLeg',R,C]):-
    ValAddLeg >= ValCreation, ValAddLeg >= ValAddPincer, ! .
chooseBestMoveCreateOrUpdate(_,_,[_,R,C],['addPincer',R,C]).

%Generates a random possible move
%randomMoveCreateOrUpdate(+Board, +Player,-Move)
randomMoveCreateOrUpdate(Board, Player,Move):-
    random(1, 4, Type),
    randomMoveCreateOrUpdateAux(Type, Board, Player,Move).

%Generates a random possible move according to type given (Type->action: 1->creation 2->addLeg 3->addPincer) if not possible tries another type of move
%randomMoveCreateOrUpdateAux(+Type, +Board, +Player,-Move)
randomMoveCreateOrUpdateAux(1, Board, Player,['creation',R,C]):-
    valid_moves_createAdaptoid(Board, Player, ListOfMoves),
    ListOfMoves \= [], !,
    random_member([R,C], ListOfMoves).
randomMoveCreateOrUpdateAux(1, Board, Player,Move):-    
    random(2, 4, Type),
    randomMoveCreateOrUpdateAux(Type, Board, Player,Move).
randomMoveCreateOrUpdateAux(2, Board, Player,['addLeg',R,C]):-
    valid_moves_addLeg(Board, Player, ListOfMoves),
    ListOfMoves \= [], !,
    random_member([R,C], ListOfMoves).
randomMoveCreateOrUpdateAux(3, Board, Player,['addPincer',R,C]):-
    valid_moves_addPincer(Board, Player, ListOfMoves),
    ListOfMoves \= [], !,
    random_member([R,C], ListOfMoves).
randomMoveCreateOrUpdateAux(Type, Board, Player,Move):-
    Type =\= 1,
    randomMoveCreateOrUpdateAux(1, Board, Player,Move).

%For testing purposes only
testBestMove(BestMove, Move):-
    assert(player(w, 12, 12, 12, 0)), 
    assert(player(b, 12, 12, 12, 0)),
    
    boardToTestValidMoves(Board),
    Player = [w, 12, 12, 12, 0],
    bestMoveCreateOrUpdate(Board, Player, BestMove),
    randomMoveCreateOrUpdate(Board, Player,Move).

