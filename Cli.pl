:- dynamic board/1.
:- dynamic turnColor/1.
:- dynamic player/5.
:- dynamic computerLevel/1.

:- include('Logic.pl').
:- include('BoardsForTest.pl').
:- include('Bot.pl').
		
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
	
% logicToCliCoords(LogicRow, LogicCol, CliRow, CliCol)
logicToCliCoords(R, C, R, C) :- R =< 4.
logicToCliCoords(R, C, R, CliC) :-
	R > 4,
	CliC is C + R - 4.
	
% CliToLogicCoords(CliRow, CliCol, LogicRow, LogicCol)
cliToLogicCoords(R, C, R, C) :- R =< 4.
cliToLogicCoords(R, C, R, LogC) :-
	R > 4,
	LogC is C - (R - 4).
    
testEnd(_) :- 
    player(w, _, _, _, Score),
    Score > 4.
testEnd(_) :- 
    player(b, _, _, _, Score),
    Score > 4.
testEnd(Board) :-
    findall([R,C], getPiece(R,C,Board,[w|_]), Pieces),
    Pieces = [].
testEnd(Board) :-
    findall([R,C], getPiece(R,C,Board,[b|_]), Pieces),
	Pieces = [].
    
showResults:- 
    player(w, _, _, _, WhiteScore),
    player(b, _, _, _, BlackScore),
    write('White '), write(WhiteScore), write(' - '), write(BlackScore), write(' Black'), nl,
    writeWhoWon(WhiteScore,BlackScore).
    
writeWhoWon(WhiteScore,BlackScore):-
    WhiteScore > BlackScore, !,
    write('White won!!!'), nl .
writeWhoWon(WhiteScore,BlackScore):-
    WhiteScore < BlackScore, !,
    write('Black won!!!'), nl .
%If captures are equal the last to move wins
writeWhoWon(_,_):-
    turnColor(b),
    write('White won!!!'), nl .
writeWhoWon(_,_):-
    turnColor(w),
    write('Black won!!!'), nl . 

setLevel(hh):- !.
setLevel(Type):-
    Type \= hh,
    write('Enter computer level (1-easy 2-hard): '),
    read(Option),
    assert(computerLevel(Option)).
	
initGame :- 
	write('***********************************'), nl,
	write('*                                 *'), nl,
	write('*          Adaptoid Game          *'), nl,
	write('*                                 *'), nl,
	write('*              PLAY               *'), nl, 
	write('*       1 - human vs human        *'), nl,
	write('*       2 - human vs cpu          *'), nl,
	write('*       3 - cpu vs cpu            *'), nl,
	write('*       4 - exit                  *'), nl,
	write('*                                 *'), nl,
	write('***********************************'), nl,
	write('Option: '), read(Option), 
	select_mode_game(Option).


select_mode_game(1) :-
	game(hh).
select_mode_game(2) :-
	game(hc).
select_mode_game(3) :-
	game(cc).
select_mode_game(4).
    
% game(Type)
game(Type):-
    %inicializações
	boardInitGame(InitBoard), 
    assert(board(InitBoard)),
    % player(color, adaptoids, legs, pincers, score)
	assert(player(w, 12, 12, 12, 0)), 
    assert(player(b, 12, 12, 12, 0)),
    assert(turnColor(w)),
    setLevel(Type),
    %ciclo de jogo
    repeat,
        turnColor(ColorIn),
        once(play(Type, ColorIn, ColorOut)),
        retract(turnColor(ColorIn)),
        assert(turnColor(ColorOut)), 
		board(B),
        testEnd(B),
    showResults,
    retract(board(_)),
    retract(player(w,_,_,_,_)),
    retract(player(b,_,_,_,_)), 
	retract(turnColor(_)),
    retract(computerLevel(_)).
    
% writeWhoIsPlaying(Color)
writeWhoIsPlaying(w):-
    write('White playing').
writeWhoIsPlaying(b):-
    write('Black playing').

% play(Type, ColorIn, ColorOut)
play(hh, ColorIn, ColorOut):-
    getColorOfEnemy(ColorIn, ColorOut),
    board(BoardIn),
    %jogada
	showScores, nl,
	showMaterialOfPlayers, nl,
    writeWhoIsPlaying(ColorIn), nl,
    printBoard(BoardIn),
    nl,
    userMoveAndCapture(ColorIn, BoardIn, Board1, PlayerFrom, PlayerTo), 
    updatePlayer(PlayerFrom),
    updatePlayer(PlayerTo),
	(testEnd(Board1), retract(board(BoardIn)), assert(board(Board1));
    nl,
	(playerStockExpired(ColorIn), captureAdaptoids(ColorOut, Board1, BoardOut);
    write('Enter option (1-create new 2-add pincer 3-add leg): '),
    read(Option),
    userCreateOrUpdate(Option, ColorIn, Board1, Board2, PlayerOut),
    updatePlayer(PlayerOut),
	captureAdaptoids(ColorOut, Board2, BoardOut,PlayerOut2)),
    updatePlayer(PlayerOut2),
    %Update board
    retract(board(BoardIn)),
    assert(board(BoardOut))).
    
play(hc, w, b):-
    board(BoardIn),
    %jogada
	showScores, nl,
	showMaterialOfPlayers, nl,
    writeWhoIsPlaying(w), nl,
    printBoard(BoardIn),
    nl,
    userMoveAndCapture(w, BoardIn, Board1, PlayerFrom, PlayerTo), 
    updatePlayer(PlayerFrom),
    updatePlayer(PlayerTo),
	(testEnd(Board1), retract(board(BoardIn)), assert(board(Board1));
    nl,
	(playerStockExpired(w), captureAdaptoids(b, Board1, BoardOut);
    write('Enter option (1-create new 2-add pincer 3-add leg): '),
    read(Option),
    userCreateOrUpdate(Option, w, Board1, Board2, PlayerOut),
    updatePlayer(PlayerOut),
	captureAdaptoids(b, Board2, BoardOut,PlayerOut2)),
    updatePlayer(PlayerOut2),
    %Update board
    retract(board(BoardIn)),
    assert(board(BoardOut))).
    
play(hc, b, w):-
    board(BoardIn),
    %jogada
	showScores, nl,
	showMaterialOfPlayers, nl,
    writeWhoIsPlaying(b), nl, 
    printBoard(BoardIn),
    nl,
    botMoveAndCapture(b, BoardIn, Board1, PlayerFrom, PlayerTo),
    updatePlayer(PlayerFrom),
    updatePlayer(PlayerTo),
	(testEnd(Board1), retract(board(BoardIn)), assert(board(Board1));
    nl,
	(playerStockExpired(b), captureAdaptoids(w, Board1, BoardOut);
    botCreateOrUpdate(b, Board1, Board2, PlayerOut),
    updatePlayer(PlayerOut),
    captureAdaptoids(w, Board2, BoardOut, PlayerOut2)),
    updatePlayer(PlayerOut2),
    %Update board
    retract(board(BoardIn)),
    assert(board(BoardOut))).
    
play(cc, ColorIn, ColorOut):-
    getColorOfEnemy(ColorIn, ColorOut),
    board(BoardIn),
    %jogada
	showScores, nl,
	showMaterialOfPlayers, nl,
    writeWhoIsPlaying(ColorIn), nl, 
    printBoard(BoardIn),
    nl,
    botMoveAndCapture(ColorIn, BoardIn, Board1, PlayerFrom, PlayerTo),
    updatePlayer(PlayerFrom),
    updatePlayer(PlayerTo),
	(testEnd(Board1), retract(board(BoardIn)), assert(board(Board1));
    nl,
	(playerStockExpired(ColorIn), captureAdaptoids(ColorOut, Board1, BoardOut);
    botCreateOrUpdate(ColorIn, Board1, Board2, PlayerOut),
    updatePlayer(PlayerOut),
    captureAdaptoids(ColorOut, Board2, BoardOut, PlayerOut2)),
    updatePlayer(PlayerOut2),
    %Update board
    retract(board(BoardIn)),
    assert(board(BoardOut))),
    get_char(_).
	
playerStockExpired(Color) :-
	player(Color, Adaptoids, Legs, Pincers, _),
	Adaptoids =:= 0, Legs =:= 0, Pincers =:= 0.
    
userMoveAndCapture(Color, BoardIn, BoardIn, PlayerFrom, PlayerTo):-
    findall(Legs, getPiece(_R,_C,BoardIn,[Color,Legs,_]), Pieces),
    checkIfNoLegs(Pieces), !,
    player(Color, AFrom, LFrom, PFrom, SFrom),
    PlayerFrom = [Color, AFrom, LFrom, PFrom, SFrom],
    getColorOfEnemy(Color, ColorEnemy),
    player(ColorEnemy, ATo, LTo, PTo, STo),
    PlayerTo = [ColorEnemy, ATo, LTo, PTo, STo].
    

userMoveAndCapture(Color, BoardIn, BoardOut, PlayerFrom, PlayerTo):-
    write('Enter Coordinates of adaptoid to move: '), nl, 
	readCoords(UserRowFrom, UserColFrom), 
    cliToLogicCoords(UserRowFrom, UserColFrom, RowFrom, ColFrom),
    write('Enter Coordinates of destination: '), nl, 
	readCoords(UserRowTo, UserColTo), 
    cliToLogicCoords(UserRowTo, UserColTo, RowTo, ColTo),    
    moveAndCapture(Color,RowFrom,ColFrom,RowTo,ColTo,BoardIn,BoardOut, PlayerFrom, PlayerTo),
    printBoard(BoardOut),
    nl.
    
botMoveAndCapture(Color, BoardIn, BoardIn, PlayerFrom, PlayerTo):-
    findall(Legs, getPiece(_R,_C,BoardIn,[Color,Legs,_]), Pieces),
    checkIfNoLegs(Pieces), !,
    player(Color, AFrom, LFrom, PFrom, SFrom),
    PlayerFrom = [Color, AFrom, LFrom, PFrom, SFrom],
    getColorOfEnemy(Color, ColorEnemy),
    player(ColorEnemy, ATo, LTo, PTo, STo),
    PlayerTo = [ColorEnemy, ATo, LTo, PTo, STo].
    
botMoveAndCapture(Color, BoardIn, BoardOut, PlayerFrom, PlayerTo):-
    player(Color, AFrom, LFrom, PFrom, SFrom),
    Player = [Color, AFrom, LFrom, PFrom, SFrom],
    computerLevel(1),
    randomFirstMove(BoardIn, Player, Move),
    Move = [RFrom, CFrom, RTo, CTo],
    moveAndCapture(Color,RFrom,CFrom,RTo,CTo,BoardIn,BoardOut, PlayerFrom, PlayerTo),
    printBoard(BoardOut),
    nl.
    
botMoveAndCapture(Color, BoardIn, BoardOut, PlayerFrom, PlayerTo):-
    player(Color, AFrom, LFrom, PFrom, SFrom),
    Player = [Color, AFrom, LFrom, PFrom, SFrom],
    getColorOfEnemy(Color, ColorEnemy),
    player(ColorEnemy, ATo, LTo, PTo, STo),
    Enemy = [ColorEnemy, ATo, LTo, PTo, STo],
    computerLevel(2),
    findBestFirstMove(BoardIn, Player, Enemy, RFrom, CFrom, RTo, CTo),
    moveAndCapture(Color,RFrom,CFrom,RTo,CTo,BoardIn,BoardOut, PlayerFrom, PlayerTo),
    printBoard(BoardOut),
    nl.

checkIfNoLegs([]).    
checkIfNoLegs([L|Legs]):-
    L is 0,
    checkIfNoLegs(Legs).
   
userCreateOrUpdate(1, Color, BoardIn, BoardOut, PlayerOut):-
	player(Color, Adaptoids, _, _, _),
	Adaptoids >= 1,
    write('Enter Coordinates of new adaptoid: '), nl,
	readCoords(UserRow, UserCol), 
    cliToLogicCoords(UserRow, UserCol, Row, Col),
    createAdaptoid(Color, Row, Col, BoardIn, BoardOut, PlayerOut).
    
userCreateOrUpdate(2, Color, BoardIn, BoardOut, PlayerOut):-
	player(Color, _, _, Pincers, _),
	Pincers >= 1,
    write('Enter Coordinates of adaptoid: '), nl, 
    readCoords(UserRow, UserCol), 
    cliToLogicCoords(UserRow, UserCol, Row, Col),
    addPincer(Color, Row, Col, BoardIn, BoardOut, PlayerOut).
    
userCreateOrUpdate(3, Color, BoardIn, BoardOut, PlayerOut):-
	player(Color, _, Legs, _, _),
	Legs >= 1,
    write('Enter Coordinates of adaptoid: '), nl, 
    readCoords(UserRow, UserCol), 
    cliToLogicCoords(UserRow, UserCol, Row, Col),
    addLeg(Color, Row, Col, BoardIn, BoardOut, PlayerOut).
    
botCreateOrUpdate(Color, BoardIn, BoardOut, PlayerOut):-
    player(Color, AFrom, LFrom, PFrom, SFrom),
    Player = [Color, AFrom, LFrom, PFrom, SFrom],
    computerLevel(1),
    randomMoveCreateOrUpdate(BoardIn, Player,Move),
    Move = [Type, Row, Col],
    botCreateOrUpdateByType(Type, Row, Col, Color, BoardIn, BoardOut, PlayerOut).
    
botCreateOrUpdate(Color, BoardIn, BoardOut, PlayerOut):-
    player(Color, AFrom, LFrom, PFrom, SFrom),
    Player = [Color, AFrom, LFrom, PFrom, SFrom],
    computerLevel(2),
    bestMoveCreateOrUpdate(BoardIn, Player,BestMove),
    BestMove = [Type, Row, Col],
    botCreateOrUpdateByType(Type, Row, Col, Color, BoardIn, BoardOut, PlayerOut).
    
botCreateOrUpdateByType('creation', Row, Col, Color, BoardIn, BoardOut, PlayerOut):-
    createAdaptoid(Color, Row, Col, BoardIn, BoardOut, PlayerOut).
botCreateOrUpdateByType('addLeg', Row, Col, Color, BoardIn, BoardOut, PlayerOut):-
    addLeg(Color, Row, Col, BoardIn, BoardOut, PlayerOut).
botCreateOrUpdateByType('addPincer', Row, Col, Color, BoardIn, BoardOut, PlayerOut):-
    addPincer(Color, Row, Col, BoardIn, BoardOut, PlayerOut).
	
readCoords(Row, Col) :-  
	write('Row-Col'), read(Row-Col).
	
showMaterialOfPlayers :-
	player(w, Adaptoids, Legs, Pincers, _),
	player(b, Adaptoids2, Legs2, Pincers2, _),
	write('MATERIAL (WHITE - BLACK)'), nl,
	write('Adaptoids: ('), write(Adaptoids), write(' - '), write(Adaptoids2), write(')'), nl,
	write('Legs: ('), write(Legs), write(' - '), write(Legs2), write(')'), nl,
	write('Pincers: ('), write(Pincers), write(' - '), write(Pincers2), write(')'), nl.
	
showScores :-
	player(w, _, _, _, ScoreWhite),
	player(b, _, _, _, ScoreBlack),
	write('SCORES   '),  write('White: '), write(ScoreWhite), write('  Black: '), write(ScoreBlack), nl.
	

updatePlayer(Player):-
    Player = [Color, Adaptoids, Legs, Pincers, Score],
    retract(player(Color,_,_,_,_)),
    assert(player(Color, Adaptoids, Legs, Pincers, Score)).

