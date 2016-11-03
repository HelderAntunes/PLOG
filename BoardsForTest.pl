
% as brancas atacam apos o movimento ate a peca das pretas, ficando as pretas sem pecas
boardToTestEndGame(
        [
        [empty, empty, empty, empty], 
        [empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty],
        [empty, [w, 3, 3], empty, empty, [b, 0, 0], empty, empty],
        [empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty],
        [empty, empty, empty, empty] 
        ]). 

% board inicial por defeito		
boardInitGame(
        [
        [empty, empty, empty, empty], 
        [empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty],
        [empty, [w, 0, 0], empty, empty, empty, [b, 0, 0], empty],
        [empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty],
        [empty, empty, empty, empty] 
        ]). 
		
% 2 pecas brancas irao desaparecer
boardToTestCaptureHungryAdaptoids(
        [
        [empty, empty, empty, empty], 
        [empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty],
        [empty, [w, 0, 0], [w, 3, 3], empty, empty, [b, 0, 0], empty],
        [empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty],
        [[w, 4, 0], empty, empty, empty] 
        ]). 
		