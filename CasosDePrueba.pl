/* 
    Parte 1: Inicialización de barriles 
*/

?- initialBarrels(["A", "B", "C"], [10, 7, 3], [10, 0, 0]).

% Base de conocimientos:
barrel("A", 10, 10).
barrel("B", 7, 0).
barrel("C", 3, 0).

?- initialBarrels(["A", "B", "C"], [0, 0, 0], [0, 0, 0]).

% Base de conocimientos:
barrel("A", 0, 0).
barrel("B", 0, 0).
barrel("C", 0, 0).

?- initialBarrels(["A", "B", "C"], [0, 0, 0], [1, 2, 3]).

% Base de conocimientos:
barrel("A", 0, 0).
barrel("B", 0, 0).
barrel("C", 0, 0).

/* 
    Parte 2: Existe solución 
*/
% Base de conocimientos
initialBarrels(["A", "B", "C"], [10, 7, 3], [1, 3, 0]).

?- iSolution("A", 1, 2).
true.
?- iSolution("C", 3, 4).
false.

% Base de conocimientos
initialBarrels(["A", "B", "C"], [5, 3, 2], [1, 1, 1]).

?- iSolution("C", 10, 0).
true.

% Base de conocimientos
initialBarrels(["A", "B", "C"], [5, 3, 2], [2, 2, 2]).

?- iSolution("C", 1, 1).
true.

% Base de conocimientos
initialBarrels(["A", "B", "C"], [1, 2, 3], [1, 2, 3]).

?- iSolution("C", 0, 0).
true.

% Base de conocimientos
initialBarrels(["A", "B", "C"], [1, 2, 3], [1, 2, 3]).

?- iSolution("C", 0, 1).
true.

% Base de conocimientos
% TOFIX
initialBarrels(["A", "B", "C"], [0, 0, 0], [0, 0, 0]).

?- iSolution("C", 0, 0).
false.

/* 
    Parte 3: Añadir cerveza 
*/
% Base de conocimientos
initialBarrels(["A", "B", "C"], [20, 7, 4], [15, 0, 3]).

?- addBeer("A", 5, Transfer).
Transfer = 0.

?- addBeer("C", 5, Transfer).
Transfer = 4.

% Base de conocimientos
initialBarrels(["A", "B", "C"], [9, 7, 4], [0, 0, 3]).

?- addBeer("A", 10, Transfer).
Transfer = 1. % (9, 9)

% Base de conocimientos
initialBarrels(["A", "B", "C"], [999, 7, 4], [998, 0, 3]).

?- addBeer("A", 1000, Transfer).
Transfer = 999. % (999,999)

% Base de conocimientos
initialBarrels(["A", "B", "C"], [1000, 7, 4], [1000, 0, 3]).

?- addBeer("A", 1000, Transfer).
Transfer = 1000. % (1000,1000)

% Base de conocimientos
initialBarrels(["A", "B", "C"], [0, 7, 4], [0, 0, 3]).

?- addBeer("A", 1000, Transfer).
Transfer = 1000. % (0,0)

/* 
    Parte 4: Mejor solución 
*/
% Base de conocimientos
initialBarrels(["A", "B", "C"], [10, 7, 4], [3, 0, 0]).

?- findSolution(4, "best", Result).
Result = (1, "A").

?- findSolution(4, "all", Result).
Result = (1, "A") ;
Result = (4, "C") ;
Result = (8, "C") ;
Result = (11, "A") ;
Result = (12, "C") ;
Result = (18, "A").

?- findSolution(1, "best", Result).
Result = (0, "N/A").