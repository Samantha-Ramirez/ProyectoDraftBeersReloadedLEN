/* 
    Parte 1: Inicialización de barriles 
*/

?- initialBarrels(["A", "B", "C"], [10, 7, 3], [10, 0, 0]).

% Base de conocimientos:
barrel("A", 10, 10).
barrel("B", 7, 0).
barrel("C", 3, 0).

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