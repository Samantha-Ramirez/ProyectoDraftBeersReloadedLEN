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
?- iSolution("A", 1, 2).
true.
?- iSolution("C", 3, 4).
false.

% Base de conocimientos
barrel("A", 10, 1).
barrel("B", 7, 3).
barrel("C", 3, 0).