/* 
Colaboradores: 
- Marcel Mejias 30514210 
- Samantha Ramirez 31307714
*/ 
/*  
    Diseñar un programa lógico que permita determinar cuántos litros de cerveza deben agregarse entre los barriles para servir 
    exactamente n vasos de cerveza desde cualquiera de las salidas
    Vaso
    - 1 vaso = 1L cerveza
    - Se sirve desde cualquier barril que cumpla con la cantidad solicitada
    Barril
    - Diferentes capacidades máximas
    - Tienen una salida para servir en el vaso
    - Transferencias entre ellos respetan sus capacidades máximas
    - Posibles transferencias A <-> B <-> C 
    - Posibles agregaciones desde A, C
*/

/*
    Funciones adicionales
*/

% Predicados dinámicos para barriles
:- dynamic barrel/3.

/* 
    Parte 1: Inicialización de barriles 
*/
initialBarrels(Barrels, Capacity, Beer) :-
    % Verificar misma longitud en listas
    length(Barrels, Len),
    length(Capacity, Len),
    length(Beer, Len),
    % Verificar solo sean 3 barriles
    Len = 3,
    % Verificar que Barrels sean ["A", "B", "C"]
    Barrels = ["A", "B", "C"],
    % Verificar números válidos en capacidades y cantidades
    maplist(number, Capacity),
    maplist(number, Beer),
    % Predicado cut
    !,
    % Retractar hechos barrel/3
    retractall(barrel(_, _, _)),
    % Inicializar barriles
    initializeBarrels(Barrels, Capacity, Beer).

% Caso base
initializeBarrels([], [], []).

% Caso recursivo
initializeBarrels([ID|ID_Tail], [Capacity|CapacityTail], [Beer|BeerTail]) :-
    % Verificar válido
    Capacity >= 0,
    Beer >= 0,
    % Usar la menor cantidad entre Beer y Capacity
    ActualBeer is min(Beer, Capacity),
    % Asertar hecho barrel/3
    assertz(barrel(ID, Capacity, ActualBeer)),
    % Procesar resto de barriles
    initializeBarrels(ID_Tail, CapacityTail, BeerTail).

% Caso de error
initialBarrels(_, _, _) :- fail.

/* 
    Parte 2: Existe solución 
*/
iSolution(Barrel, Beer, Goal) :-
    (Barrel = "A" ; Barrel = "C"),
    number(Beer), Beer >= 0, integer(Goal), Goal >= 0,
    !,
    findall(barrel(ID, Cap, Amt), barrel(ID, Cap, Amt), SavedBarrels),
    addBeer(Barrel, Beer, Transfer),
    handleTransfer(Transfer, SavedBarrels, Goal),
    retractall(barrel(_, _, _)),
    maplist(assertz, SavedBarrels).

iSolution(Barrel, Beer, Goal) :-
    (Barrel = "A" ; Barrel = "C"),
    number(Beer), Beer >= 0, integer(Goal), Goal >= 0,
    findall(barrel(ID, Cap, Amt), barrel(ID, Cap, Amt), SavedBarrels),
    (   \+ addBeer(Barrel, Beer, _) ; \+ handleTransfer(Transfer, SavedBarrels, Goal)),
    retractall(barrel(_, _, _)),
    maplist(assertz, SavedBarrels),
    fail.

% Manejar transferencias y desbordes
handleTransfer(0, _, Goal) :- barrel(_, _, Amt), Amt >= Goal, !.
handleTransfer(Transfer, SavedBarrels, Goal) :-
    barrel("B", BCapacity, BCurrentBeer),
    NewBBeer is BCurrentBeer + Transfer,
    (   NewBBeer =< BCapacity
    ->  retract(barrel("B", BCapacity, _)),
        assertz(barrel("B", BCapacity, NewBBeer)),
        barrel(_, _, Amt), Amt >= Goal, !
    ;   ExcessB is NewBBeer - BCapacity,
        retract(barrel("B", BCapacity, _)),
        assertz(barrel("B", BCapacity, BCapacity)),
        barrel(_, _, Amt), Amt >= Goal, !;
        transferExcess(ExcessB, SavedBarrels, Goal)
    ).

% Transferir exceso al barril con menor cantidad
transferExcess(0, _, Goal) :- barrel(_, _, Amt), Amt >= Goal, !.
transferExcess(Excess, SavedBarrels, Goal) :-
    findall((ID, Amt), (barrel(ID, _, Amt), ID \= "B"), Barrels),
    Barrels \= [],
    sort(2, @=<, Barrels, [(MinID, _)|_]),
    barrel(MinID, MinCapacity, MinBeer),
    NewMinBeer is MinBeer + Excess,
    (   NewMinBeer =< MinCapacity
    ->  retract(barrel(MinID, MinCapacity, _)),
        assertz(barrel(MinID, MinCapacity, NewMinBeer)),
        barrel(_, _, Amt), Amt >= Goal, !
    ;   ExcessNext is NewMinBeer - MinCapacity,
        retract(barrel(MinID, MinCapacity, _)),
        assertz(barrel(MinID, MinCapacity, MinCapacity)),
        barrel(_, _, Amt), Amt >= Goal, !;
        % Intentar transferir el exceso al otro barril (A o C)
        (MinID == "A" -> OtherID = "C" ; OtherID = "A"),
        barrel(OtherID, OtherCapacity, OtherBeer),
        NewOtherBeer is OtherBeer + ExcessNext,
        (   NewOtherBeer =< OtherCapacity
        ->  retract(barrel(OtherID, OtherCapacity, _)),
            assertz(barrel(OtherID, OtherCapacity, NewOtherBeer)),
            barrel(_, _, Amt), Amt >= Goal, !
        ;   % Si hay desborde en el otro barril: llenar al máximo y perder el resto
            retract(barrel(OtherID, OtherCapacity, _)),
            assertz(barrel(OtherID, OtherCapacity, OtherCapacity)),
            barrel(_, _, Amt), Amt >= Goal, !
        )
    ).

/* 
    Parte 3: Añadir cerveza 
*/
addBeer(Barrel, Beer, Transfer) :-
    % Verificar solo se puede agregar desde A o C
    member(Barrel, ["A", "C"]), 
    % Verificar válidos
    number(Beer), Beer >= 0,
    % Verificar barril existe
    barrel(Barrel, _, _),
    !,  % Predicado corte
    % Si Beer es 0, no hacer nada
    (   Beer = 0
    ->  Transfer = 0
    ;   % Obtener estado actual del barril
        barrel(Barrel, Capacity, CurrentBeer),
        % Calcular nueva cantidad
        NewBeer is CurrentBeer + Beer,
        % Verificar si hay desborde
        (   NewBeer =< Capacity
        ->  % Sin desborde: actualizar el barril
            retract(barrel(Barrel, Capacity, _)),
            assertz(barrel(Barrel, Capacity, NewBeer)),
            Transfer = 0
        ;   % Con desborde: calcular exceso y actualizar barril
            Transfer is NewBeer - Capacity,
            retract(barrel(Barrel, Capacity, _)),
            assertz(barrel(Barrel, Capacity, Capacity))
        )
    ).

% Caso de error
addBeer(_, _, _) :- fail.

ValidBeer(Barrel, Beer, preBeer, Goal) :-
    member(Barrel, ["A", "C"]),
    iSolution(Barrel, Beer, Goal),
    iSolution(Barrel, preBeer, Goal),
    barrel(Barrel, Capacity, CurrentBeer),
    SpaceAvailable is integer(Capacity) - integer(CurrentBeer),
    integer(preBeer) =< SpaceAvailable,
    integer(Beer) > SpaceAvailable.


ValidBeer(Barrel, Beer, preBeer, Goal) :-
    member(Barrel, ["A", "C"]),
    iSolution(Barrel, Beer, Goal),
    iSolution(Barrel, preBeer, Goal),
    barrel("B", Capacity, CurrentBeer),
    SpaceAvailable is integer(Capacity) - integer(CurrentBeer),
    integer(preBeer) =< SpaceAvailable,
    integer(Beer) > SpaceAvailable.


    
/* 
    Parte 4: Mejor solución 
*/
findSolution(Goal, _, (0, "N/A")) :- 
    integer(Goal), Goal > 0,
    barrel(_, _, Amt), Amt >= Goal, !.
findSolution(Goal, _, (0, "N/A")) :- 
    integer(Goal), Goal =< 0, !.
findSolution(Goal, SolutionType, Result) :-
    integer(Goal), Goal > 0,
    % Calcular límite máximo de cerveza a probar
    findall(Cap, barrel(_, Cap, _), Caps),
    sum_list(Caps, MaxBeer),
    MaxBeerAdjusted is MaxBeer + Goal,
    % Obtener cantidades iniciales
    findall(Amt, barrel(_, _, Amt), InitialAmts),
    sum_list(InitialAmts, InitialBeer),
    % Probar soluciones
    findSolutionAux(Goal, SolutionType, MaxBeerAdjusted, InitialBeer, Result).

% Auxiliar para manejar soluciones
findSolutionAux(Goal, SolutionType, MaxBeerAdjusted, InitialBeer, Result) :-
    between(1, MaxBeerAdjusted, Beer),
    preBeer is Beer - 1,
    ValidBeer(Barrel, Beer, preBeer, Goal),
    Beer =< MaxBeerAdjusted - InitialBeer,
    (Barrel = "A" ; Barrel = "C"),
    findall(barrel(ID, Cap, Amt), barrel(ID, Cap, Amt), SavedBarrels),
    iSolution(Barrel, Beer, Goal),
    Result = (Beer, Barrel),
    retractall(barrel(_, _, _)),
    maplist(assertz, SavedBarrels),
    (   SolutionType = "best" -> ! % Detener en la primera solución para "best"
    ;   SolutionType = "all" % Continuar para "all"
    ).
