
/* 
Colaboradores: 
- Marcel Mejias 30514210 
- Samantha Ramirez 31307714
*/ 
/*  
    Diseñar un programa que permita determinar cuántos litros de cerveza deben agregarse entre los barriles para servir 
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
    Beer =< Capacity,
    % Asertar hecho barrel/3
    assertz(barrel(ID, Capacity, Beer)),
    % Procesar resto de barriles
    initializeBarrels(ID_Tail, CapacityTail, BeerTail).

% Caso de error
initialBarrels(_, _, _) :- fail.

/* 
    Parte 2: Existe solución 
*/
iSolution(Barrel, Beer, Goal) :-
    % Verificar solo se puede agregar desde A o C
    member(Barrel, ["A", "C"]),
    % Verificar válidos
    number(Beer), Beer >= 0,
    integer(Goal), Goal >= 0,
    % Guardar estado actual de los barriles
    findall(barrel(ID, Cap, Amt), barrel(ID, Cap, Amt), SavedBarrels),
    % Intentar añadir cerveza
    (   addBeer(Barrel, Beer, Transfer),
        % Manejar la transferencia a B
        (   Transfer = 0
        ->  true  % No hay transferencia
        ;   barrel("B", BCapacity, BCurrentBeer),
            NewBBeer is BCurrentBeer + Transfer,
            (   NewBBeer =< BCapacity
            ->  % Actualizar B
                retract(barrel("B", BCapacity, _)),
                assertz(barrel("B", BCapacity, NewBBeer))
            ;   % Desborde en B: transferir a A o C
                ExcessB is NewBBeer - BCapacity,
                retract(barrel("B", BCapacity, _)),
                assertz(barrel("B", BCapacity, BCapacity)),
                % Encontrar barril con menor cantidad (A o C)
                findall((ID, Amt), (barrel(ID, _, Amt), ID \= "B"), Barrels),
                Barrels \= [],
                sort(2, @=<, Barrels, [(MinID, _)|_]),
                barrel(MinID, MinCapacity, MinBeer),
                NewMinBeer is MinBeer + ExcessB,
                (   NewMinBeer =< MinCapacity
                ->  retract(barrel(MinID, MinCapacity, _)),
                    assertz(barrel(MinID, MinCapacity, NewMinBeer))
                ;   % Desborde en el destino: fallar
                    retractall(barrel(_, _, _)),
                    maplist(assertz, SavedBarrels),
                    fail
                )
            )
        ),
        % Verificar si algún barril tiene exactamente Goal litros
        (   barrel(_, _, Goal)
        ->  % Solución encontrada: restaurar estado
            retractall(barrel(_, _, _)),
            maplist(assertz, SavedBarrels),
            true
        ;   % No hay solución: restaurar estado
            retractall(barrel(_, _, _)),
            maplist(assertz, SavedBarrels),
            fail
        )
    ;   % addBeer falló: restaurar estado
        retractall(barrel(_, _, _)),
        maplist(assertz, SavedBarrels),
        fail
    ).

% Caso de error
iSolution(_, _, _) :- fail.

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
  
/* 
    Parte 4: Mejor solución 
*/
