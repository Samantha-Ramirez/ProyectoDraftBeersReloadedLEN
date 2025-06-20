
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

/* 
    Parte 1: Inicialización de barriles 
*/
% Predicados dinámicos para barriles
:- dynamic barrel/3.

initialBarrels(IDs, Capacities, Beers) :-
    % Verificar misma longitud en listas
    length(IDs, Len),
    length(Capacities, Len),
    length(Beers, Len),
    % Verificar solo sean 3 barriles
    Len = 3,
    % Verificar que IDs sean ["A", "B", "C"]
    IDs = ["A", "B", "C"],
    % Verificar números válidos en capacidades y cantidades
    maplist(number, Capacities),
    maplist(number, Beers),

    % Retractar hechos barrel/3
    retractall(barrel(_, _, _)),
    % Inicializar barriles
    initializeBarrels(IDs, Capacities, Beers).

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
% initialBarrels(_, _, _) :- fail.

/* 
    Parte 2: Existe solución 
*/
iSolution(BarrelID, Beer, Goal) :-
    % Validar solo se puede agregar desde A o C
    member(BarrelID, ["A", "C"]),
    % Validar válidos
    number(Beer), Beer >= 0,
    integer(Goal), Goal >= 0,

    % Guardar estado actual de los barriles
    findall(barrel(ID, Cap, Amt), barrel(ID, Cap, Amt), SavedBarrels),

    % Intentar añadir cerveza
    (   addBeer(BarrelID, Beer)
    ->  % Verificar si algún barril tiene exactamente Goal litros
        (   barrel(_, _, Goal)
        ->  % Solución: restaurar estado
            retractall(barrel(_, _, _)),
            maplist(assertz, SavedBarrels),
            true
        ;   % No hay solución: restaurar estado
            retractall(barrel(_, _, _)),
            maplist(assertz, SavedBarrels),
            fail
        )
    ;   % addBeer falló, restaurar estado
        retractall(barrel(_, _, _)),
        maplist(assertz, SavedBarrels),
        fail
    ).

% Caso de error
% iSolution(_, _, _) :- fail.

/* 
    Parte 3: Añadir cerveza 
*/
addBeer(BarrelID, Beer) :-
    % Si Beer es 0, no hacer nada
    (   Beer = 0
    ->  true
    ;   % Obtener el estado actual del barril
        barrel(BarrelID, Capacity, CurrentBeer),
        % Calcular nueva cantidad
        NewBeer is CurrentBeer + Beer,
        % Verificar si hay desborde
        (   NewBeer =< Capacity
        ->  % Sin desborde: actualizar el barril
            retract(barrel(BarrelID, Capacity, _)),
            assertz(barrel(BarrelID, Capacity, NewBeer))
        ;   % Con desborde
            Excess is NewBeer - Capacity,
            % Actualizar el barril al máximo
            retract(barrel(BarrelID, Capacity, _)),
            assertz(barrel(BarrelID, Capacity, Capacity)),
            % Si el barril es B, transferir el exceso
            (   BarrelID = "B"
            ->  % Encontrar el barril con menor cantidad de cerveza
                findall((ID, Amt), (barrel(ID, _, Amt), ID \= "B"), Barrels),
                sort(2, @=<, Barrels, [(MinID, _)|_]),
                % Obtener el estado del barril destino
                barrel(MinID, MinCapacity, MinBeer),
                NewMinBeer is MinBeer + Excess,
                % Verificar que no haya desborde en el destino
                (   NewMinBeer =< MinCapacity
                ->  % Actualizar el barril destino
                    retract(barrel(MinID, MinCapacity, _)),
                    assertz(barrel(MinID, MinCapacity, NewMinBeer))
                ;   % Desborde en el destino, fallar
                    fail
                )
            ;   % Desborde en A o C, fallar
                fail
            )
        )
    ).

% Caso de error
% addBeer(_, _) :- fail.
  
/* 
    Parte 4: Mejor solución 
*/
