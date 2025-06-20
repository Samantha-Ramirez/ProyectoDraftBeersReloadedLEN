
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
% Oredicados dinámicos para barriles
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
    initialize_barrels(IDs, Capacities, Beers).

% Caso base
initialize_barrels([], [], []).

% Caso recursivo
initialize_barrels([ID|ID_Tail], [Capacity|Capacity_Tail], [Beer|Beer_Tail]) :-
    % Verificar válido
    Capacity >= 0,
    Beer >= 0,
    Beer =< Capacity,
    % Asertar hecho barrel/3
    assertz(barrel(ID, Capacity, Beer)),
    % Procesar resto de barriles
    initialize_barrels(ID_Tail, Capacity_Tail, Beer_Tail).

% Caso de error: cualquier validación fallida retorna false
% initialBarrels(_, _, _) :- fail.

/* 
    Parte 2: Existe solución 
*/

/* 
    Parte 3: Añadir cerveza 
*/
  
/* 
    Parte 4: Mejor solución 
*/
