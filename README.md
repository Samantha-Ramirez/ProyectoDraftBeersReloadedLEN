# Draft Beers Reloaded

## Colaboradores

- **Marcel Mejías**
- **Samantha Ramirez**

### Parte 1: Inicialización de barriles 
Con initialBarrels se hacen validaciones, se eliminan los hechos barrel/3 en la base de conocimientos y luego llama a initializeBarrels.
**Verificaciones**
- Se verifica que solo sean 3 barriles con length.
- Se verifica que esos 3 barriles sean específicamente "A", "B" y "C" con IDs = ["A", "B", "C"].
- Se verifica las capacidades y cantidades sean números válidos con maplist(number, _).
**Funcionamiento**
- Con el predicado cut hace que solo devuelva una solución.
- Si no cumple todo lo anterior, se devuelve false initialBarrels(_, _, _) :- fail.

Con initializeBarrels se valida que las capacidades y cantidades estén en el rango apropiado (>=0), se insertan un hecho barrel/3 y así recursivamente.
- Dado que se usa :- dynamic barrel/3. los hechos son modificables en tiempo de ejecución.

### Parte 2: Existe solución
Con 
**Desborde en B**
- Se maneja de la misma manera pero ahora se debe encontrar el barril con menor cantidad de cerveza entre "A" y "C".
- Se recolectan los barriles disponibles con findall y se ordenan por cantidad de cerveza.
- Se calcula la nueva cantidad en el barril destino (NewMinBeer). 
- Si no hay desborde (NewMinBeer =< MinCapacity), se actualiza el barril destino y Transfer = Excess. 
- Si hay desborde, el predicado falla (evitar ciclo infinito).

### Parte 3: Añadir cerveza 
Con addBeer se añade cerveza a un barril ("A" o "C") y devuelve en Transfer la cantidad de cerveza que se transfiere a otros barriles en caso de desborde.
**Verificaciones**
- Se verifica que solo se puede agregar desde "A" o "C".
- Se verifica que las cantidades sean números válidos (>=0).
- Se verifica que el barril existe.
**Funcionamiento**
- Con el predicado cut hace que solo devuelva una solución.
- (Caso especial) Si la cantidad es 0 no se realiza ninguna transferencia (Transfer=0) y termina sin modificar el estado de los barriles.
- Se obtiene el estado actual del barril.
- Se calcula la nueva cantidad de cerveza en el barril NewBeer is CurrentBeer + Beer.
**Desborde**
- Si no hay desborde (NewBeer =< Capacity), se actualiza el barril y no hay transferencia (Transfer=0).
- Si hay desborde (NewBeer > Capacity), se actualiza el barril al máximo, se calcula la cantidad sobrante.

### Parte 4: Mejor solución