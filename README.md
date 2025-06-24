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
Con isSolution se verifica si existe solución al agregar cierta cantidad de cerveza desde un barril específico.
**Verificaciones**
- Se verifica que solo se puede agregar desde "A" o "C".
- Se verifica que las cantidades sean números válidos (>=0).
**Funcionamiento**
- Se almacena el estado actual de los barrilles en SavedBarrels.
- Se llama a addBeer para añadir litros a "A" o "C".
- Se llama a handleTransfer.
- Se comprueba si algún barril tiene exactamente Goal litros con barrel(_, _, Goal).
- Si se cumple, restaura el estado original y devuelve true.
- Si no, entra en la segunda cláusula, restaura el estado y falla (false).
**Manejar transferencias**
Con handleTransfer.
- Si Transfer = 0, no hace nada.
- Si no, se transfiere el exceso a "B".
- Si "B" se desborda, se calcula ExcessB y lo pasa a transferExcess.
**Transferir exceso**
Con transferExcess.
- Se transfiere el exceso al barril con menor cantidad.
- Si este barril se desborda, se transfiere el exceso restante al otro barril (C o A).
- Si el otro barril se desborda, se restaura el estado y falla.

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
Con ValidBeer y ValidBeer2 se toma una cantidad de cervezas agregadas y se comprueba si es una solución posible unica.
**Casos**
- En el primer ValidBeer se verifica si la cantidad de cerveza agregada logra que el barril donde se esta agregando ("A" o "C") llega al objetivo.
- En el segundo ValidBeer se verifica si la cantidad de cerveza agregada logra que el barril B llege al objetivo a través del desborde del barril "A" o el barril "C".
- En los dos validBeer2 se prueban de forma separada el caso donde tratamos con el barril "A" y el caso donde tratamos con el barril "C". Estas funciones nos verifican si la cantidad de cerveza agregada logre que el barril al extremo opuesto llegue al objetivo a través del desborde del barril de orrigen y del barril "B"

Luego en FindSolution se calcula el número maximo de cerveza para añadir a iterar y se va probando añadir todos los posibles valores hasta este número máximo

Por último en FindSolutionAux se crea una lista con todos los barriles y se aplica la función a todos para cada iteración de la cantidad de cerveza a añadir a excepción del caso donde se coloque "Best" de parametro en el cual solo se opera con el primer valor de Beer y por ende se optiene la mejor solución unicamente.
