# Draft Beers Reloaded

## Colaboradores

- **Marcel Mejías**
- **Samantha Ramirez**

### Parte 1: Inicialización de barriles 
Con initialBarrels se hacen validaciones, se eliminan los hechos barrel/3 en la base de conocimientos y luego llama a initializeBarrels.
- Se verifica que solo sean 3 barriles con length.
- Se verifica que esos 3 barriles sean específicamente "A", "B" y "C" con IDs = ["A", "B", "C"].
- Se verifica las capacidades y cantidades sean números válidos con maplist(number, _).
- Con el predicado cut hace que solo devuelva una solución.
- Si no cumple todo lo anterior, se devuelve false initialBarrels(_, _, _) :- fail.

Con initializeBarrels se valida que las capacidades y cantidades estén en el rango apropiado (>=0), se insertan un hecho barrel/3 y así recursivamente.
- Dado que se usa :- dynamic barrel/3. los hechos son modificables en tiempo de ejecución.

### Parte 2: Existe solución
Con 

### Parte 3: Añadir cerveza 
Con addBeer se añade cerveza a un barril.
- Se verifica que solo se puede agregar desde "A" o "C".
- Se verifica que las cantidades sean 
- Si no cumple todo lo anterior, se devuelve false addBeer(_, _, _) :- fail.


### Parte 4: Mejor solución