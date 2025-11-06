# Motor de Aventura de Texto

**Nombre**: Rosa Ramirez - Alejandra Mármol 
**Carnet**: 2010527 - 2010408 
**Curso**: CI3661 - Laboratorio de Lenguajes de Programación

## Cómo compilar y ejecutar

```bash
stack build
stack run
```

## Justificación de diseño

Se utilizó `Data.Map` para representar el inventario del jugador, las salidas de las salas y el mapa del mundo, debido a su eficiencia en búsquedas (O(log n)) y porque los nombres de salas y objetos actúan como claves únicas. La lógica del juego se mantiene completamente pura en el módulo `Engine.Core` (sin IO), mientras que toda la interacción con el usuario (lectura de comandos, impresión de mensajes) se aísla en `Main.hs\). Este diseño garantiza un motor reutilizable, desacoplado del contenido y fácilmente testeable.
