# Motor de Aventura de Texto

**Nombre**: Rosa Ramirez - Alejandra Mármol 
**Carnet**: 2010527 - 2010408 
**Curso**: CI3661 - Laboratorio de Lenguajes de Programación I

## Cómo compilar y ejecutar

```bash
stack build
stack run
```

Comandos disponibles:
- `ir <dirección>` → para moverse (`norte`, `sur`, `este`, `oeste`)
- `mirar` → muestra la descripción de la sala actual
- `tomar <objeto>` → recoge un objeto de la sala
- `inventario` → muestra los objetos que llevas
- `salir` → termina el juego

El objetivo es explorar la torre, encontrar la *llave del guardián* y escapar por la *Puerta Principal*.

## Justificación de diseño

Elegimos utilizar `Data.Map` para representar el inventario del jugador, las salidas de cada sala y el mapa completo del mundo. Cada sala y objeto tiene un nombre único, lo que nos permite usarlo como clave. Esta elección es más eficiente que usar listas: mientras que en una lista la búsqueda tiene complejidad **O(n)**, en `Data.Map` es **O(log n)**, lo que mejora el rendimiento incluso si el mundo crece.

Además, organizamos el código para separar claramente la lógica del juego de las operaciones de entrada y salida. Toda la lógica pura como procesar comandos o actualizar el estado está en los módulos `Engine.Core` y `Engine.Persistence`, y no contiene efectos secundarios. Por otro lado, todo lo relacionado con interactuar con el usuario (leer comandos, imprimir mensajes, cargar el archivo `mundo.txt`) se maneja exclusivamente en `Main.hs`.

Gracias a esta separación, el motor es reutilizable, funciona con cualquier mundo que siga el formato de `mundo.txt`, sin modificar su lógica interna.
