# Trabajo práctico de la materia Lenguajes Formales FIUBA

Intérprete de C64 desarrollado en Clojure. Ofrece dos modos de ejecución de COMMODORE 64 BASIC V2: 
ejecución inmediata y ejecución diferida. Basado en un REPL (read-eval-print-loop) que 
acepta,  además  de  sentencias  de  COMMODORE  64  BASIC  V2,  los  comandos  del  intérprete.  


## Uso
Desde el directorio /tp_formales:
`lein test` corre los tests.
`lein run` corre el intérprete. Para cargar los programas: `load <programa.bas>`