(ns tp-formales.core-test
  (:require [clojure.test :refer :all]
            [tp-formales.core :refer :all]))

(deftest variable-string?-test
  (testing "Prueba de la función: variable-string?"
    (is (= true (variable-string? 'X$))))
  (is (= false (variable-string? 'X)))
  (is (= false (variable-string? 'X%)))
  (is (= false (variable-string? "X$"))))

(deftest variable-integer?-test
  (testing "Prueba de la función: variable-integer?"
    (is (= false (variable-integer? 'X)))
    (is (= true (variable-integer? 'X%)))
    (is (= false (variable-integer? 'X$)))
    (is (= false (variable-integer? "x")))))

(deftest variable-float?-test
  (testing "Prueba de la función: variable-float?"
    (is (= true (variable-float? 'X)))
    (is (= false (variable-float? 'X%)))
    (is (= false (variable-float? 'X$)))
    (is (= false (variable-float? "x")))))
    

(deftest eliminar-cero-decimal-test
  (testing "Prueba de la función: eliminar-cero-decimal"
    (is (= 1 (eliminar-cero-decimal 1.0))))
  (is (= 1.1 (eliminar-cero-decimal 1.1)))
  (is (= 'A (eliminar-cero-decimal 'A))))

(deftest eliminar-cero-entero-test
  (testing "Prueba de la función: eliminar-cero-entero"
    (is (= " 0" (eliminar-cero-entero 0))))
  (is (= " 1.5" (eliminar-cero-entero 1.5)))
  (is (= " 1" (eliminar-cero-entero 1)))
  (is (= "A" (eliminar-cero-entero 'A)))
  (is (= nil (eliminar-cero-entero nil)))
  (is (= " .5" (eliminar-cero-entero 0.5)))
  (is (= "-.5" (eliminar-cero-entero -0.5)))
  (is (= "-1.5" (eliminar-cero-entero -1.5))))


(deftest expandir-nexts-test
  (testing "Prueba de la función: expandir-nexts"
  (let [sentences (list '(PRINT 1) (list 'NEXT 'A (symbol ",") 'B))
        expected (list '(PRINT 1) (list 'NEXT 'A) (list 'NEXT 'B))
        actual (expandir-nexts sentences)]
    (is (= expected actual)))))


(deftest cargar-linea-test
  (testing "Prueba de la función: cargar-linea")
  ;;lista vacia
  (is (= (cargar-linea '(10 (PRINT X)) ['() [:ejecucion-inmediata 0] [] [] [] 0 {}])
         ['((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}]))
  ;;agregar al final
  (is (= (cargar-linea '(20 (X = 100)) ['((10 (EXIT))) [:ejecucion-inmediata 0] [] [] [] 0 {}])
         ['((10 (EXIT)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}]))

  ;;agregar al medio
  (is (= (cargar-linea '(15 (X = X + 1)) ['((10 (PRINT X)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}])
         ['((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}]))
  
  ;;reemplazar
  (is (= (cargar-linea '(15 (X = X - 1))  ['((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}])
         ['((10 (PRINT X)) (15 (X = X - 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))

(deftest operador?-test
  (testing "Prueba de la función: operador?"
    (is (= true (operador? '+)))
    (is (= true (operador? '-)))
    (is (= true (operador? '*)))
    (is (= true (operador? '/)))
    (is (= false (operador? '%)))
    (is (= true (operador? '=)))
    (is (= true (operador? '<>)))
    (is (= true (operador? '<)))
    (is (= true (operador? '<=)))
    (is (= true (operador? '>)))
    (is (= true (operador? '>=)))
    (is (= true (operador? 'AND)))
    (is (= true (operador? 'OR)))
    (is (= false (operador? 'X)))
    (is (= false (operador? 1)))
    (is (= false (operador? "&")))
    (is (= false (operador? "!")))
    (is (= false (operador? "X"))))
    (is (= true (operador? "+"))))

(deftest palabra-reservada?-test
  (testing "Prueba de la función: palabra-reservada?" 
    (is (= true (palabra-reservada? 'IF)))
    (is (= true (palabra-reservada? 'THEN)))
    (is (= false (palabra-reservada? 'ELSE)))
    (is (= false (palabra-reservada? 'ENDIF)))
    (is (= false (palabra-reservada? 'WHILE)))
    (is (= true (palabra-reservada? 'PRINT)))
    (is (= true (palabra-reservada? 'EXIT)))
    (is (= true (palabra-reservada? 'NEXT)))
    (is (= true (palabra-reservada? 'FOR)))
    (is (= false (palabra-reservada? 1)))
    (is (= false (palabra-reservada? 1.0)))
    (is (= false (palabra-reservada? "X")))
    (is (= true (palabra-reservada?  'MID$)))))

(deftest dar-error-test
  (testing "Prueba de la función: dar-error"
    (is (= (with-out-str (dar-error 16 [:ejecucion-inmediata 4])) "?SYNTAX  ERROR"))
    (is (= (with-out-str (dar-error "?ERROR DISK FULL" [:ejecucion-inmediata 4])) "?ERROR DISK FULL"))
    (is (= (with-out-str (dar-error 16 [100 3])) "?SYNTAX  ERROR IN 100"))
    (is (= (with-out-str (dar-error "?ERROR DISK FULL" [100 3])) "?ERROR DISK FULL IN 100"))))

(deftest preprocesar-expresion-test
  (testing "Prueba de la función: preprocesar-expresion")
  (is (= (preprocesar-expresion '(X$ + " MUNDO" + Z$) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}]) '("HOLA" + " MUNDO" + "")))
  (is (= (preprocesar-expresion '(X + . / Y% * Z) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 5 Y% 2}]) '(5 + 0 / 2 * 0))))

(deftest desambiguar-tests
  (testing "Prueba de la función: desambiguar"
    (is (= (desambiguar (list '- 2 '* (symbol "(") '- 3 '+ 5 '- (symbol "(") '+ 2 '/ 7 (symbol ")") (symbol ")")))
           (list '-u 2 '* (symbol "(") '-u 3 '+ 5 '- (symbol "(") 2 '/ 7 (symbol ")") (symbol ")"))))
    (is (= (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ")")))
           (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ")"))))
    (is (= (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ",") 3 (symbol ")")))
           (list 'MID3$ (symbol "(") 1 (symbol ",") 2 (symbol ",") 3 (symbol ")"))))
    (is (= (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") '- 2 '+ 'K (symbol ",") 3 (symbol ")")))
           (list 'MID3$ (symbol "(") 1 (symbol ",") '-u 2 '+ 'K (symbol ",") 3 (symbol ")"))))))

(deftest anular-invalidos-test
  (testing "Prueba de la función: anular-invalidos"
    (is (= (anular-invalidos '(+ 1 2)) '(+ 1 2)))

    (is (= (anular-invalidos '(IF X & * Y < 12 THEN LET ! X = 0)) '(IF X nil * Y < 12 THEN LET nil X = 0)))

    ;los nombres de variables pueden contener letras y numeros
    (is (= (anular-invalidos '(IF XA9 & * Y < 12 THEN LET ! X = 0)) '(IF XA9 nil * Y < 12 THEN LET nil X = 0)))

    ;los nombres de variables no pueden empezar con algo que no sea una letra
    (is (= (anular-invalidos '(IF $X & * Y < 12 THEN LET ! X = 0)) '(IF nil nil * Y < 12 THEN LET nil X = 0)))))

(deftest precedencia-test
  (testing "Prueba de la función: precendencia"
    (is (= (precedencia 'OR) 1))
    (is (= (precedencia 'AND) 2))
    (is (= (precedencia 'NOT) 3))
    (is (= (precedencia '=) 4))
    (is (= (precedencia '-) 5))
    (is (= (precedencia '/) 6))
    (is (= (precedencia '-u) 7))
    (is (= (precedencia 'MID$) 8))))

(deftest aridad-test
  (testing "Prueba de la función: aridad"
    (is (= (aridad 'OR) 2))
    (is (= (aridad 'AND) 2))
    (is (= (aridad 'NOT) 1))
    (is (= (aridad '=) 2))
    (is (= (aridad '-) 2))
    (is (= (aridad '/) 2))
    (is (= (aridad '-u) 1))
    (is (= (aridad 'MID$) 2))
    (is (= (aridad 'THEN) 0))
    (is (= (aridad 'MID3$) 3))))

(deftest contar-sentencias-test
  (testing "Prueba de la función: contar-sentencias"
    (is (= (contar-sentencias 10 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}]) 2))
    (is (= (contar-sentencias 15 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}]) 1))
    (is (= (contar-sentencias 20 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}]) 2))))


(deftest extraer-data-test
  (testing "Prueba de la función: extraer-data"
    (is (= '() (extraer-data '(()))))
    (is (= (extraer-data (list '(10 (PRINT X) (REM ESTE NO) (DATA 30)) '(20 (DATA HOLA)) (list 100 (list 'DATA 'MUNDO (symbol ",") 10 (symbol ",") 20)))) '("HOLA" "MUNDO" 10 20)))
    
    ))

(deftest ejecutar-asignacion-test
  (testing "Prueba de la función: ejecutar-asignacion"
    (is (= (ejecutar-asignacion '(X = X + 1) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}]) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 3}]))
    
    (is (= (ejecutar-asignacion '(X$ = X$ + " MUNDO") ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}]) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA MUNDO"}]))
    
    (is (= (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}]) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 5}]))
    
    (is (= (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 {}]) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 5}]))))  


(deftest buscar-lineas-restantes-test
  (testing "Pruebas de la función: buscar-lineas-restantes"
    (is (= (buscar-lineas-restantes [() [:ejecucion-inmediata 0] [] [] [] 0 {}]) nil))

    (is (= (buscar-lineas-restantes ['((PRINT X) (PRINT Y)) [:ejecucion-inmediata 2] [] [] [] 0 {}]) nil))

    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 2] [] [] [] 0 {}])
           (list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J)))))

    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])
           (list '(10 (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J)))))

    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 0] [] [] [] 0 {}]) (list '(10) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J)))))

    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [] [] [] 0 {}]) (list '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J)))))

    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 0] [] [] [] 0 {}]) (list '(15) (list 20 (list 'NEXT 'I (symbol ",") 'J)))))

    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}]) '((20 (NEXT I) (NEXT J)))))

    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 1] [] [] [] 0 {}]) '((20 (NEXT J)))))
    
    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 0] [] [] [] 0 {}]) '((20))))
    
    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 -1] [] [] [] 0 {}]) '((20))))

    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [25 0] [] [] [] 0 {}]) nil))))


(deftest continuar-linea-test
  (testing "Pruebas de la función: continuar-linea"
    (is (= (with-out-str (continuar-linea [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}])) "?RETURN WITHOUT GOSUB  ERROR IN 20"))

    (is (= (binding [*out* *err*] (continuar-linea [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}])) (vector nil [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}]))) ;binding es para no mostrar el mensaje de error en la salida estándar

    (is (= (continuar-linea [(list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [[15 2]] [] [] 0 {}]) (vector :omitir-restante [(list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [] [] [] 0 {}])))))


  

