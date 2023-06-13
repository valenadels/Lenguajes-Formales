(ns tp-formales.core-test
  (:require [clojure.test :refer :all]
            [tp-formales.core :refer :all]))

(deftest variable-string?-test
  (testing "Prueba de la funcion: variable-string?"
    (is (= true (variable-string? 'X$))))
  (is (= false (variable-string? 'X)))
  (is (= false (variable-string? 'X%)))
  (is (= false (variable-string? "X$"))))

(deftest variable-integer?-test
  (testing "Prueba de la funcion: variable-integer?"
    (is (= false (variable-integer? 'X)))
    (is (= true (variable-integer? 'X%)))
    (is (= false (variable-integer? 'X$)))
    (is (= false (variable-integer? "x")))))

(deftest variable-float?-test
  (testing "Prueba de la funcion: variable-float?"
    (is (= true (variable-float? 'X)))
    (is (= false (variable-float? 'X%)))
    (is (= false (variable-float? 'X$)))
    (is (= false (variable-float? "x")))))
     


;; (deftest variable-float?-test
;;   (testing "Prueba de la funcion: variable-float?"
;;     (is (= true (variable-integer? 'X))))
;;   (is (= false (variable-integer? 'X$))))

(deftest eliminar-cero-decimal-test
  (testing "Prueba de la funcion: eliminar-cero-decimal"
    (is (= 1 (eliminar-cero-decimal 1.0))))
  (is (= 1.1 (eliminar-cero-decimal 1.1)))
  (is (= 'A (eliminar-cero-decimal 'A))))

(deftest eliminar-cero-entero-test
  (testing "Prueba de la funcion: eliminar-cero-entero"
    (is (= " 0" (eliminar-cero-entero 0))))
  (is (= " 1.5" (eliminar-cero-entero 1.5)))
  (is (= " 1" (eliminar-cero-entero 1)))
  (is (= "A" (eliminar-cero-entero 'A)))
  (is (= nil (eliminar-cero-entero nil)))
  (is (= " .5" (eliminar-cero-entero 0.5)))
  (is (= "-.5" (eliminar-cero-entero -0.5)))
  (is (= "-1.5" (eliminar-cero-entero -1.5))))


(deftest expandir-nexts-test
  (let [sentences (list '(PRINT 1) (list 'NEXT 'A (symbol ",") 'B))
        expected-result (list '(PRINT 1) (list 'NEXT 'A) (list 'NEXT 'B))
        actual-result (expandir-nexts sentences)]
    (is (= expected-result actual-result))))


(deftest cargar-linea-test
  (testing "Prueba de la funcion: cargar-linea")
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
  (testing "Prueba de la funcion: operador?"
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
    (is (= false (operador? 1.0)))
    (is (= false (operador? "X"))))
    (is (= true (operador? "+"))))

(deftest palabra-reservada?-test
  (testing "Prueba de la funcion: palabra-reservada?" 
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
    (is (= true (palabra-reservada?  'MID))))) ;va con $?

(deftest dar-error-test
  (testing "Prueba de la funcion: dar-error"
    (is (= (dar-error 16 [:ejecucion-inmediata 4]) nil))
    (is (= (dar-error "?ERROR DISK FULL" [:ejecucion-inmediata 4]) nil))
    (is (= (dar-error 16 [100 3]) nil))
    (is (= (dar-error "?ERROR DISK FULL" [100 3]) nil))))

