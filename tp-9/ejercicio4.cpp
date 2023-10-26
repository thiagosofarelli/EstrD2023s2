#include <iostream>
using namespace std;
// Ejercicio 4

// Dar dos implementaciones para las siguientes funciones, una iterativa y otra recursiva, y utilizando
// la menor cantidad posible de variables. Recordar definir subtareas en caso de que sea estrictamente necesario.

// 1. 
void printN(int n, string s){
// Propósito: imprime n veces un string s. (FORMA ITERATIVA)
for (int i = 0; i < n; ++i) {
        cout << s;
    }
    cout << endl;
}

void printNR(int n, string s){
// Propósito: imprime n veces un string s. (FORMA RECURSIVA)
if (n>0) {
        cout << s;
        printNR(n-1, s);
    }
    cout << endl;
}

 
// 2.
void cuentaRegresiva(int n) {
    // Propósito: imprime los números desde n hasta 0, incluyendo n, separados por saltos de línea.
    for (; n >= 0; n--) {
        cout << n << endl;   
    }
}

void cuentaRegresivaR(int n){
    if (n>=0) {
        cout << n << endl;
        cuentaRegresivaR(n-1);
    }
}

/*
// 3. 
void desdeCeroHastaN(int n)
// Propósito: imprime los números de 0 hasta n, separados por saltos de línea.

// 4. 
int mult(int n, int m)
// Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).

// 5. 
void primerosN(int n, string s)
// Propósito: imprime los primeros n char del string s, separados por un salto de línea.
// Precondición: el string tiene al menos n char.

// 6. 
bool pertenece(char c, string s)
// Propósito: indica si un char c aparece en el string s.

// 7. 
int apariciones(char c, string s)
// Propósito: devuelve la cantidad de apariciones de un char c en el string s.
*/
int main() {
    // Prueba de printN 
    printN(5, "Hola");
    // Prueba de printNR
    printNR(5, "Chau");
    // Prueba de cuentaRegresiva
    cuentaRegresiva(10);
    // Prueba de cuentaRegresivaR
    cuentaRegresivaR(5);
}