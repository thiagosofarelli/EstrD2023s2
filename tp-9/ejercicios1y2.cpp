// EJERCICIO 1

/*
1- 
Se asigna un stack frame a 'x' con valor 0
Se asigna un stack frame a 'y' con valor 2
Se modifica el stack frame de 'x' y es reemplazado por (0 + 2)

2-
Se asigna un stack frame a 'x' con valor 0
Se asigna un stack frame a 'y' con valor 0
Se recorre el stack frame de 'y' de forma iterativa, sumando el valor de 'y' al
stack frame de 'x', y aumentando en 1 el stack frame de 'y'.


3-
Se asigna un stack frame a 'y' con valor 0
Se asigna un stack frame a 'b' con valor true
Se recorre el stack de frame de 'b' de forma iterativa, sumando en 1 el valor 
de 'y', y modificando el estado de 'b' a false.
*/


// EJERCICIO 2

/* 
1-
Propósito: Describe la secuencia de códigos ASCII entre los caracteres dados por char c1 y char c2 inclusive.
Memoria: Crea un stack frame 'c1' con valor pasado por parámetro, otro 'c2' con valor pasado por parámetro, y uno 'i' con valor '0'.
Se recorre el stack frame de 'i' de forma iterativa, sumando en 1 el valor de 'i' y modificando el stack frame de 'c1' aumentandole el valor dado por 'i'.

// Precondición: c1 < c2
#include <iostream>

void printFromTo(char c1, char c2) {
    for(int i = 0; c1 + i <= c2; i++) {
        std::cout << c1 + i << ", ";
    }
    std::cout << std::endl;
}
*/

/*
2-
Propósito: Describe el factorial del número dado por 'n'
Memoria: Se crea un stack frame 'x' con valor 1, y un stack frame 'n' con el valor pasado por parámetro.
Luego, de forma iterativa,  se reemplaza el valor del stack frame de 'x' por el valor de 'n' multiplicado por el valor de 'x', y se resta 1 al valor
que almacena el stack frame de 'n'. Esto se realiza de forma iterativa hasta que el valor de 'n' iguala a 0.
// Precondición: n >= 0
#include <iostream>
using namespace std;

int fc(int n) {
    int x = 1;
    while(n > 0) {
    x = x * n;
    n--;
    }
    return x;
}
*/


/*
3-
Propósito: Describe la suma de todos los valores entre 'n' y 'm', incluyendo 'n' y 'm'.
Memoria: Se crea un stack frame 'n' con valor pasado por parámetro, otro 'm' con valor pasado por parámetro, y se realiza una iteración que modifica
'n' sumandose a su propio valor el valor 1 por cada vez que se realiza la iteración, hasta que su valor iguala al de 'm'.
// Precondición: n <= m
#include <iostream>
using namespace std;
int ft(int n, int m) {
    if (n == m) {
    return n;
    }
    return n + ft(n+1, m);
}
*/
