#include <iostream>
using namespace std;
#include <iomanip>
#include <string>
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
    cout << endl;
}

void cuentaRegresivaR(int n){
    if (n>=0) {
        cout << n << endl;
        cuentaRegresivaR(n-1);
    }
    cout << endl;
}

// 3. 
void desdeCeroHastaN(int n) {
// Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
    for (int i = 0; n >= i; i++) {
        cout << i << endl;   
    }
    cout << endl;
}

void desdeCeroHastaNR(int n){
    int i = 0;
    if (n>=0) {
        cout << i << endl;
        i++;
        desdeCeroHastaNR(n);
    }
    cout << endl;
}


// 4. 
int mult(int n, int m) {
    // Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
    int resultado = 0; 
    for (; m > 0; m--) {
        resultado += n; 
    }
    return resultado; 
}

int multR(int n, int m) {
    if (m == 0) {
        return 0;
    } else if (m == 1) {
        return n;
    } else {
        return n + multR(n, m - 1);
    }
}


// 5. 
void primerosN(int n, string s) {
// Propósito: imprime los primeros n char del string s, separados por un salto de línea.
// Precondición: el string tiene al menos n char.
for (int i = 0; n > i; i++) {
        cout << s[i] << endl;   
    }
    cout << endl;
}

void primerosNR(int n, string s){
    int i = 0;
    while (i < n){
        cout << s[i] << endl;
        i++;
    }
    cout << endl;
}


// 6. 
bool pertenece(char c, string s) {
// Propósito: indica si un char c aparece en el string s.
    for (int i = 0; i < s.length(); i++) {
        if (c == s[i]) {
            return true;
        }
    } 
            return false;      
}

bool perteneceRAux(char c, string s, int n) {
    if (n == 0) {
        return false;
    }
    if (c == s[n - 1]) {
        return true;
    }
    return perteneceRAux(c, s, n - 1);
}

bool perteneceR(char c, string s) {
    return perteneceRAux(c, s, s.length());
}


// 7. 
int apariciones(char c, string s) {
    // Propósito: devuelve la cantidad de apariciones de un char c en el string s.
    int apariciones = 0;
    for (int i = 0; i < s.length(); i++) {
        if (c == s[i]) {
            apariciones += 1;
        }
    }
    return apariciones;
}

int aparicionesRAux(char c, string s, int n) {
    if (s[n-1] == c && n > 0) {
        return (1 + aparicionesRAux (c, s, (n-1)));
    }

    if (s[n-1] != c && n > 0) {
        return (aparicionesRAux (c, s, (n-1)));
    }
}

int aparicionesR(char c, string s) {
    return aparicionesRAux(c, s, s.length());
}


int main() {
    // Prueba de printN 
    printN(5, "Hola");
    // Prueba de printNR
    printNR(5, "Chau");
    // Prueba de cuentaRegresiva
    cuentaRegresiva(10);
    // Prueba de cuentaRegresivaR
    cuentaRegresivaR(5);
    // Prueba de desdeCeroHastaN
    desdeCeroHastaN(3);
    // Prueba de desdeCeroHastaNR
    desdeCeroHastaN(3);
    // Prueba de mult
    cout << mult(45, 108) << endl;
    cout << endl;
    // Prueba de multR
    cout << multR(22, 10) << endl;
    cout << endl;
    // Prueba de primerosN
    primerosN(3, "Thiago");
    // Prueba de primerosNR
    primerosNR(3, "Thiago");
    // Prueba de pertenece
    cout << std::boolalpha;
    cout << pertenece('a', "Thiago") << endl;
    // Prueba de perteneceR
    cout << std::boolalpha;
    cout << perteneceR('o', "Thiago") << endl;
    cout << endl;
    // Prueba de apariciones
    cout << apariciones('t',"termotanque") << endl;
     // Prueba de aparicionesR
    cout << aparicionesR('t',"termotanque") << endl;
    cout << endl;




    // OTRAS PRUEBAS
    string nombre = "Thiago"; // Tiene 6 caracteres, pero van del 0 al 5.
    cout << nombre.length() << endl; // Devuelve 6
    char primerCaracter = nombre[0]; // Devuelve 'T'
    char ultimoCaracter = nombre[5]; // Devuelve 'o'
    cout << primerCaracter << endl;
    cout << ultimoCaracter << endl;
    cout << endl;
}