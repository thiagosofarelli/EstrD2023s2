#include <iostream>
#include "Fraccion.cpp"
using namespace std;

int main() {
    Fraccion fraccion1 = consFraccion(10, 5);
    Fraccion fraccion2 = consFraccion(2, 7);

    cout << "Fraccion 1: " << numerador(fraccion1) << "/" << denominador(fraccion1) << endl;
    cout << "Fraccion 2: " << numerador(fraccion2) << "/" << denominador(fraccion2) << endl;

    Fraccion suma = sumarFracciones(fraccion1, fraccion2);
    cout << "Suma de fracciones: " << numerador(suma) << "/" << denominador(suma) << endl;

    Fraccion producto = multF(fraccion1, fraccion2);
    cout << "Multiplicacion de fracciones: " << numerador(producto) << "/" << denominador(producto) << endl;

    Fraccion fraccionSimplificada = simplificar(suma);
    cout << "Fraccion simplificada: " << numerador(fraccionSimplificada) << "/" << denominador(fraccionSimplificada) << endl;

    float resultadoDivision = division(fraccion1);
    cout << "Division de fracciones: " << resultadoDivision << endl;

    return 0;
}

