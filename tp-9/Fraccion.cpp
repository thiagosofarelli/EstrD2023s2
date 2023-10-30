#include <iostream>
#include "Fraccion.h"
using namespace std;

Fraccion consFraccion(int numerador, int denominador){
    Fraccion f;
    f.numerador = numerador;
    f.denominador = denominador;
    return (f);
}

int numerador(Fraccion f){
    return (f.numerador);
}

int denominador(Fraccion f){
    return (f.denominador);
}

float division(Fraccion f){
    return (f.numerador / f.denominador);
}

Fraccion multF(Fraccion f1, Fraccion f2){
    return(consFraccion(f1.numerador * f2.numerador, f1.denominador * f2.denominador));
}

int mcd(int a, int b) {
    while (b != 0) {
        int temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}

Fraccion simplificar(Fraccion f) {
    int divisorComun = mcd(f.numerador, f.denominador);
    f.numerador /= divisorComun;
    f.denominador /= divisorComun;
    return f;
}

Fraccion sumarFracciones(Fraccion f1, Fraccion f2) {
    Fraccion sumada;
    sumada.numerador = (f1.numerador * f2.denominador) + (f1.denominador * f2.numerador);
    sumada.denominador = f1.denominador * f2.denominador;
    return simplificar(sumada);
}


