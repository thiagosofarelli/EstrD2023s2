#include <iostream>
#include "Par.cpp"
using namespace std;


int main() {
    Par resultado = consPar(10, 3);

    // Prueba de consPar
    cout << "consPar: " << fst(resultado) << "-" << snd(resultado) << endl;

    // Prueba de fst y snd
    cout << "fst: " << fst(resultado) << endl;
    cout << "snd: " << snd(resultado) << endl;

    // Prueba de maxDelPar
    cout << "maxDelPar: " << maxDelPar(resultado) << endl;

    // Prueba de swap
    Par resultadoSwap = swap(resultado);
    cout << "swap: " << fst(resultadoSwap) << "-" << snd(resultadoSwap) << endl;

    // Prueba de divisionYResto
    Par resultadoDivYResto = divisionYResto(fst(resultado), snd(resultado));
    cout << "divisionYResto: " << fst(resultadoDivYResto) << "-" << snd(resultadoDivYResto) << endl;

    return 0;
}