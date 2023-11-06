#include <iostream>
#include "Persona.cpp"
using namespace std;

int main() {
    Persona thiago = consPersona("Thiago");

    cout << "Nombre: " << nombre(thiago) << endl;
    cout << "Edad: " << edad(thiago) << endl;

    crecer(thiago);
    cout << "Edad despues de crecer: " << edad(thiago) << endl;

    cambioDeNombre("Fausto", thiago);
    cout << "Nombre despues de cambiarlo: " << nombre(thiago) << endl;
}