#include <iostream>
#include "Persona.h"
using namespace std;

Persona consPersona(string n) {
    PersonaSt* p = new PersonaSt;
    p->nombre = n;
    p-> edad = 0;
    return(p);
}

string nombre(Persona p) {
    return (p -> nombre); 
}

int     edad(Persona p) {
    return (p -> edad);
}

void    crecer(Persona p) {
    p -> edad++;
}

void cambioDeNombre(string nombre, Persona p) {
    p -> nombre = nombre;
}

bool esMayorQueLaOtra(Persona p1, Persona p2) {
    return (p1 -> edad > p2-> edad);
}

Persona laQueEsMayor(Persona p1, Persona p2) {
    if (esMayorQueLaOtra(p1, p2)){
        return (p1);
    } else {
        return (p2);
    }
}