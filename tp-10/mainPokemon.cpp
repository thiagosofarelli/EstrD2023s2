#include <iostream>
#include "Pokemon.cpp"
using namespace std;

int main() {
    Pokemon pikachu = consPokemon("Agua");
    Pokemon bulbasaur = consPokemon("Fuego");

    cout << "Tipo: " << tipoDePokemon(pikachu) << endl;
    cout << "Energia: " << energia(pikachu) << endl;

    perderEnergia(20, pikachu);
    cout << "Energia luego de perder 20: " << energia(pikachu) << endl;

    
    cout << "¿Pikachu supera a Bulbasaur? " << superaA(pikachu, bulbasaur) << endl;
}