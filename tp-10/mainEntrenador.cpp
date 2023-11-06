using namespace std;
#include <iostream>
#include "Pokemon.cpp"
#include "Entrenador.cpp"


int main(){
    Pokemon pikachu = consPokemon("Agua");
    Pokemon bulbasaur = consPokemon("Fuego");
    Pokemon* pokemones1 = new Pokemon[2];
    pokemones1[0] = pikachu;
    pokemones1[1] = bulbasaur;
    Pokemon* pokemones2 = new Pokemon[1];
    pokemones2[0] = pikachu;
    Entrenador thiago = consEntrenador("Thiago", 2, pokemones1);
    Entrenador buchu = consEntrenador("Buchu", 1, pokemones2);
    cout << "Nombres: " << nombreDeEntrenador(thiago) << " y " << nombreDeEntrenador(buchu) << endl;
    cout << "Thiago posee " << cantidadDePokemon(thiago) << " pokemones" << endl;
    cout << "Thiago posee " << cantidadDePokemonDe("Agua", thiago) << " pokemones de tipo agua" << endl;
    cout << "El pokemon n1 de Thiago es de tipo " << tipoDePokemon(pokemonNro(1, thiago)) << endl;
    cout << "¿Thiago le gana a todos los de Buchu? " << leGanaATodos(thiago, buchu) << endl;
}