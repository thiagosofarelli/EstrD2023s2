#include <iostream>
using namespace std;
#include "Pokemon.h"

Pokemon consPokemon(TipoDePokemon tipo){
  PokeSt* p = new PokeSt;
  p->tipo = tipo;
  p->vida = 100;
  return (p);
}
// Dado un tipo devuelve un pokémon con 100 % de energía.

TipoDePokemon tipoDePokemon(Pokemon p){
  return(p -> tipo);
}
// Devuelve el tipo de un pokémon.

int energia(Pokemon p){
  return (p -> vida);
}
// Devuelve el porcentaje de energía.

void perderEnergia(int energia, Pokemon p){
   p->vida -= energia;
}
// Le resta energía al pokémon.

bool superaA(Pokemon p1, Pokemon p2) {
    return ((tipoDePokemon (p1) == "Agua"   && tipoDePokemon (p2) == "Fuego")  ||
        (tipoDePokemon (p1) == "Fuego"  && tipoDePokemon (p2) == "Planta") ||
        (tipoDePokemon (p1) == "Planta" && tipoDePokemon (p2) == "Agua")); 
}
// Dados dos pokémon indica si el primero, en base al tipo, es superior al segundo. Agua supera
// a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.

