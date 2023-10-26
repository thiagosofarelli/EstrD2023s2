#include <iostream>
using namespace std;

// Definición de la estructura
struct RegistroDeP {
    int x;
    int y;
};

// Alias de tipo para la estructura
typedef struct RegistroDeP Par;  

// Propósito: construye un par
Par consPar(int x, int y);
// Propósito: devuelve la primera componente
int fst(Par p);
// Propósito: devuelve la segunda componente
int snd(Par p);
// Propósito: devuelve el mayor componente
int maxDelPar(Par p);
// Propósito: devuelve un par con las componentes intercambiadas
Par swap(Par p);
// Propósito: devuelve un par donde la primer componente
// es la división y la segunda el resto entre ambos números
Par divisionYResto(int n, int m);
