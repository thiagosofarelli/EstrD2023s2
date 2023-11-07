#include <iostream>
using namespace std;
#include "ArrayList.h"

// Inv. Rep.: Capacidad > Cantidad de elementos.

struct ArrayListSt {
    int cantidad;
    int* elementos; 
    int capacidad;
};

//Crea una lista con 0 elementos
// Empieza con 16 de capacidad
ArrayList newArrayList() {
    ArrayList array = new ArrayListSt;
    array->cantidad = 0;
    array->elementos = new int[16];
    array->capacidad = 16;
    return array;
}

//Crea una lista con 0 elementos y una capacidad dada por parametro
ArrayList newArrayListWith(int capacidad){
    ArrayList array = new ArrayListSt;
    array->cantidad = 0;
    array->elementos = new int(capacidad);
    array->capacidad = capacidad;
    return array;
}


//Devuelve la cantidad de elementos existentes
int lengthAL(ArrayList xs){
return xs->cantidad;
}
//Devuelve en iesimo elemento de la lista
int get(int i, ArrayList xs){
return xs->elementos[i-1];
}
//Reemplaza el iesimo elemento por otro dado
void set(int i, int x, ArrayList xs){
     if (xs->cantidad >= i) {
        xs->elementos[i-1] = x;
    } 
}

//Decrementa o aumenta la capacidad del array 
// En caso de decrementarla se pierden los elementos del final de la lista
void decrementar(int x, ArrayList xs) {
    int* temp = xs->elementos; 
    int* nuevosElementos = new int[x];
    int i = 0;
    for (; i < x && i < xs->cantidad; i++) {
        nuevosElementos[i] = xs->elementos[i];
    }
    delete[] temp;  
    xs->capacidad = x;
    xs->cantidad = i;  
    xs->elementos = nuevosElementos;
}


void aumentar(int x, ArrayList xs) {
    int* nuevosElementos = new int[x];
    for (int i = 0; i < xs->cantidad; i++) {
        nuevosElementos[i] = xs->elementos[i];
    }
    delete xs->elementos;
    xs->capacidad = x;
    xs->elementos = nuevosElementos;
}

void resize(int capacidad, ArrayList xs) {
    if (capacidad < xs->capacidad) {
        decrementar(capacidad, xs);
    } else {
        aumentar(capacidad, xs);
    }
}

void add(int x, ArrayList xs){
    if (xs->cantidad == xs-> capacidad){
        aumentar(xs->capacidad * 2, xs);
    }
    int posicionAAgregar = xs->cantidad;
    xs->elementos[posicionAAgregar] = x;
    xs->cantidad++;
}

void remove(ArrayList xs) {
    int posicionABorrar = xs->cantidad;
    xs->elementos[posicionABorrar] = -1;
    xs->cantidad--;
}