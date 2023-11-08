#include <iostream>
using namespace std;
#include "LinkedList.h"

struct NodoL{
    int elem;         // valor del nodo
    NodoL *siguiente; // puntero al siguiente nodo
}; 

struct LinkedListSt{
    // INV.REP.: cantidad indica la cantidad de nodos que se pueden recorrer
    // desde primero por siguiente hasta alcanzar a NULL
    int cantidad;   // cantidad de elementos
    NodoL *primero; // puntero al primer nodo
}; 

struct IteratorSt{
    NodoL *current;
};

LinkedList nil(){
    LinkedListSt* ll = new LinkedListSt;
    ll->cantidad = 0;
    ll->primero = NULL;
    return ll;
}
// Crea una lista vacía.

bool isEmpty(LinkedList xs){
    return(xs->cantidad == 0);
}
// Indica si la lista está vacía.

int head(LinkedList xs){
// Devuelve el primer elemento.
        return xs->primero->elem;
}

void Cons(int x, LinkedList xs){
// Agrega un elemento al principio de la lista.
    NodoL* nodo = new NodoL;
    nodo->elem  = x;
    nodo->siguiente = xs->primero;
    xs->cantidad++;
    xs->primero = nodo;
} 

void Tail(LinkedList xs){
// Quita el primer elemento.
    if (!isEmpty (xs)){
        NodoL* temp = xs->primero;
        xs->cantidad--;
        xs->primero = temp->siguiente;
        delete temp;
    } 
}

int length(LinkedList xs);
// Devuelve la cantidad de elementos.

void Snoc(int x, LinkedList xs);
// Agrega un elemento al final de la lista.

ListIterator getIterator(LinkedList xs);
// Apunta el recorrido al primer elemento.

int current(ListIterator ixs);
// Devuelve el elemento actual en el recorrido.

void SetCurrent(int x, ListIterator ixs);
// Reemplaza el elemento actual por otro elemento.

void Next(ListIterator ixs);
// Pasa al siguiente elemento.

bool atEnd(ListIterator ixs);
// Indica si el recorrido ha terminado.

void DisposeIterator(ListIterator ixs);
// Libera la memoria ocupada por el iterador.

void DestroyL(LinkedList xs);
// Libera la memoria ocupada por la lista.
