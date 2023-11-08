#include <iostream>
#include "LinkedList.cpp"

using namespace std;

int main() {
    LinkedList nuevaLL = nil();
    cout << "Esta vacia? " << isEmpty(nuevaLL) << endl;
    Cons(10, nuevaLL);
    cout << "La head de nuevaLL es " << head(nuevaLL) << endl;
    cout << "Esta vacia? " << isEmpty(nuevaLL) << endl;
    Tail(nuevaLL);
    cout << "Al hacer TAIL, ahora la LinkedList es vacia? " << isEmpty(nuevaLL) << endl;
    Cons(30, nuevaLL);
    Cons(20, nuevaLL);
    Cons(10, nuevaLL);
    Snoc(40, nuevaLL);
    return 0; 
}
