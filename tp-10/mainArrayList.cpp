using namespace std;
#include <iostream>
#include "ArrayList.cpp"

int main(){
    ArrayList lista = newArrayList();
    cout << "La longitud es de " << lengthAL(lista) << " elementos" << endl;
    add(100, lista);
    cout << "La longitud es de " << lengthAL(lista) << " elementos" << endl;
    add(200, lista);
    cout << "El primer elemento del array es " << get(1, lista) << endl;
    cout << "El segundo elemento del array es " << get(2, lista) << endl;
    set(1, 500, lista);
    cout << "El primer elemento del array al ser reemplazado es " << get(1, lista) << endl;
    resize(1, lista);
    cout << "Longitud del array al decrementarlo a 1: " << lengthAL(lista) << endl;
}