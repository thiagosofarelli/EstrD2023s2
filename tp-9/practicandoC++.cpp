#include <iostream>
using namespace std;

int succ(int n) { // GENERA UN CUADRADO PARA n
    return(n+1);
}

int fact(int n) { // No es Iterativo por lo que la memoria es lineal, ya que se crean muchos stack frames.
    if (n==0)
         { return(1);}
    else { return(n*(fact(n-1)));}
}

int ifact(int n) { // Es Iterativo por lo que la memoria es constante, ya que se van pisando los stack frames.
    int f = 1;
    while (n>0)
        { f = f*n; n = n-1; }
    return (f);
}

/*
Los procedimientos/funciones en C/C++ son FUNCIONES con EFECTOS PERMANENTES.
Cada función tiene su propio frame.
Al anidar funciones, los frames se apilan.
A la memoria de C se la conoce como STACK (o MEMORIA ESTÁTICA). "Si algo está en MEMORIA ESTÁTICA, es porque está dentro de un STACK FRAME dentro de un STACK."
A cada frame se lo conoce como STACK FRAME.
Los parámetros tambien tienen espacio de memoria (y son como cualquier otra variable).

*/

/*
int main() { // GENERA UN CUADRADO PARA x Y PARA c
  int x = 17;
  char c = 'a';
  cout << x << endl;
  cout << c << endl;
}
*/

/*
int main() { // GENERA UN CUADRADO PARA x Y PARA y
  int x = 17;
  int y = succ(x);
  cout << x << "+1=" << y << endl;
}
*/

/*
int main() {
    int x = 4;
    int y = fact(x); // EFICIENCIA (O(X) de memoria) - Es Lineal en X por usar fact.
    cout << "fact(" << x << ")=";
    cout << y << endl;
}
*/


int main() {
    int x = 4;
    int y = ifact(x); // EFICIENCIA (O(1) de memoria) - Es CONSTANTE en X por usar ifact.
    cout << "fact(" << x << ")=";
    cout << y << endl;
}


// EN C/C++ solamente usamos recursión PARA ÁRBOLES.
// Sino, se debe resolver con ITERACIÓN (en iteración, la memoria es constante ya que se va auto-reemplazando.
