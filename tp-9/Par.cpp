#include <iostream>
#include "Par.h"
using namespace std;

Par consPar(int x, int y) {
    Par p;
    p.x = x ; p.y = y;
    return (p);
}

int fst(Par p) {
    return (p.x);
}

int snd(Par p) {
    return (p.y);
}

int maxDelPar(Par p) {
    if (p.x > p.y) {
        return (p.x);
    }
    else {
        return (p.y);
    }
}

Par swap(Par p) {
    Par q;
    q.x = snd(p); q.y = fst(p);
    return (q);
}

Par divisionYResto(int n, int m) {
    Par p;
    p.x = (n / m); p.y = (n % m);
    return (p);
}