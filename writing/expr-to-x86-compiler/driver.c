#include <stdlib.h>
#include <stdio.h>

int _exp(void* mem);

int main() {
    void* mem = (void*)malloc(32*sizeof(int));
    int result = _exp(mem);
    printf("%d\n", result);
}
