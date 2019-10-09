#include <stdlib.h>
#include <stdio.h>

int expr(void* mem);

int main() {
    void* mem = (void*)malloc(32*sizeof(int));
    int result = expr(mem);
    printf("%d\n", result);
}
