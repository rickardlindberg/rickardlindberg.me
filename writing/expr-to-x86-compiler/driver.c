#include <stdlib.h>
#include <stdio.h>

long long _exp(void* mem);

int main() {
    void * mem = malloc(8*1024);
    long long result = _exp(mem);
    printf("%d\n", result);
}
