#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

typedef enum op {
    PLUS,
    MINUS,
    DIV,
    MULT,
    PUSH,
    END,
} Op;

typedef struct code {
    Op op;
    int value;
} Code;

typedef struct stack {
    struct item * top;
} Stack;

typedef struct item {
    int value;
    struct item * parent;
} Item;

void stack_push(Stack * stack, int value) {
    Item * new_item = (Item *)malloc(sizeof(Item));
    new_item->parent = stack->top;
    new_item->value = value;
    stack->top = new_item;
}

int stack_pop(Stack * stack) {
    Item * item = stack->top;
    int value = stack->top->value;
    stack->top = stack->top->parent;
    free(item);
    return value;
}

int run(Code * code, Stack * stack) {
    int left, right;
    while (1) {
        switch (code->op) {
            case PLUS:
                right = stack_pop(stack);
                left = stack_pop(stack);
                stack_push(stack, left + right);
                break;
            case MINUS:
                right = stack_pop(stack);
                left = stack_pop(stack);
                stack_push(stack, left - right);
                break;
            case DIV:
                right = stack_pop(stack);
                left = stack_pop(stack);
                stack_push(stack, left / right);
                break;
            case MULT:
                right = stack_pop(stack);
                left = stack_pop(stack);
                stack_push(stack, left * right);
                break;
            case PUSH:
                stack_push(stack, code->value);
                break;
            default:
                return stack_pop(stack);
        }
        code++;
    }
}

void main() {
    Stack stack = { NULL };
    Code code[] = {
        { PUSH, 1 },
        { PUSH, 2 },
        { PUSH, 3 },
        { MULT, 0 },
        { PLUS, 0 },
        { END, 0 },
    };
    printf("%d\n", run(code, &stack));
}
