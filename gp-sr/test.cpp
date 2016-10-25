#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#define DIMENSION 57
#define STACK_MAX_SIZE 1000

int expr_input(char*, char*[10000]);                    // get expression
double data_input(char*, double[DIMENSION]);            // get data from string
double mse(char*[10000], int, char*);                   // Mean Square Error
double eval(char*[10000], int, double[DIMENSION]);      // evaluation
bool eq(char*, const char*);                                  // check equal string

class Stack {
public:
    Stack() {
        size = 0;
    }
    void push(double data) { arr[size++] = data; }
    void display() {
        int i;
        printf("SIZE: %d\n", size);
        printf("----------\n");
        for (i = 0; i < size; i++)
            printf("%lf\n", arr[i]);
        printf("----------\n");
    }
    double pop() { return arr[--size]; }
    int get_size() { return size; }
private:
    double arr[STACK_MAX_SIZE];
    int size;
};

int main(int argc, char** argv){
    char* words[10000];
    int size;
    int i;

    size = expr_input(argv[1], words);
    printf("%lf\n", mse(words, size, argv[2]));
    return 0;
}

int expr_input(char* str, char* words[10000]) {
    char *ptr = str;
    int size = 0;

    ptr = strtok(str, " ");
    while(ptr) {
        words[size++] = ptr;
        ptr = strtok(NULL, " ");
    }
    return size;
}

double data_input(char* str, double x[DIMENSION]) {
    char *ptr = str;
    int i;

    ptr = strtok(str, ",");
    for (i = 0; i < DIMENSION; i++){
        x[i] = atof(ptr);
        ptr = strtok(NULL, ",");
    }
    return atof(ptr);
}

double mse(char* words[10000], int size, char* filename) {
    FILE *fi = fopen(filename, "r");
    char str[1001];
    double x[DIMENSION], y, result, sum = 0.0;
    int k = 0;
    fscanf(fi, "%s", str);

    while(fscanf(fi, "%s", str) == 1){
        y = data_input(str, x);
        result = eval(words, size, x);
        sum += (result - y) * (result - y);
        k++;
    }
    return sum /= k;
}

double eval(char* words[10000], int size, double x[DIMENSION]) {
    int i;
    Stack* stack = new Stack();
    double result;
    for (i = 0; i < size; i++) {
        char* str = words[i];
        if (str[0] == 'x') {
            stack->push(x[atoi(str + 1) - 1]);
        } else if(eq(str, "~")) {
            stack->push(-(stack->pop()));
        } else if(eq(str, "abs")) {
            stack->push(fabs(stack->pop()));
        } else if(eq(str, "sin")) {
            stack->push(sin(stack->pop()));
        } else if(eq(str, "cos")) {
            stack->push(cos(stack->pop()));
        } else if(eq(str, "tan")) {
            stack->push(tan(stack->pop()));
        } else if(eq(str, "asin")) {
            stack->push(asin(stack->pop()));
        } else if(eq(str, "acos")) {
            stack->push(acos(stack->pop()));
        } else if(eq(str, "atan")) {
            stack->push(atan(stack->pop()));
        } else if(eq(str, "sinh")) {
            stack->push(sinh(stack->pop()));
        } else if(eq(str, "cosh")) {
            stack->push(cosh(stack->pop()));
        } else if(eq(str, "tanh")) {
            stack->push(tanh(stack->pop()));
        } else if(eq(str, "exp")) {
            stack->push(exp(stack->pop()));
        } else if(eq(str, "sqrt")) {
            stack->push(sqrt(stack->pop()));
        } else if(eq(str, "log")) {
            stack->push(log(stack->pop()));
        } else {
            double right = stack->pop();
            double left = stack->pop();
            if(eq(str, "+")) {
                stack->push(left + right);
            } else if(eq(str, "-")) {
                stack->push(left - right);
            } else if(eq(str, "*")) {
                stack->push(left * right);
            } else if(eq(str, "/")) {
                stack->push(left / right);
            } else if(eq(str, "^")) {
                stack->push(pow(left, right));
            } else {
                stack->push(left);
                stack->push(right);
                stack->push(atof(str));
            }
        }
    }
    result = stack->pop();
    delete stack;
    return result;
}

bool eq(char* left, const char* right) {
    return strcmp(left, right) == 0;
}
