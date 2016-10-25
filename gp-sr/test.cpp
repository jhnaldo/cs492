#include <stdio.h>

int main(int argc, char** argv){
    char* words[10000];
    expr_input(argv[1], words);
    csv_input(argv[2], 
    return 0;
}

void expr_input(char* str, char* words[10000]) {
    char str[101];
    char *ptr;
}

int main(void){

    printf("함수 호출 전의 스트링 : %s\n" , str) ;

    //ptr = strtok(str, ",");
    //printf("%s\n" , ptr);

    ptr = strtok(str, ",");

    while(ptr != NULL ){

        printf( "%s\n" , ptr);
        ptr = strtok(NULL, ",");
    }

    printf("함수 호출 후의 스트링 : %s\n" , str);
    return 0;
}
