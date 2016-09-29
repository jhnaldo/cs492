#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

#define OUT_NAME "solution.csv"
#define SQ(a) ((a) * (a))
#define MAX_WEIGHT 1000000000
#define MUTATE_RATIO 1.0
#define MUTATE_REL_RADIUS 20.0
#define SEL_FACTOR 10.0

typedef long long int LL;

FILE *fi;
int population = 100;
int max_eval = 1000;
char filename[101] = "test.tsp";
int size;
double *x, *y;
int **P, **temp_P;
int *weight;
int *order;
LL *dist, *temp_dist;

void input(int, char**);                        // input from given filename
void output(int*);                              // output to "solution.csv"
LL distance(int*);                              // calculate distance of a given tour
LL distance(int, int);                          // calculate distance between given 2 cities
LL nint(double);                                // double -> long long int
int int_compare(const void *, const void *);    // compare between long long int
int order_compare(const void *, const void *);  // compare between population
void init_tour(int*);                           // initial tour
void generate_population();                     // generate initial population
void next_generation();                         // create next generation
void crossover(int, int, int);                  // cross-over for 2 tours
void finish();                                  // delete all memories
void print_dist(bool all = false);              // print distances of tours
void print_tour(int*);                          // print a given tour
bool random_pass(double);                       // return true with given probability
int random_select(int except = -1);             // position random selection
void population_sort(int);                      // sorting population
void upgrade(int);                              // to local maximum of given tour
int swap_delta(int*, int);                      // calculate distance delat after swap
void swap(int*, int, int);                      // swap position

int main(int argc, char** argv) {
    int i;
    // seed for rand
    srand(time(NULL));

    // get input
    input(argc, argv);

    // generate initial population
    generate_population();
    print_dist();

    // generation iteration
    for (i = 0; i < max_eval; i++) {
        next_generation();
        print_dist();
    }

    // display final distance of tour
    printf("%lld\n", dist[0]);
    finish();
    return 0;
}

///////////////////////////////////////////////
// Helper
///////////////////////////////////////////////
void input(int argc, char** argv) {
    int i;
    // option input
    for (i = 1; i < argc; i++) {
        char* str = argv[i];
        int len = strlen(str);
        if (strcmp(str, "-p") == 0) {
            // population
            population = atoi(argv[++i]);
        } else if (strcmp(str, "-f") == 0) {
            // fitness evaluations
            max_eval = atoi(argv[++i]);
        } else {
            // filename
            strcpy(filename, str);
            break;
        }
    }

    // description input
    char temp[101];
    fi = fopen(filename, "r");
    fscanf(fi, "NAME : %s\n", temp);
    fscanf(fi, "COMMENT : %[^\n]\n", temp);
    fscanf(fi, "TYPE : %s\n", temp);
    fscanf(fi, "DIMENSION: %d\n", &size);
    fscanf(fi, "EDGE_WEIGHT_TYPE : %s\n", temp);
    fscanf(fi, "%s\n", temp);

    // data input
    x = new double[size];
    y = new double[size];
    for (i = 0; i < size; i++) {
        int temp;
        fscanf(fi, "%d %lf %lf", &temp, &x[i], &y[i]);
    }

    // init for global weight
    weight = new int[size];

    // init for distance of tours
    dist = new LL[population * 2];
    temp_dist = new LL[population * 2];

    // init for order
    order = new int[population * 2];
}

void output(int* arr) {
    int i;
    FILE *fo = fopen(OUT_NAME, "w");
    for (i = 0; i < size; i++) {
        fprintf(fo, "%d\n", arr[i]);
    }
    fclose(fo);
}

LL distance(int* tour) {
    int i;
    LL dist = distance(tour[0], tour[size-1]);
    for (i = 1; i < size; i++) {
        dist += distance(tour[i-1], tour[i]);
    }
    return dist;
}

LL distance(int from, int to) {
    return nint(sqrt(SQ(x[from] - x[to]) + SQ(y[from] - y[to])));
}

LL nint(double d) { return (LL)(d + 0.5); }

int int_compare(const void *left, const void *right) {
    int l_pos = *(int*)left;
    int r_pos = *(int*)right;
    return (int)(weight[r_pos] - weight[l_pos]);
}

// TODO better initial tour
void init_tour(int* init) {
    int i;
    for (i = 0; i < size; i++) {
        init[i] = i;
        weight[i] = rand() % MAX_WEIGHT;
    }
    qsort(init, size, sizeof(int), int_compare);
}

void generate_population() {
    int i;
    P = new int*[2 * population];
    temp_P = new int*[2 * population];
    for (i = 0; i < 2 * population; i++){
        P[i] = new int[size];
        if (i < population) {
            init_tour(P[i]);
            dist[i] = distance(P[i]);
            upgrade(i);
        }
    }
}

void crossover(int father, int mother, int child) {
    int i;
    for (i = 0; i < size; i++){
        int mutate = 0;
        if (rand() % 100 < random_pass(MUTATE_RATIO)) {
            int sign = (rand() % 2) * 2 - 1;
            mutate = sign * (rand() % (int)(size / 100.0 * MUTATE_REL_RADIUS));
        }
        weight[i] = (P[father][i] + P[mother][i]) / 2 + mutate;
        P[child][i] = i;
    }
    qsort(P[child], size, sizeof(int), int_compare);
    dist[child] = distance(P[child]);
}

void next_generation() {
    int father, mother, i;
    for (i = 0; i < population; i++){
        father = random_select();
        mother = random_select(father);
        crossover(father, mother, i + population);
        upgrade(i);
    }
    population_sort(population * 2);
}

void population_sort(int n) {
    int i;
    for (i = 0; i < n; i++) {
        order[i] = i;
    }
    qsort(order, n, sizeof(int), order_compare);
    for (i = 0; i < n; i++) {
        temp_dist[i] = dist[i];
        temp_P[i] = P[i];
    }
    for (i = 0; i < n; i++) {
        dist[i] = temp_dist[order[i]];
        P[i] = temp_P[order[i]];
    }
}

void print_dist(bool all) {
    int i;
    int n = population;
    if (all) n *= 2;
    for (i = 0; i < n; i++){
        printf("%lld%s", dist[i], ((i+1)%10?", ":"\n"));
    }
    printf("\n");
}

bool random_pass(double percent) {
    int k = rand() % 10000;
    int t = (int)(percent * 100);
    return k < t;
}

int random_select(int except) {
    int i;
    for (i = 0; i < population; i++) {
        if (i != except && random_pass(SEL_FACTOR)) {
            return i;
        }
    }
    i = rand() % population;
    if (i == except)
        return population - 1;
    return i;
}

void finish() {
    int i;
    for (i = 0; i < 2*population; i++){
        delete[] P[i];
    }
    delete[] P;
    delete[] temp_P;

    fclose(fi);
    delete[] x;
    delete[] y;
    delete[] dist;
    delete[] temp_dist;
    delete[] order;
    delete[] weight;
}

int order_compare(const void *left, const void *right) {
    LL l_pos = dist[*(int*)left];
    LL r_pos = dist[*(int*)right];
    if (l_pos < r_pos) return -1;
    else if (l_pos > r_pos) return 1;
    else return 0;
}

void print_tour(int* tour) {
    int i;
    for (i = 0; i < size; i++){
        printf("%d\n", tour[i]);
    }
}

void upgrade(int k) {
    int *tour = P[k];
    while (true) {
        int min_delta = swap_delta(tour, 0);
        int min_idx = 0;
        int i;
        for (i = 1; i < size; i++) {
            int delta = swap_delta(tour, i);
            if (min_delta > delta) {
                min_delta = delta;
                min_idx = i;
            }
        }
        if (min_delta < 0) swap(tour, min_idx, (min_idx+size-1) % size);
        else break;
    }
    dist[k] = distance(tour);
}

int swap_delta(int* tour, int x) {
    int x_after = (x+1)%size;
    int y = (x+size-1)%size;
    int y_before = (y+size-1)%size;
    return (distance(tour[x], tour[y_before]) + distance(tour[y], tour[x_after])
        - distance(tour[x], tour[x_after]) - distance(tour[y], tour[y_before]));
}

void swap(int* tour, int x, int y) {
    int tmp;
    tmp = tour[x];
    tour[x] = tour[y];
    tour[y] = tmp;
}
