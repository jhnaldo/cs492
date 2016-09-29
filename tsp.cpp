#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

#define OUT_NAME "solution.csv"
#define SQ(a) ((a) * (a))
#define MAX_WEIGHT 1000000000
#define MUTATE_MAX_RATIO 20
#define MUTATE_SEL_RATIO 30
#define SEL_FACTOR 10.0
#define UPGRADE_RATIO 5.0
#define BEFORE(x) (((x)+size-1)%size)
#define AFTER(x) (((x)+1)%size)
#define GAUSSIAN_NUM 30
#define MULTIPLE 5

typedef long long int LL;

FILE *fi;
int population = 100;
int max_eval = 100;
char filename[101] = "test.tsp";
int size;
double *x, *y;
int **P, **temp_P;
int *weight;
int *order;
LL *dist, *temp_dist;
bool *temp_check;
bool use_db = false;
char db_name[101];
bool verbose = false;
bool upgrade_verbose = false;

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
void swap(int*, int, int);                      // swap position
bool cross_check(int*, int, int);               // line segments cross check
bool get_line_intersection(
        double, double, double, double,
        double, double, double, double);
void unwind(int*, int, int);                    // unwind cross line
bool tour_check(int*);                          // checking whether it is tour
double gaussian(int);                           // gaussian random
void mutate(int);                               // mutate tour
void read_db();                                 // read db

int main(int argc, char** argv) {
    int i;
    // seed for rand
    srand(time(NULL));

    // get input
    input(argc, argv);

    // generate initial population
    generate_population();

    // generation iteration
    for (i = 0; i < max_eval; i++) {
        if (verbose) {
            print_dist();
            output(P[0]);
        }
        next_generation();
    }

    // display final distance of tour
    printf("%lld\n", dist[0]);

    // print route of the best tour
    output(P[0]);
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
        } else if (strcmp(str, "-db") == 0) {
            // fitness evaluations
            use_db = true;
            strcpy(db_name, argv[++i]);
        } else if (strcmp(str, "-v") == 0) {
            verbose = true;
        } else if (strcmp(str, "-uv") == 0) {
            upgrade_verbose = true;
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
    fscanf(fi, "DIMENSION : %d\n", &size);
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

    // init for temporal check
    temp_check = new bool[size];
}

void output(int* arr) {
    int i, j;
    FILE *db = fopen("out.db", "w");
    FILE *fo = fopen(OUT_NAME, "w");
    printf("writing...");
    for (i = 0; i < size; i++) {
        fprintf(fo, "%d\n", arr[i] + 1);
    }
    fprintf(db, "%d\n", population);
    fprintf(db, "%d\n", size);
    for (i = 0; i < population; i++) {
        fprintf(db, "%lld\n", dist[i]);
        for (j = 0; j < size; j++) {
            fprintf(db, "%d%s", P[i][j], ((i==size-1)?"\n":" "));
        }
    }
    fclose(fo);
    fclose(db);
    printf("done!!\n");
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

void rand_init_tour(int* init) {
    int i;
    for (i = 0; i < size; i++) {
        init[i] = i;
        weight[i] = rand() % MAX_WEIGHT;
    }
    qsort(init, size, sizeof(int), int_compare);
}

void generate_population() {
    int i;
    // init for distance of tours
    dist = new LL[population * MULTIPLE];
    temp_dist = new LL[population * MULTIPLE];

    // init for order
    order = new int[population * MULTIPLE];

    // init for population
    P = new int*[MULTIPLE * population];
    temp_P = new int*[MULTIPLE * population];
    for (i = 0; i < MULTIPLE * population; i++){
        P[i] = new int[size];
        if (!use_db && i < population) {
            rand_init_tour(P[i]);
            dist[i] = distance(P[i]);
            upgrade(i);
        }
    }
    if (use_db) read_db();
}

void crossover(int father, int mother, int child) {
    int i, j;
    int delta = rand() % size;
    int cut = (int)gaussian(size) % size;
    for (i = 0; i < size; i++){
        temp_check[i] = false;
    }
    for (i = 0; i < cut; i++){
        int k = (delta + i) % size;
        P[child][k] = P[father][k];
        temp_check[P[father][k]] = true;
    }
    for (i = cut, j = 0; i < size; i++, j++){
        int k = (delta + i) % size;
        while (temp_check[P[mother][j]]) j++;
        P[child][k] = P[mother][j];
    }
    dist[child] = distance(P[child]);
}

void next_generation() {
    int father, mother, i;
    for (i = 0; i < (MULTIPLE-1) * population; i++){
        father = random_select();
        mother = random_select(father);
        crossover(father, mother, i + population);
        if (random_pass(UPGRADE_RATIO)) upgrade(i + population);
        mutate(i + population);
        // printf("%lld\n", dist[i + population]);
    }
    population_sort(population * MULTIPLE);
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
    if (all) n *= MULTIPLE;
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
    for (i = 0; i < MULTIPLE*population; i++){
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
    delete[] temp_check;
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
        printf("%4d", tour[i]);
        if ((i+1) % 20 == 0) printf("\n");
    }
}

void upgrade(int k) {
    int *tour = P[k];
    if (upgrade_verbose) printf("upgrade....");
    // unwind line segment cross
    while (true) {
        int i, j;
        int count = 0;
        bool stop = true;
        for (i = 0; i < size; i++){
            for (j = i+2; j < size; j++){
                if (cross_check(tour, i, j)) {
                    int new_dist;
                    unwind(tour, i, j);
                    new_dist = distance(tour);
                    stop = dist[k] == new_dist;
                    dist[k] = new_dist;
                    if (upgrade_verbose) printf("%lld\n", dist[k]);
                    count++;
                }
            }
        }
        if (count <= 2 || stop) break;
    }
    if (upgrade_verbose) printf("done!!\n");
}

void swap(int* tour, int x, int y) {
    int tmp;
    tmp = tour[x];
    tour[x] = tour[y];
    tour[y] = tmp;
}

bool cross_check(int* tour, int left_idx, int right_idx) {
    int left_before_idx = BEFORE(left_idx);
    int right_before_idx = BEFORE(right_idx);
    int left = tour[left_idx];
    int left_before = tour[left_before_idx];
    int right = tour[right_idx];
    int right_before = tour[right_before_idx];
    return get_line_intersection(
            x[left], y[left], x[left_before], y[left_before],
            x[right], y[right], x[right_before], y[right_before]);
}

// http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
bool get_line_intersection(
        double p0_x, double p0_y, double p1_x, double p1_y,
        double p2_x, double p2_y, double p3_x, double p3_y)
{
    double s1_x, s1_y, s2_x, s2_y;
    s1_x = p1_x - p0_x;
    s1_y = p1_y - p0_y;
    s2_x = p3_x - p2_x;
    s2_y = p3_y - p2_y;

    double s, t;
    s = (-s1_y * (p0_x - p2_x) + s1_x * (p0_y - p2_y)) / (-s2_x * s1_y + s1_x * s2_y);
    t = (s2_x * (p0_y - p2_y) - s2_y * (p0_x - p2_x)) / (-s2_x * s1_y + s1_x * s2_y);

    if (s >= 0 && s <= 1 && t >= 0 && t <= 1)
    {
        return true;
    }

    return false; // No collision
}

void unwind(int *tour, int left, int right){
    int left_before = BEFORE(left);
    int right_before = BEFORE(right);
    int gap = (right - left + size) % size;
    int i;
    for (i = 0; i < gap / 2; i++){
        swap(tour, (left+i)%size, ((right-i-1)+size) % size);
    }
}

double gaussian(int max) {
    double sum = 0;
    int i;
    for (i = 0; i < GAUSSIAN_NUM; i++){
        sum += rand() % max;
    }
    return sum / GAUSSIAN_NUM;
}

void mutate(int tour_idx) {
    int *tour = P[tour_idx];
    if (random_pass(MUTATE_SEL_RATIO)) {
        int i, j;
        int delta = rand() % size;
        int max = size / 100.0 * MUTATE_MAX_RATIO;
        int cut = (int)gaussian(max) % max;
        for (i = 0; i < 2 * cut; i++){
            int random_x = (rand() % cut + delta) % cut;
            int random_y = (rand() % cut + delta) % cut;
            swap(tour, random_x, random_y);
        }
    }
}

void read_db() {
    int i, j;
    FILE *db = fopen(db_name, "r");
    fscanf(db, "%d", &population);
    fscanf(db, "%d", &size);
    int tmp;
    for (i = 0; i < population; i++) {
        fscanf(db, "%d", &tmp);
        for (j = 0; j < size; j++) {
            fscanf(db, "%d", &P[i][j]);
        }
        dist[i] = distance(P[i]);
    }
    fclose(db);
}
