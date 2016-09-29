#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#define OUT_NAME "solution.csv"
#define SQ(a) ((a) * (a))

FILE *fi;
int population = 100;
int max_eval = 1000;
char filename[101] = "test.tsp";
int size;
double *x, *y;

typedef long long int LL;

void input(int, char**);            // input from given filename
void output(int*);                  // output to "solution.csv"
LL distance(int*);                  // calculate distance of a given tour
LL distance(int, int);              // calculate distance between given 2 cities
LL nint(double);

int main(int argc, char** argv) {
    int i;
    int *init;
    int temp[280] = {
0,
1,
241,
242,
243,
240,
239,
238,
237,
236,
235,
234,
233,
232,
231,
230,
245,
244,
246,
249,
250,
229,
228,
227,
226,
225,
224,
223,
222,
221,
220,
219,
218,
217,
216,
215,
214,
213,
212,
211,
210,
209,
206,
205,
204,
203,
202,
201,
200,
197,
196,
195,
194,
193,
192,
191,
190,
189,
188,
187,
186,
185,
184,
183,
182,
181,
180,
175,
179,
178,
149,
177,
176,
150,
151,
155,
152,
154,
153,
128,
129,
130,
20,
21,
128,
127,
126,
125,
124,
123,
122,
121,
120,
119,
157,
158,
159,
160,
175,
161,
162,
163,
164,
165,
166,
167,
168,
169,
170,
172,
171,
173,
174,
107,
106,
105,
104,
103,
102,
101,
100,
99,
98,
97,
96,
95,
94,
93,
92,
91,
90,
89,
109,
108,
110,
111,
112,
88,
87,
113,
114,
115,
117,
116,
86,
85,
84,
83,
82,
81,
80,
79,
78,
77,
76,
75,
74,
73,
72,
71,
70,
69,
68,
67,
66,
65,
64,
58,
57,
56,
55,
54,
53,
52,
51,
50,
49,
48,
47,
46,
45,
44,
59,
63,
62,
118,
61,
60,
43,
42,
41,
40,
39,
38,
37,
36,
35,
34,
33,
32,
31,
30,
29,
28,
27,
26,
22,
25,
23,
24,
14,
15,
13,
12,
11,
10,
9,
8,
7,
6,
5,
4,
277,
276,
275,
274,
273,
272,
271,
16,
17,
18,
19,
132,
133,
134,
270,
269,
135,
136,
268,
267,
137,
138,
139,
149,
148,
147,
146,
145,
199,
200,
144,
143,
142,
141,
140,
266,
265,
264,
263,
262,
261,
260,
259,
258,
257,
254,
253,
208,
209,
252,
255,
256,
249,
248,
278,
279,
3,
280
    };
    input(argc, argv);
    init = new int[size];
    for (i = 0; i < size; i++){
        init[i] = i;
    }
    printf("%lld\n", distance(init));
    printf("%lld\n", distance(temp));

    fclose(fi);
    delete[] init;
    delete[] x;
    delete[] y;
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
    return;
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
    // LL dist = distance(tour[0], tour[size-1]);
    LL dist = 0;
    for (i = 1; i < size; i++) {
        dist += distance(tour[i-1], tour[i]);
    }
    return dist;
}

LL distance(int from, int to) {
    return nint(sqrt(SQ(x[from] - x[to]) + SQ(y[from] - y[to])));
}

LL nint(double d) { return (LL)(d + 0.5); }
