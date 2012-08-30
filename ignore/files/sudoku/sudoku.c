/*
 * Simple Sudoku Solver
 * 2012/7/25 Takuo Watanabe
 * (Use -std=c99 for compilation with gcc)
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>

int board_size = 9;
bool show_all_answers = true;

int readBoard(FILE *fp, int n, int board[]) {
    int count = 0;
    for (int i = 0; i < n * n; i++) {
        if (fscanf(fp, "%d", &board[i]) < 1)
            return count;
        count++;
    }
    return count;
}

static inline void showNum(int v) {
    if (v == 0)
        printf(".");
    else
        printf("%d", v);
}

void showBoard(int n, int board[]) {
    for (int y = 0; y < n; y++) {
        showNum(board[y * n]);
        for (int x = 1; x < n; x++) {
            printf(" ");
            showNum(board[x + y * n]);
        }
        printf("\n");
    }
}

// check whether v can be placed in board[k]
// assert: board[k] == 0
// n : board size, m : box size (= sqrt(n))
bool check(int n, int m, int board[], int k, int v) {
    int x = k % n, y = k / n;
    for (int i = 0; i < n; i++)
        if (board[i + y * n] == v || board[x + i * n] == v)
            return false;
    int bx = x / m * m, by = y / m * m;
    for (int j = by; j < by + m; j++)
        for (int i = bx; i < bx + m; i++)
            if (board[i + j * n] == v)
                return false;
    return true;
}

int ans_count;

// try to fill board[k] .. board[n * n - 1] and show answers.
// assert: k >= 0 and board[0] .. board[k - 1] are already filled.
// n : board size, m : box size (= sqrt(n))
bool solve(int n, int m, int board[], int k) {
    while (k < n * n) {
        if (board[k] == 0) {
            for (int v = 1; v <= n; v++)
                if (check(n, m, board, k, v)) {
                    board[k] = v;
                    if (solve(n, m, board, k + 1))
                        return true;
                    board[k] = 0;
                }
            return false;
        }
        k++;
    }
    printf("*** answer #%d\n", ++ans_count);
    showBoard(n, board);
    return !show_all_answers;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "usage: %s problem_file\n", argv[0]);
        exit(EXIT_FAILURE);
    }
    char *problem_file = argv[1];

    int n = board_size;
    int board[n * n];
    
    FILE *fp = fopen(problem_file, "r");
    if (fp == NULL) {
        perror(problem_file);
        exit(EXIT_FAILURE);
    }
    if (readBoard(fp, n, board) < n * n) {
        fprintf(stderr, "%s: too few data\n", problem_file);
        fclose(fp);
        exit(EXIT_FAILURE);
    }
    fclose(fp);

    printf("*** problem %s\n", problem_file);
    showBoard(n, board);

    ans_count = 0;
    solve(n, sqrt(n), board, 0);
    if (ans_count == 0)
        printf("*** no answer\n");

    return EXIT_SUCCESS;
}
