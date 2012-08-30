# 数独の解を求めるプログラム
渡部卓雄 (2012/7/25)

## 概要
再帰による単純なバックトラックにより数独の解を計算する．

## コンパイル方法
以下のようにmakeコマンドでコンパイルできる．

    make

あるいは以下のようにコンパイルする．
C99の構文を用いているので，gccの場合 -std=c99 が必要.

    gcc -std=c99 sudoku.c -o sudoku

## 実行方法

    ./sudoku 問題ファイル

## 問題ファイル
問題を構成する N * N 個の数を（列方向を優先して）並べたもの．
数が入っていない場所は0で表す．

いくつかの例題がディレクトリexに入っている．

## プログラムの説明

### 変数

* `board_size`:
  盤の大きさ（一辺の長さ，正整数の2乗でなければならない）

* `show_all_answers`: 真の場合は全ての解を表示して終了する．
  偽の場合は最初の解を表示した時点で終了する．

### 主な関数

* `bool solve(int n, int m, int board[], int k);`

    `board[0]`〜`board[k - 1]`が既に埋まっている状態で呼び出され，
    残りの`board[k]`〜`board[n * n - 1]`を埋めて解を表示する．

    nは盤の大きさ，mは盤面内の箱の大きさ（nの正の平方根）である．
     
* `bool check(int n, int m, int board[], int k, int v);`

    `board[k]`に数vを置くことができるか否かを検査する．

### アルゴリズム

#### solve

    while (k < n * n) {
        if (board[k] == 0) {
            // board[k]に数1〜nを順に入れて試す．
            for (int v = 1; v <= n; v++)
                if (check(n, m, board, k, v)) {
                    // 数vはboard[k]に入れることができる．
                    board[k] = v;
                    // 残りの盤面を埋める．
                    if (solve(n, m, board, k + 1))
                        return true;
                    // 失敗したら（あるいは他の解も必要なら）
                    // board[k]に入れた数を取り去って次の数を調べる．
                    board[k] = 0;
                }
            // board[k]にはどの数も入れることができない．
            return false;
        }
        // board[k]が既に埋まっていたら次に進む．
        k++;
    }
    // 盤面全てが埋まったので解を出力する．

#### check

    // board[k]のxy座標
    int x = k % n, y = k / n;
    // board[k]を含む列および行の検査
    for (int i = 0; i < n; i++)
        if (board[i + y * n] == v || board[x + i * n] == v)
            return false;
    // board[k]を含む箱の検査
    // (bx, by)は箱の左上隅
    int bx = x / m * m, by = y / m * m;
    for (int j = by; j < by + m; j++)
        for (int i = bx; i < bx + m; i++)
            if (board[i + j * n] == v)
                return false;
    return true;
