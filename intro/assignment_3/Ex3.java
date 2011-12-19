/***************************************************
 * intro121/ipis121: Third assignment              *
 *                                                 *
 * This class is for assignment #3 - Part 3        *
 *                                                 *
 * Author(s): ### YOUR NAME(S) AND EMAIL(S). ##### *
 * Date: ##/##/####                                *
 *                                                 *
 ***************************************************/

/*
 * Important! Add comments to every method!
 *
 * The "return" statement at the end of each method and the initial value 
 * is just so this skeleton file compiles, change it according to your needs
 */

public class Ex3 {

	public static int north(int[] tile){
		return tile[0];
	}

	public static int east(int[] tile){
		return tile[1];
	}

	public static int south(int[] tile){
		return tile[2];
	}

	public static int west(int[] tile){
		return tile[3];
	}


	/******************** Task 1 ********************/
	public static boolean canPut(int[]tile, int x, int y, int[][][] board) {
        int n = board.length;  // Board size (NxN - square).

        // Check vacancy in given coordinates.
        if (x<0 || y<0 || x>n || y>n || board[x][y] != null) {
            return false;
        }

        // Index difference (delta) of neighbouring tiles to test against, by order: North, East, South, West.
        int[][] neighbours = { {0,-1}, {1,0}, {0,1}, {-1,0} };

        int tx,ty;  // Temporary coordinates of current neighbour.
        for (int i=0; i<neighbours.length; i++) {
            tx = x + neighbours[i][0];
            ty = y + neighbours[i][1];

            // Test to if given tile is on the edges of the board:
            // If current neighbouring tile is outside, we can conclude this.
            if (ty == -1 || tx == n || ty == n || tx == -1) {
                if (tile[i] != 0) {
                    return false;
                }
            // Test if neighbour's side is the same color as the
            // opposite side's color of our given tile.
            } else if (board[tx][ty] != null  &&  board[tx][ty][(i+2) % 4] != tile[i]) {
                return false;
            }
        }

        // If all tests passed successfuly, we can put the given tile in the board.
        return true;
	}


	/******************** Task 2 ********************/
	public static int[][][] put(int[] tile, int x, int y, int[][][] board) {
		int n = board.length;  // Board size in NxN (square).

		int[][][] copy = new int[n][n][];  // New board - Copy by value of the original board + new assigned tile.

        // Iterate over board's tiles and copy them tiles by value (NOT by reference).
        int a,b,c;  // x,y & tile indexes.
        for (a=0; a<n; a++) {
            for (b=0; b<n; b++) {
                // If the current tile is occupied, copy it by value.
                if (board[a][b] != null) {
                    copy[a][b] = new int[4];
                    for (c=0; c<4; c++) {
                        copy[a][b][c] = board[a][b][c];
                    }
                }
            }
        }

        // Assign the tile given as argument to the new board.
        copy[x][y] = new int[4];
        for (c=0; c<4; c++) {
            copy[x][y][c] = tile[c];
        }

        return copy;
	}


	/******************** Task 3 ********************/
	public static int[][] delete(int i, int[][] tiles) {
        int[][] filtered = new int[tiles.length -1][];
        int len = tiles.length;

        int j,c;  // Tile & color index.
        // Build new filtered list UNTIL the given index.
        for (j=0; j<i; j++) {
            filtered[j] = new int[4];
            for (c=0; c<4; c++) {
                filtered[j][c] = tiles[j][c];
            }
        }

        // Build the rest of the list from AFTER the given index until the end.
        for (j=i; j<len -1; j++) {
            filtered[j] = new int[4];
            for (c=0; c<4; c++) {
                filtered[j][c] = tiles[j+1][c];
            }
        }

        return filtered;
	}

	public static int[] rotate(int j, int[] tile){
		int[] t = new int[4];

        // Build new tile, and assign colors to each index by difference of rotation (j).
        for (int i=0; i<4; i++) {
            t[(i+j) %4] = tile[i];
        }

        return t;
	}


	/******************** Task 4 ********************/
	public static int[][][] solve(int[][] tiles){
		int size = (int) Math.sqrt(tiles.length);
		int[][][] board = new int[size][size][];
		return solve(board,tiles);
	}

	public static int[][][] solve(int[][][] board, int[][] tiles){
		int[][][] solution = null;
		// YOUR CODE HERE
		return solution;
	}

	/******************** Auxiliary functions ********************/
	
	/**
	 * Compare two boards and return true iff they are equal.
	 * @param board1
	 * @param board2
	 * @return true iff the boards are equal
	 */
	public static boolean equalBoards(int[][][] board1, int[][][] board2) {
		boolean ans = true;
		for (int i = 0; i < board1.length && ans; i++) {
			for (int j = 0; j < board1.length && ans; j++) {
				int[] tile1 = board1[i][j];
				int[] tile2 = board2[i][j];
				if ((tile1 == null && tile2 != null)
						|| (tile1 != null && tile2 == null))
					ans = false;
				else if (tile1 != null && tile2 != null) {
					for (int k = 0; k < 4 && ans; k++)
						if (tile1[k] != tile2[k])
							ans = false;
				}
			}
		}
		return ans;
	}


	public static void main(String[] args) {
		int[][][] board = { { { 0, 2, 1, 0 }, null, { 1, 3, 0, 0 } },
				{ { 0, 2, 4, 2 }, null, { 4, 4, 0, 3 } },
				{ { 0, 0, 4, 2 }, { 4, 0, 3, 3 }, { 3, 0, 0, 4 } } };

		// Test task 1
		int[] test1tile = {1, 2, 1, 0};
		System.out.println("Test 1: expected=true actual="
				+ canPut(test1tile, 0, 1, board));

		int[] test2tile = {2, 2, 1, 0};
		System.out.println("Test 2: expected=false actual="
				+ canPut(test2tile, 0, 1, board));

		int[] test3tile = {1, 2, 1, 1};
		System.out.println("Test 3: expected=false actual="
				+ canPut(test3tile, 0, 1, board));

		// Test task 2
		int[] test4tile = {1, 2, 1, 0};
		int[][][] test4exp = { { { 0, 2, 1, 0 }, { 1, 2, 1, 0 }, { 1, 3, 0, 0 } },
				{ { 0, 2, 4, 2 }, null, { 4, 4, 0, 3 } },
				{ { 0, 0, 4, 2 }, { 4, 0, 3, 3 }, { 3, 0, 0, 4 } } };
		System.out.println("Test 4: "
				+ (equalBoards(test4exp, put(test4tile, 0, 1, board)) ? "passed :)"
						: "failed!"));
		
		// Test task 3
		int[] test5tile= {1, 2, 3, 4};
		int[] test5exp = {4, 1, 2, 3};
		System.out.println("Test 5: expected=" + Ex2.arrayToString(test5exp)  + 
				" actual=" + Ex2.arrayToString(rotate(1, test5tile)));
		
		int[] test6tile= {1, 2, 3, 4};
		int[] test6exp = {3, 4, 1, 2};
		System.out.println("Test 6: expected=" + Ex2.arrayToString(test6exp)  + 
				" actual=" + Ex2.arrayToString(rotate(2, test6tile)));
		
		int[][] test7tiles = {{1, 2, 3, 4}, {0, 2, 4, 5}, {5, 2, 5, 1}};
		int[][] test7exp = {{1, 2, 3, 4}, {5, 2, 5, 1}};
		System.out.println("Test 7: expected=" + Ex2.matrixToString(test7exp)  + 
				" actual=" + Ex2.matrixToString(delete(1, test7tiles)));

        int[][] tiles1 = {{3,1,0,2},{3,2,0,1},{2,0,0,2},{0,2,1,0},{0,0,1,1},{1,3,1,0},
                          {4,4,3,3},{2,0,2,3},{3,3,3,4},{1,2,0,0},{1,4,1,0},{0,2,4,2},
                          {0,1,4,2},{4,3,4,4},{4,4,3,3},{1,0,2,4}};

		int[][] tiles2 = {{3,1,0,2},{0,1,3,2},{0,0,2,2},{0,2,1,0},{0,0,1,1},{1,3,1,0},
                          {4,4,3,3},{2,0,2,3},{3,3,3,4},{0,0,1,2},{1,4,1,0},{0,2,4,2},
                          {0,1,4,2},{4,3,4,4},{4,4,3,3},{1,0,2,4}};

		int[][] tiles3 = {{2,0,2,3},{3,3,3,4},{0,0,1,2},{1,4,1,0}};

        int[][][] solution = solve(tiles1);
        EternityPrint.showBoard(solution); // showing a game board
	}
}
