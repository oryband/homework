public class Part1 {
	
	/* ********************************** *
	 * *  Part 1 - increment            * *
	 * ********************************** */
	// Task 1.1
	public static boolean increment(int[] vec, int base) {
        // Iterate over all digits (from LSB to MSB) and nullify them if they reached the cap.
        // Otherwise increase just the current digit, no need to increase the rest towards the MSB.
        for (int i=vec.length -1; i>=0; i--) {
            if (vec[i] == base -1) {
                vec[i] = 0;
            } else {
                vec[i]++;
                return true;
            }
        }

        // If all digits were nullified, we reached the cap.
        return false;
	}

	// Task 1.2
	public static boolean increment(int[][] matrix, int base) {
        // Iterate over all indices and increment them.
        for (int i=matrix.length -1; i>=0; i--) {
            if ( increment(matrix[i], base) ) {
                return true;  // Return true if current index was incremented.
            }
        }

        // If all indices were nullified, we reached our cap.
        return false;
	}
	
	/* ********************************** *
	 * *  Part 2 - validate solution    * *
	 * ********************************** */
	// Task 2.1
	public static int[] findSameColorRec(int[][] board, int topRow, int leftColumn) {
        int rows = board.length;  // Set row & column length
        int columns = board[0].length;

        int color = board[topRow][leftColumn];  // Set rectangle corner color.

        // Iterate over all rows and columns, starting from {TopRow, LeftColumn}, and test all corners of the current rectangle.
        for (int i=topRow +1; i<rows; i++) {
            for (int j=leftColumn +1; j<columns; j++) {
                if (board[topRow][j] == color && board[i][leftColumn] == color && board[i][j] == color) {
                    int[] coords = {i,j};
                    return coords;
                }
            }
        }

        return null;
	}
	
	// Task 2.2
	public static int[] findSameColorRec(int[][] board) {
        int rows = board.length;
        int columns = board[0].length;

        int[] line = null;

        for (int i=0; i<rows; i++) {
            for (int j=0; j<columns; j++) {
                line = findSameColorRec(board, i, j);

                if (line != null) {
                    int[] coords = {i, j, line[0], line[1]};
                    return coords;
                }
            }
        }

        return null;
	}

	// Task 2.3
	public static boolean isValidSolution(int[][] board, int c) {
		boolean res=false;
		// YOUR CODE HERE
		return res;
	}
	
	/* ********************************** *
	 * *  Part 3 - Basic solver         * *
	 * ********************************** */
	public static int[][] solver(int n, int m, int c) {
		int[][] board=null;
		// YOUR CODE HERE
		return board;
	}

	/* ********************************** *
	 * *  Part 4 - Random solver        * *
	 * ********************************** */
	// Task 4.1
	public static int[][] randomBoard(int n, int m, int c) {
		int[][] board=null;
		// YOUR CODE HERE
		return board;
	}
	
	// Task 4.2
	public static void randomFix(int[][] board, int c, int[] twoCorners) {
		// YOUR CODE HERE
	}
	
	// Task 4.3
	public static int[][] randomSolver(int n, int m, int c, int numFixes) {
		int[][] board=null;
		// YOUR CODE HERE
		return board;
	}
	
	// Task 4.3
	public static int[][] randomSolver(int n, int m, int c, int numResets, int numFixes) {
		int[][] board=null;
		// YOUR CODE HERE
		return board;
	}

	
	/* ********************************** *
	 * *  Main you may want to use      * *
	 * ********************************** */
    public static void main(String[] args) {
        /*int n=4, m=4, c=3;
        long startTime=System.currentTimeMillis();
        int[][] sol=solver(n, m, c);
        //int[][] sol=randomSolver(n, m, c, n*m, n*m);
        long endTime=System.currentTimeMillis();
        System.out.println("Solution time : "+(endTime-startTime)+" ms");
        System.out.println("Solution found: "+(sol!=null));
        if(sol!=null) {
            System.out.println("Valid solution: "+isValidSolution(sol,c));
        }*/
	}
}
