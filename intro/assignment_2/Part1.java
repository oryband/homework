public class Part1 {
	
	/* ********************************** *
	 * *  Part 1 - increment            * *
	 * ********************************** */
	// Task 1.1
	public static boolean increment(int[] vec, int base) {
        for (int i=0; i<vec.length; i++) {
            if (base - vec[i] != 1) {
                for (int j=i; j<vec.length; j++) {
                    if (vec[j] == base -1) {
                        vec[j] = 0;
                    } else {
                        vec[j]++;
                    }
                }

                return true;
            }
        }

        nullify(vec);
		return false;
	}		

    // Assigns 0's to all elements of a 1-dimensional array.
    public static void nullify(int[] arr) {
        int i;
        for (i=0; i<arr.length; i++) {
            arr[i] = 0;
        }
    }

	// Task 1.2
	public static boolean increment(int[][] matrix, int base) {
        int i;
        for (i=0; i<matrix.length; i++) {
            for (int j=0; j<matrix[i].length; j++) {
                if (increment(matrix[i], base)) {
                    return true;
            }
        }

        for (i=0; i<matrix.length; i++) {
            nullify(matrix[i]);
        }

        return false;
	}
	
	/* ********************************** *
	 * *  Part 2 - validate solution    * *
	 * ********************************** */
	// Task 2.1
	public static int[] findSameColorRec(int[][] board, int topRow, int leftColumn) {
		int[] res=null;
		// YOUR CODE HERE
		return res;
	}
	
	// Task 2.2
	public static int[] findSameColorRec(int[][] board) {
		int[] res=null;
		// YOUR CODE HERE
		return res;
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
    public static void main(String[] args) {}
        /*int n=4, m=4, c=3;
        long startTime=System.currentTimeMillis();
        int[][] sol=solver(n, m, c);
        //int[][] sol=randomSolver(n, m, c, n*m, n*m);
        long endTime=System.currentTimeMillis();
        System.out.println("Solution time : "+(endTime-startTime)+" ms");
        System.out.println("Solution found: "+(sol!=null));
        if(sol!=null) {
            System.out.println("Valid solution: "+isValidSolution(sol,c));
        }
	}*/
}
