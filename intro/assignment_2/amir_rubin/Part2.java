public class Part2 {

	/* ********************************** *
	 * *  Part 5 - SAT Based solver     * *
	 * ********************************** */

	// provided
	public static int[][][] map(int n, int m, int c) {
		int[][][] boardMap = new int[n][m][c];
		for(int y=0; y<n; y=y+1) {
			for(int x=0; x<m; x=x+1) {
				for(int clr=0; clr<c; clr=clr+1) {
					boardMap[y][x][clr] = clr + x*c + y*c*m + 1;
				}
			}
		}
		return boardMap;
	}
	
	// Task 5.1  gives true if atleast one is true
		public static int[][] atLeastOne(int[] lits) {
		int[][] clauses=new int[1][lits.length];//sets the size of the new array
		for(int a=0 ; a<lits.length ; a=a+1){//copies the original array to a cnf
			clauses[0][a]=lits[a];
			}
		
		return clauses;
	}

	// Task 5.2
public static int[][] atMostOne(int[] lits) {
	
		int size=((lits.length)*(lits.length-1))/2;  //finds the amount of couples needed
		
		int[][] clauses= new int [size][2];  //sets the number of couples
		
		int c=0;  //c will be the place of the first number taken from lits
		int d=c+1;  //d will be the place of the second number taken from lits
		
		int a=0; //marks the number of the first dimension in clauses array
		
				while (c<lits.length-1){//buils clauses, stop when reaches lits-1
						
					while (d<lits.length){  //sets couples in to causes. 
					clauses[a][0]=-lits[c];
					clauses[a][1]=-lits[d];
								
					d=d+1;  //takes the next number from lits for the second place
					a=a+1;  //put it in the next couple
					}
					
				c=c+1;  //takes the next number from lits for the first number
				d=c+1;  //statrs setting the second number from c+1.
				}
		return clauses;
	}

	// Task 5.4
	public static int[][] notSameColor(int[] x1, int[] x2, int[] x3, int[] x4) {
		int[][] clauses=null;
		clauses= new int[x1.length][4];//sets the length of the new array.
		
				
				int b=0; //marks which one to take from the arry
				
				for (int c=0; c<clauses.length; c=c+1 , b=b+1){ // this loop will set values in the return aray
						
							clauses[c][0]=-x1[b];
							clauses[c][1]=-x2[b];
							clauses[c][2]=-x3[b];
							clauses[c][3]=-x4[b];
					
						}
		
		return clauses;
	}

	
	
	
	// Task 5.3

public static int[][] decode(int[][][] boardMap, boolean[] satSol) {
		int[][] res=null;
		if (satSol!=null){ //checks that its a problem i can solve
			
			int q = boardMap.length;
			int w = boardMap[0].length;
			
			res= new int[q][w];//sets the size of the answer
			int c= boardMap[0][0].length;//sets the number of colors i can use
			
			for(int a = 0 ; a<boardMap.length; a=a+1){ // this will set the color for every brick
					
					for (int b =0; b<boardMap[0].length; b=b+1){
					
					int place=a*boardMap[0].length*c+b*c+1;//this is the place in satSol for the first value of the brick in a,b
					
					res[a][b]=color(satSol,boardMap[a][b] , place, c);
			
			
					} //for line 78
			} //for line76			
		} //if
	
		return res;
	}
	
	//this is an "helper" function, which will find the colour..
	public static int color(boolean[]satSol, int[]ta , int  place , int c){ //this function will tell me the colour in the brick
	int color=-1;
	int r=0;
		for(int a=place; a<place+c & color==-1;  r=r+1 , a=a+1){//looks for the true color to use
			if ( (ta[r]>0 & satSol[a]==true) | (ta[r]<0 & satSol[a]==false)){//if its a negative number it will look for false
				color=a-place;
			}
		}
		return color;
		}
		
		
		
		
		
		
	// Task 5.5
		public static int[][] satBasedSolver(int n, int m, int c) {
		// create map
		int[][][] boardMap = map(n,m,c);
		// initialize SAT solver
		SATSolver.init(n*m*c);
		
		
		for(int e=0 ; e<boardMap.length ;e=e+1){//starts checking all the conditions, stops when finishd the rows
			
			for(int a=0 ; a<boardMap[0].length ; a=a+1){//checks by coloums
				
				SATSolver.addClauses(atLeastOne(boardMap[e][a]));
				SATSolver.addClauses(atMostOne(boardMap[e][a]));//these two will confirm only only one colour in every place
				
				if(a<(boardMap[0].length-1) && e<(boardMap.length-1)){//checks if making a squere is possible
				
				for(int b=a+1 ; b<boardMap[0].length ;b=b+1){//will mention the second coloum to take from
				
					for(int d=e+1 ; d<boardMap.length ;d=d+1){//will mention the second row to take from
					int[] x1=boardMap[e][a];
					int[] x2=boardMap[e][b];
					int[] x3=boardMap[d][a];
					int[] x4=boardMap[d][b];//these are the four i want to check
					
					SATSolver.addClauses(notSameColor( x1,  x2,  x3, x4));
					}//for l 145
				}//for l 143	
				}//if l 141	
			}//for l 136
		}//for l 134
		
			
		
				
		boolean[] satSol=SATSolver.getSolution();
		
		int[][] ans=null;
		
		if(satSol.length==0){
		System.out.println("cant!");
		}
		else{
		ans=decode(boardMap,satSol);
		}
		
		// decode solution
		return ans;
	}
	
	
	/* ********************************** *
	 * *  Main you may want to use      * *
	 * ********************************** */
/*
	public static void main(String[] args) {
		int n=4, m=4, c=3;
		long startTime=System.currentTimeMillis();
		int[][] sol=satBasedSolver(n, m, c);
		long endTime=System.currentTimeMillis();
		System.out.println("Solution time : "+(endTime-startTime)+" ms");
		System.out.println("Solution found: "+(sol!=null));
		if(sol!=null) {
			System.out.println("Valid solution: "+Part1.isValidSolution(sol,c));
		}
	}
*/
}
