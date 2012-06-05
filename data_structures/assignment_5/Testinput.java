import java.util.* ;
import java.io.* ;
public class Testinput{
	   
	   public static String read_From_File_Example(String inputFilename){
		    String idsString = "" ;
	        try {
	            File inFile = new File(inputFilename);
	            FileReader ifr = new FileReader(inFile);
	            BufferedReader ibr= new BufferedReader(ifr) ;

	            String tempLine = "" ;
	           
	            while (tempLine != null )
	            {
	            	tempLine = ibr.readLine() ;
	                if (tempLine != null)
	                {
	                	idsString += tempLine;
	                }
	            }
	            
	            ibr.close();
	            ifr.close();
	            
	        }
	        catch(Exception e) {
	            System.out.println( "Error \""+ e.toString()+ "\" on file "+inputFilename);
	            e.printStackTrace() ;
	            System.exit(-1) ;      //brutally exit the program
	        }
	        return idsString;

	    }
	   private int partition(int arr[], int left, int right)
	   {
	         int i = left, j = right;
	         int tmp;
	         int pivot = arr[(left + right) / 2];
	        
	         while (i <= j) {
	               while (arr[i] < pivot)
	                     i++;
	               while (arr[j] > pivot)
	                     j--;
	               if (i <= j) {
	                     tmp = arr[i];
	                     arr[i] = arr[j];
	                     arr[j] = tmp;
	                     i++;
	                     j--;
	               }
	         };
	        
	         return i;
	   }
	    
	   private void quickSort(int arr[], int left, int right) {
	         int index = partition(arr, left, right);
	         if (left < index - 1)
	               quickSort(arr, left, index - 1);
	         if (index < right)
	               quickSort(arr, index, right);
	   }

	    private static void write_To_File_Example(String outputFilename,String str) {
   
	        try {
	            File outFile = new File(outputFilename);
	            FileWriter ofw = new FileWriter(outFile, true);

	            
	          	ofw.append(str+"\r\n");

	            ofw.close();
	        }
	        catch (Exception e) {
	            System.out.println( "Error \""+ e.toString()+ "\" on file "+outputFilename);
	            e.printStackTrace() ;
	            System.exit(-1) ;      //brutally exit the program
	        }
	    }    
	    private int hs1 (int id,int n){
	    	return id % (n/3);
	    }
	    private int hs2 (int id,int n){
	    	return id % (n);
	    }
	    private int reverse(int n){ // reverse the int!
	    	int ans=0;
	    	int temp;
	    	while (n!=0){
	    		ans=ans*10;
	    		temp=n%10;
	    		ans=ans+temp;
	    		n=n/10;
	    	}
	    	return ans;
	    }
	    public static void main(String[] args){
   	
 	    String str = read_From_File_Example("input1.dat");//read the file to a string
 	    String[] ids = str.split(",");//now you have an array of ids as strings (but they contain spaces
 	    int N = ids.length;//this is N - the amount of ids
 	    int[] idsint = new int[N];
 	    for(int i = 0; i <N ;i++)
	    {
	    	ids[i] = ids[i].trim();//remove spaces
	    	idsint[i]=Integer.parseInt(ids[i]);
	    }
 	    AvlTree[] reg = new AvlTree[N/3]; // creating the array with the reg people.
 	    for (int i=0;i<N;i=i+1) // inserting
			reg[hs1(idsint[i],N)].insert(idsint[i]);	
 	    for(int j=0;j<N/3-1;j=j+1) // printing row number 1 using AvlTree.size & AvlTree height.
 	    	System.out.print(reg[j].height+" "+reg[j].size+" "); // Row number 1
 	    System.out.print(reg[N/3-1].height+" "+reg[N/3-1].size);
 	    System.out.println(); // Row number 1
 	   // lvl 2
 	    String str2 = read_From_File_Example("input2.dat");//read the file to a string
 	    String[] ids2 = str.split(",");//now you have an array of ids as strings (but they contain spaces
 	    int N2 = ids2.length;//this is N - the amount of ids
 	    int[] idsint2 = new int[N2]; 
 	    for(int i = 0; i <N2 ;i++)
	    {
	    	ids2[i] = ids2[i].trim();//remove spaces
	    	idsint2[i]=Integer.parseInt(ids2[i]);
	    }
 	    int[] checkin= new int[N]; // the people on the flight
 	    int[] spare= new int[N2];  // the waiting list.
 	    int countercheckin=0;
 	    int counterspare=0;
 	    int steps=0;
 	    for (i=0;i<N2;i=i+1){ // running on the array of the new people
 	    	steps=steps+2+reg[hs1(idsint2[i],N)].steps(idsint2[i]); // sum the steps for each person - using AvlTree.steps.
 	    	if (reg[hs1(idsint2[i],N)].find(idsint2[i])!=null) { // check if each person is in the flight.
 	    		checkin[countercheckin]=idsint2[i]; // if yes insert him to the flight
 	    		countercheckin=countercheckin+1; // add 1 to the counter,
 	    	}
 	    	else {
 	    		spare[counterspare]=idsint2[i]; // else add him to the spare array.
 	    		counterspare=counterspare+1; // add 1 to the counter.
 	    	}
 	    }
 	    System.out.println(steps/N2); // row 2 - avg steps.
 	    for (i=counterchecking;i<N;i=i+1){ // inserting other people to the plane.
 	    	checkin[i]=spare[i-countercheckin];
 	    }
 	    int[]checkinsort=new int[N]; // copying checking to sort it 
 	    for (i=0;i<N;i=i+1)
 	    	checkinsort[i]=checkin[i];
 	   quickSort(checkinsort); // sorting
 	   for (i=0;i<N-1;i=i+1) // printing the people of this flight
 		   System.out.print(checkinsort[i]+" "); //row 3
 	   System.out.print(checkinsort[N-1]); //row 3
 	   System.out.println(); 
 	   lvl 3
 	   
 	   
 	   
 	   
 	   
 	   
 	   
 	    
 	    write_To_File_Example("output1.dat","number of ids = " + N);//write N to the output file
 	    for(int i = 0; i <N ;i++)
	    {
 	    	write_To_File_Example("output1.dat","id number " + i + " : " + ids[i]);//write ids[i] to the output file
	    }
   }  
}