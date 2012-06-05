/**
 * Main flight-managing class.
 *
 * @author Liran Oz, Ory Band
 * @version 1.0
 */
public class FlightManager{
    /**
     * Main hash function.
     *
     * @param id ID to hash.
     * @param n Amount of IDs.
     *
     * @return Hash result of ID given as argument.
     */
    private int hash1(int id, int n){
        return id % (n/3);
    }

    /**
     * Secondary (skip) hash function.
     *
     * @param id ID to hash.
     * @param n Amount of IDs.
     *
     * @return Hash result of ID given as argument.
     */
    private int hash2(int id, int n){
        return id % (n);
    }

    /**
     * Third (2nd skip) hash function.
     *
     * @param id ID to hash.
     * @param n Amount of IDs.
     *
     * @return Hash result of ID given as argument.
     */
    private int hash3(int id, int n) {
        return hash2(reverse(id), n);
    }

    /**
     * @param n Integer to reverse.
     *
     * @return Reversed integer given as argument.
     */
    private int reverse(int n) {
        int res = 0,
            temp;

        // Shift all digits.
        while (n!=0) {
            res  *= 10;      // Shift all digits left.
            temp  = n % 10;  // Fetch left digit.
            res  += temp;    // Insert it at the beginning.
            n    /= 10;      // Removed (not shifted) leftmost digits.
        }

        return res;
    }

    private int[] getIds(String path) {
        String s = Files.read("input1.dat");
        String[] ss = s.split(",");

        int l = ids.length;
        int[] ids = new int[l];

        // Convert string IDs to int.
        for(int i=0; i<l; i++) {
            ss[i] = s[i].trim();
            ids[i] = Integer.parseInt(ss[i]);
        }
    }

    private AvlTree[] buildAVLs(int[] ids) {
        int l = ids.length;
        AvlTree[] t = new AvlTree[l/3];

        // Build tree.
        int i;
        for (i=0 ; i<l; i++) { 
            t[ hash1(ids[i], l) ].insert(ids[i]);    
        }

        // Print row 1: Trees' height and size.
        for(i=0; i < l/3 -1; i++) {
            System.out.print(t[i].height + " " + t[i].size + " ");
        }

        System.out.println(t[l/3 -1].height + " " + t[l/3 -1].size);
    }

    public static void stage1(String path) {
        int ids[] = getIds(path);
        AvlTree[] t = buildAVLs(ids);
    }

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
