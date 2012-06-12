/**
 * Main flight-managing class.
 *
 * @author Liran Oz, Ory Band
 * @version 1.0
 */
public class FlightManager {
    /**
     * Main hash function.
     *
     * @param id ID to hash.
     * @param n Amount of IDs.
     *
     * @return Hash result of ID given as argument.
     */
    private int treeHash(int id, int n){
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
    private int seatHash1(int id, int n){
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
    private int seatHash2(int id, int n) {
        return seatHash1(reverse(id), n);
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
        while (n != 0) {
            res  *= 10;      // Shift all digits left and add a zero at the end (first digit).
            temp  = n % 10;  // Fetch left digit.
            res  += temp;    // Insert it at the beginning (where the zero was at).
            n    /= 10;      // Remove (non-shifted) leftmost digits.
        }

        return res;
    }

    /**
     * @param path ID string read from file.
     *
     * @return ID list.
     */
    private int[] getIds(String path) {
        String s = Files.read(path);
        String[] ss = s.split(",");

        int l = ss.length;
        int[] ids = new int[l];

        // Convert string IDs to int.
        for(int i=0; i<l; i++) {
            ss[i] = ss[i].trim();
            ids[i] = Integer.parseInt(ss[i]);
        }

        return ids;
    }

    /**
     * @param ids ID list.
     *
     * @return Initialized AVL array.
     */
    private AvlTree[] buildAVLs(int[] ids) {
        int l = ids.length,
            al = l/3;

        // Init trees.
        AvlTree[] trees = new AvlTree[al];
        int i;
        for (i=0; i<al; i++) {
            trees[i] = new AvlTree();
        }

        // Build trees;
        int id, slot;
        for (i=0 ; i<l; i++) { 
            id = ids[i];
            slot = treeHash(id, l);
            trees[slot].insert(id);
        }

        // Print row 1 - Trees' height and size.
        for(i=0; i < al -1; i++) {
            System.out.print(trees[i].height() + " " + trees[i].size() + " ");
        }

        System.out.println(trees[al -1].height() + " " + trees[al -1].size());

        return trees;
    }


    /**
     * Decides who's boarding the flight, taking registerations and availble seats in mind.
     *
     * @param trees Customer AVL trees array.
     * @param registered Registered customers.
     * @param arrivals Arrived customers - not necesseriarely registered.
     *
     * @return Boarding customer list.
     */
    private int[] processArrivals(AvlTree[] trees, int[] registered, int[] arrivals) {
        int rl = registered.length,
            al = arrivals.length;

        // Build boardees and standby lists.
        int[] boardees = new int[rl],
              standby  = new int[al];

        int c_boardees = 0,  // counters.
            c_standby  = 0,
            totalSteps = 0,  // Statistics calcs.

            slot,            // Tree slot (index).
            customer,        // ID.
            steps,           // Total steps taken for each search.
            i;

        boolean found;       // Indicates whether a node was found in tree.

        int[] result;
        for (i=0; i < al; i++) {  // Load registered arrivals.
            customer = arrivals[i];
            slot = treeHash(customer, rl);

            result = trees[slot].steps(customer);
            found = result[0] != 0;
            steps = result[1];

            totalSteps += 1 + steps;  // Sum the steps required to find each arrival. +1 for hashing.

            if (found) {  // Load passenger if registered.
                boardees[c_boardees++] = customer;
            } else {  // Else put him on standby.
                standby[c_standby++] = customer;
            }
        }

        for (i=c_boardees; i<rl; i++) {  // Fill available standby seats.
            boardees[i] = standby[i - c_boardees];
        }

        System.out.println(totalSteps / al);  // Print row 2 - Average search-steps per arrival.
        printSortedBoardees(boardees);   // Row 3.

        return boardees;
    }


    /**
     * Prints boardees list.
     *
     * @param boardees Boardees list.
     */
    private void printSortedBoardees(int[] boardees) { 
        int[] sorted = new int[boardees.length];

        int   l = boardees.length;

        // Deep-copy, so we won't affect the original boardees list.
        int i;
        for (i=0; i<l; i++) {
            sorted[i] = boardees[i];
        }

        Files.quickSort(sorted);

        for (i=0; i<l-1; i++) {
            System.out.print(sorted[i] + " ");  // Row 3
        }

        System.out.println(sorted[l-1]);
    }


    /**
     * Seats boardees.
     *
     * @param boardees People boarding the flight.
     *
     * @return Step list, stating how many steps were needed to seat each boardee.
     */
    public int[] seatBoardees(int[] boardees, boolean first) {
        int l = boardees.length,
            customer,
            slot,        // Hash results.
            supplement,  // Seat supplement.
            slotUp,
            slotDown,
            i;

        boolean seated,  // Switch, stating if customer was seated.
                pos,     // Search seat in monotonic increasing order ONLY.
                neg;     // Monotonic decreasing.

        int[] seats = new int[l],
              steps = new int[l];  // Statistics calcs.

        // Init seats.
        for(i=0; i<l; i++) {
            seats[i] = -1;
            steps[i] = 1;
        }

        for (i=0; i<l; i++) {
            customer = boardees[i];

            if (first) {
                slot = seatHash1(customer, l);
            } else {
                slot = seatHash2(customer, l);
            }

            seated     = false;
            supplement = 0;
            pos        = false;  // For monotonic checking - only slot + supplement.
            neg        = false;  // Same, but for slot - supplement.

            // Seat customer.
            while ( ! seated ) {
                if (supplement == 0 && seats[slot] == -1) {  // First try.
                    seats[slot] = customer;
                    seated      = true;
                } else {
                    supplement++;
                    steps[i] += 2;

                    slotUp   = slot + supplement;
                    slotDown = slot - supplement;

                    // Test if we're out of bounds.
                    if ( ! pos && slotDown < 0) {
                        pos = true;
                    }
                    if ( ! neg && slotUp >= l) {
                        neg = true;
                    }

                    // Check available seats according, if necessary in monotonic order.
                    if ( ! neg && seats[slotUp] == -1 ) {  // TODO Fix steps counting.
                        seats[slotUp] = customer;
                        steps[i]--;
                        seated = true;
                    } else if ( ! pos && seats[slotDown] == -1 ) {
                        seats[slotDown] = customer;
                        seated = true;
                    }
                }
            }
        }

        System.out.println();
        for (int j=0; j<seats.length; j++) {
            System.out.println(seats[j]);
        }
        System.out.println();
        return steps;
    }


    /**
     * @param steps Step list stating how many steps were needed to seat each boardee.
     */
    public void printSeatSteps(int[] steps) {
        int l = steps.length,
            t, c, i;

        c = 0;
        t = l/2;
        for (i=0; i<t; i++) {
            c += steps[i];
        }
        System.out.print( (c/t) + " ");

        c = 0;
        t = 3*l/4;
        for (i=0; i<t; i++) {
            c += steps[i];
        }
        System.out.print( (c/t) + " ");

        c = 0;
        t = l - (int) Math.sqrt(l);
        for (i=0; i<t; i++) {
            c += steps[i];
        }
        System.out.print( (c/t) + " ");

        c = 0;
        t = (int) Math.sqrt(l);
        for (i=l-t-1; i<l; i++) {
            c += steps[i];
        }
        System.out.println( (c/t) + " ");
    }


    public void processFlight(String registered_path, String arrivals_path) {
        int[] registered = getIds(registered_path),
              arrivals   = getIds(arrivals_path),
              boardees,
              seatSteps1,
              seatSteps2;

        // Stage 1.
        AvlTree[] trees = buildAVLs(registered);

        // Stage 2
        boardees = processArrivals(trees, registered, arrivals);

        System.out.println();
        for(int i=0; i<boardees.length; i++) {
            System.out.println(boardees[i]);
        }

        seatSteps1 = seatBoardees(boardees, true);
        seatSteps2 = seatBoardees(boardees, false);

        printSeatSteps(seatSteps1);
        printSeatSteps(seatSteps2);

        //System.out.print("\n\n\n\n");
        //for (int i=0; i<seatSteps1.length; i++) {
            //System.out.println(seatSteps1[i]);
        //}

        //System.out.print("\n\n\n\n");
        //for (int i=0; i<seatSteps2.length; i++) {
            //System.out.println(seatSteps2[i]);
        //}
        //System.out.print("\n\n\n\n");
    }

    //write_To_File_Example("output1.dat","number of ids = " + N);//write N to the output file
    //for(int i = 0; i <N ;i++) {
        //write_To_File_Example("output1.dat","id number " + i + " : " + ids[i]);//write ids[i] to the output file
    //}
}

