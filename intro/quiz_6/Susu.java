public class Susu {
    public static boolean calcWeights(int[] weights, int i, int sum) {
        boolean res = false;

        if (sum == 0) {
            res = true;
        } else if (i >= weights.length) {
            res = false;
        } else {
            res = (calcWeights(weights, i+1, sum - weights[i]) ||
                   calcWeights(weights, i+1, sum));
        }
        return res;
    }


    public static boolean susuOne(int[] w, int i, int v) {
        // Base case: If we finished testing the current combination.
        if (i<0) {
            if (v==0) {
                return true;
            } else {
                return false;
            }
        // If base case wasn't met:
        // Keep testing the combination w & w/out the current weight.
        } else if (susuOne(w, i-1, v - w[i])) {
                System.out.print("1 ");
                return true;
        } else if (susuOne(w, i-1, v)) {
                System.out.print("0 ");
                return true;
        }

        return false;
    }


    public static int susuAll(int[] w, int i, int v) {
        // Base case: If we passed the value.
        if (v<0) {
            return 0;
        // Base case: If finished testing the current combination.
        } else if (i >= w.length) {
            if (v == 0) {
                return 1;
            } else {
                return 0;
            }
        // If base cases weren't met:
        // Keep testing the combination w & w/out the current weight.
        } else {
            return (susuAll(w, i+1, v - w[i]) +
                    susuAll(w, i+1, v));
        }
    }

    
    public static boolean hannukah(int[] c, int day) {
        // Base case: If we finished Hannukah succesfully! :)
        if (day > 8) {
            return true;
        }

        // Iterate over all available colors and test if we
        // have enough for the current day.
        for (int i=0; i<c.length; i++) {
            // If enough, use the candles and substract them from
            // the pack.
            if (c[i] >= day) {
                c[i] -= day;
                if (hannukah(c, day+1)) {
                    return true;
                } else {
                    // Try a different combo if the current one failed:
                    // Re-use the candles.
                    c[i] += day;
                }
            }
        }

        // If there weren't enough candles, trash the current combo.
        return false;
    }


    public static void main(String[] main) {
        int[] w1 = {1,7,9,3,12};
        System.out.println(calcWeights(w1, 0, 12));

        int[] w2 = {3, 17 ,26, 7, 9, 21, 6, 12, 13, 19, 8, 38};
        System.out.println("= " + susuOne(w2, w2.length -1, 165));

        int[] w3 = {3, 7, 9, 1, 2, 6, 4, 5};
        System.out.println(susuAll(w3, 0, 12));

        int[] c = {7, 5, 23, 5, 4};
        System.out.println(hannukah(c, 1));
    }
}
