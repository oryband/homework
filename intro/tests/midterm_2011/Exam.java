public class Exam {
    public static void primeFactors(int n) {
        int d=0,  // Current Divider.
            i=2;  // Smallest common divider possible.

        while (i<=n) {
            if (n%i == 0) {
                n /= i;
                if (i>d) {
                    d = i;
                    System.out.print(d + " ");
                }
            } else {
                i++;
            }
        }

        System.out.println();
    }


    public static boolean rec(int[] a, int value) {
        return rec(a.length-1, a, value);
    }

    public static boolean rec(int i, int[] a, int value) {
        boolean answer;

        //if (i==0) {
            //answer = (a[0]==value);
        if (i<0) {
            if (value == 0) {
                answer = true;
            } else {
                answer = false;
            }
        } else {
            answer = rec(i-1, a, value - a[i]) ||
                     rec(i-1, a, value + a[i]);
        }

        return answer;
    }


    public static void main(String[] args) {
        primeFactors(396);

        int[] a1 = {2,3,6,10};
        int[] a2 = {5,4,6,3};
        System.out.println(rec(a1, -5));
        System.out.println(rec(a2, 11));
    }
}
