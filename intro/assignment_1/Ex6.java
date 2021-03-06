import java.util.Scanner;

public class Ex6 {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);

        int input = scan.nextInt();

        int left;  // i%10.

        int sum;        // Sum of digits.
        int total = 0;  // Total sum of numbers.
        
        // Prints the sum of all integers until (including) the integer given as input,
        // where each one's sum of all its digits divide by 7 with no remainder.
        for (int i=1; i<=input; i++) {
            left = i;
            sum = 0;

            do {
                sum += left%10;
                left = left/10;
            } while (left != 0);

            if (sum%7 == 0) {
                total += i;
            }
        }

        System.out.println(total);
    }
}
