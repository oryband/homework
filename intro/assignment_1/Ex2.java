import java.util.Scanner;

public class Ex2 {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);

        int limit = scan.nextInt();

        boolean is_prime;

        int c = 1;  // Counter. Initial value includes 2.
        int s = 2;  // Sum.

        // Print the average of all primes from 2 to the limit given as input.
        if (limit != 2) {
            for (int i=limit-1; i>2; i--) {
                is_prime = true;

                // Check if the current number is odd and is a prime.
                if (i%2 != 0) {
                    for (int k=i-1; k>2 && is_prime; k--) {
                        if (i%k == 0) {
                            is_prime = false;
                        }
                    }
                // All Even numbers are non-prime.
                // Note '2' has already been added to the sum & counter.
                } else {
                    is_prime = false;
                }

                // If the current number is a prime, add it to the sum and increase the counter.
                if (is_prime)  {
                    c++;
                    s += i;
                }
            }
        }

        System.out.println( (float) s / (float) c );
    }
}

