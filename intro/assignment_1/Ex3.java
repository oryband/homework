import java.util.Scanner;

public class Ex3 {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);

        int input;

        // A>B>C
        int a = -1;
        int b = -1;
        int c = -1;

        // Recieves integers until given '0' as input, and prints the third biggest integer given so far.
        do {
            input = scan.nextInt();

            // Init the three biggest integers.
            if (input == 0) {}
            else if (a == -1) {
                a = input;
            } else if (b == -1) {
                if (input > a) {
                    b = a;
                    a = input;
                } else {
                    b = input;
                }
            } else if (c == -1) {
                if (input > a) {
                    c = b;
                    b = a;
                    a = input;
                }
                else if (input > b) {
                    c = b;
                    b = input;
                } else {
                    c = input;
                }
            }

            // Compare and assign the given integer to the three biggest integers, if needed.
            else {
                if (input > a) {
                    c = b;
                    b = a;
                    a = input;
                } else if (input > b) {
                    c = b;
                    b = input;
                } else if (input > c) {
                    c = input;
                }
            }
        } while (input != 0);

        System.out.print(c);
    }
}
