import java.util.Scanner;

public class Ex4 {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);

        int input = scan.nextInt();

        // Print all Pythagorean triples (a^2 + b^2 = c^2) until the limit given as input.
        // Iterates over all c's, b's & a's, checks and prints triples.
        for (int c=5; c<input; c++) {
            for (int b=3; b<c; b++) {
                for (int a=3; a<b; a++) {
                    if (a*a + b*b == c*c) {
                        System.out.println("(" + a + "," + b + "," + c + ") : " + a + "*" + a + " + " + b + "*" + b + " = " + c + "*" + c);
                    }
                }
            }
        }
    }
}
