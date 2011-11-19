import java.util.Scanner;

public class Ex4 {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);

        int input = scan.nextInt();

        // Print all Pythagorean triples (a^2 + b^2 = c^2) until the limit entered as input.
        // Iterates over all c's, b's & a's, checks and prints triples.
        for (int c=2; c<input; c++) {
            for (int b=2; b<c; b++) {
                for (int a=2; a<b; a++) {
                    if (b*b + a*a == c*c) {
                        System.out.println("(" + a + "," + b + "," + c + ") : " + a + "*" + a + " + " + b + "*" + b + " = " + c + "*" + c);
                    }
                }
            }
        }
    }
}
