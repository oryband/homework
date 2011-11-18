import java.util.Scanner;

public class Ex4 {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);

        int input = scan.nextInt();

        int left, right;  // Pythgorean triple wings.

        for (int c=2; c<input; c++) {
            for (int b=2; b<c; b++) {
                for (int a=2; a<b; a++) {
                    left  = b*b + a*a;
                    right = c*c;

                    if (left == right) {
                        System.out.println("(" + a + "," + b + "," + c + ") : " + a + "*" + a + " + " + b + "*" + b + " = " + c + "*" + c);
                    }
                }
            }
        }
    }
}
