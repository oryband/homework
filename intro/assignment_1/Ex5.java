import java.util.Scanner;

public class Ex5 {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);

        int input = scan.nextInt();

        for (int i=2; i<input; i++) {
            while(input%i == 0) {
                System.out.println(i);

                input = input/i;
            }
        }

        if (input != 1) {
            System.out.println(input);
        }
    }
}
