package net.ionoff.broker;

public class MainTest {

    public static void main(String[] args) {
        String s = new MainTest().reverseParentheses("asldfkj(sldkf)slkjflwe(lsdfj)lskdjf");
        System.out.println(s);
    }


    String reverseParentheses(String s) {
        StringBuilder result = new StringBuilder();
        int n = s.length();

        for (int i = 0; i < n; i++) {
            if (s.charAt(i) == '(') {
                StringBuilder rb = new StringBuilder();
                for (int j = i + 1; j < n; j++) {
                    if (s.charAt(j) == ')') {
                        result.append(revert(rb.toString()));
                        i = j;
                        break;
                    }
                    else {
                        rb.append(s.charAt(j));
                    }
                }
            }
            else {
                result.append(s.charAt(i));
            }
        }
        return result.toString();

    }

    private String revert(String s) {
        StringBuilder rb = new StringBuilder();
        int n = s.length();
        for (int i = 0; i < n ; i++) {
            rb.append(s.charAt(n - i -1));
        }
        return rb.toString();
    }


    int[] integerAdder(int[] a, int[] b) {
        int n = a.length;

        if (a.length < b.length) {
            n = b.length;
        }
        else if (a.length > b.length) {
            n = a.length;
        }
        int x[] = new int[n];
        int y[] = new int[n];
        for (int i = 0; i < n; i++ ) {
            if (i < a.length) {
                x[n - 1 - i] = a[a.length - 1 - i];
            }
            else {
                x[n - 1 - i] = 0;
            }
            if (i < b.length) {
                y[n - 1 - i] = b[b.length - 1 - i];
            }
            else {
                y[n - 1 - i] = 0;
            }
        }

        int result[] = new int[n + 1];


        int remember = 0;

        for (int i = n - 1; i >=0; i--) {
            int sum = x[i] + y[i] + remember;
            if (sum >= 10) {
                result[i + 1] = sum % 10;
                remember = 1;
            }
            else {
                result[i + 1] = sum;
                remember = 0;
            }
        }
        result[0] = remember;

        if (result[0] == 0) {
            int result2[] = new int[result.length - 1];
            for (int i = 0; i < result2.length; i++) {
                result2[i] = result[i + 1];
            }
            return result2;
        }

        return result;


    }


}