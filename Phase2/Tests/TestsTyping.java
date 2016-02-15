package tutu.titi.toto;

public class A {

    int a1 = 1;
    int[] a = { 3,4,5 };
    Bool a2 = false;
    String a6 = "jsdjkndjk\njdsdjkf";

    private String method() {
        return "result";
    }

    int test() {

        4 = 3;
        43.0 = (float)43;

        int[] myIntArray = new int[3];
        int[] myArray = {1,2,a1};
        int[] myInt = new int[]{1,2,3};
        float[] a = { 1.2, 2.3, 3.4 };
        String[] str = { "str", method() };

        4++;
        --27.;

        int c = (3 + 3);
        if (true) {
          (true? c: 2);
        }

        String call = method();

        // "void.class" = void.class;
        // "int.class" = int.class;
        // "intarray.class" = int[].class;
        // "Instanceof" = 23 instanceof Double;

        throw 45=3;

        if (43==3) {
          int d = 3;
            d++;
        } else {
            43--;
            c--;
        }

        while(true) {
            43++;
        }

        return 34;
    }
}
