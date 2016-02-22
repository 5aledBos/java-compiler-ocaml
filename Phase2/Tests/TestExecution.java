package tutu.titi.toto;

public class B {

    private int age;
	private A a = new A();
    B() {
        age = 5;
    }
    public B(int bint) {
        age = bint;
    }

//    public String b = "coucou";

    public String method() {
        return "result";
    }

    public String method2(int a) {
      return "coucou";
    }

    public String test() {
      return method();
    }

    public String test2() {
      return method2(2);
    }

	public static void main(String [] args) {
//		int a = 2;
//	//	Int b = (Int) 5;cd
//		int c = 5;
//		Int b;
        B bobject = new B();
		C cobject;
	}
}

public class A {

    int a1 = 1;
//    int[] a = { 3,4,5 };
//    Bool a2 = false;
//    String a6 = "jsdjkndjk\njdsdjkf";

    private String method() {
        B b = new B();
        B bage = new B(20);
        return "b.b";
    }

    public void params(String toto, int tutu) {
        tutu++;

        for (int i = 0; i < tutu; i++) {
            String astr = "Compilo";
            i += 5;
        }

        try {
            method();
        } catch (RuntimeException r) {
            r;
            String c1 = "catch 1";
        } catch (B b) {
            String c2 = b.method();
        } finally {
            float t = .1;
            t *= 5.;
        }
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

public class C extends B {
	public String method() {
   		return "result";
    }
}
