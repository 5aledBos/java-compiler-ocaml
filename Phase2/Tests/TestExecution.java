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

	public int getAge(int age1) {
	    age = 8;
		return age;
	}
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
        B bobject = new B();
		C cobject = new C();
		cobject.cmethod();
		String s;
		s = "testmodifie";
		int age5 = 10;
		age5 = bobject.getAge(5);
		int chiffre = bobject.getAge(2);
		String result = "test";
		result += "test";
  		s += "test";
		age5 -= 30;
		age5*= 10;
		age5 = 5+10;
		5 == 6;
//        result -= "test";
		B newobject = bobject;
		newobject = new B();
		boolean b = true;
		if(false) {
			int essa = 5;
		}
		else {
			int essa = 4;
			essa--;
		}
		int i = 0;
		for(i = 0; i<20; i++) {
			B oeoeoe = new B();
		}	
//		for(i;;i +=1) {
//			i = 10;	
//			i++;
//			newobject.test();
//		}

		while(i < 50) {
			i++;
			int j = 5;
		}
		int j = 8;
		int k = j;
	}
}

public class A {

    int a1;
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
    public void cmethod() {
		B test = new B();
		test.getAge(3);
	}
}
