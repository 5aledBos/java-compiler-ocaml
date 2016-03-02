package tutu.titi.toto;

public class C extends B {
	public String method() {
   		return "result method c";
    }
    public int cmethod() {
		B test = new B(4);
		return test.getAge(3);
	}

	public static void main(String [] args) {
        B bobject = new B();
		bobject.teststatic();
		C cobject = new C();
		int aaaaage = bobject.getAge(5);
		bobject.age++;
		String s;
		s = "testmodifie";
		int age5 = bobject.getAge(5);
		int chiffre = bobject.getAge(2);
		String f = bobject.getString(false);
		String t = bobject.getString(true);
		String result = "test";
		result += "test";
  		s += "test";
		age5 -= 30;
		age5*= 10;
		age5 = 5+10;
		5 == 6;
//        result -= "test";
		B newobject = bobject;
		newobject = new B(bobject.age);
		boolean b = true;
//		if(false) {
//			int essa = 5;
//		}
//		else {
//			int essa = 4;
//			essa--;
//		}
		int i = 0;
		for(i = 0; i<20; i++) {
			B oeoeoe = new B();
		}	
//		for(i;;i +=1) {
//			i = 10;	
//			i++;
//			newobject.test();
//		}
		5++;
		while(i < 50) {
			i++;
//			int j = 5;
		}
		int j = 2;
		try {
			int l = j/0;
		}
		catch(NullPointerException npe) {
//			printf("");
		}
		catch(ArithmeticException e) {
			int variabmearithemetic = 60;
		}
		finally {
			int finalllll = 8;
		}
		try {
			throw new MyException();
		}
		catch(MyException e) {
			int catchee = 30;
			catchee = e.testException();
		}
		int c = 688;
		A a = new A();
		a.age;
		a.age =3333;
		B tstb = new B(4);
		int ksks = 3;
		String fee = (String) ksks;
		if(fee instanceof String) {
			int fstring = 5;
		}
		boolean a2 = false;
//		int test = (int)  5;
//		String j = "5m";
		int k = (int) j;
//		int castint = (int) bobject;

		int [][] tab = new int [5][5];
//		tab[2];
//		tab = new int [5];
		int[] myInt = new int[]{1,2,3};
		
//		Int untestInt = new Int(5);
//		untestInt.getInt(5);
	}


}

public class B {

    private int age;
	private A a;
    B() {
        age = 5;
    }
    public B(int bint) {
        age = bint;
//		this.age;
//		C c = new C();
		a = new A(age, 100, "hello world", "bateau mouche", true);
    }

//    public String b = "coucou";

	public int getAge(int bint) {
//	    age = 8;
		return age;
	}

	public String getString(boolean b) {
	if(b)
		return "true";
	else {

	B coucou = new B(5);
	return "false";
	}
	
	}

    public String method() {
        return "result method b";
    }

	public static void teststatic() {
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

	public void printf(String s) {
	}

	
}

public class A {
	int age;
	int a1;
	String s;
	String bateau = "bateau";
	boolean myBoolan = true;
	C myCobject = new C();
	public A() {
//		int j = 0;
	}

	public A(int j) {
		age = j;
		a1 = 33;
	}

	public A(int age1, int a11, String s1, String bateau1, boolean myBoolan1) {
		age = age1;
		a1 = a11;
		s = s1;
		bateau = bateau1;
		myBoolan = myBoolan1;
//		myCobject = myCobject;
	}

    
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

}




public class MyException extends Exception {
	int i;
	MyException () {
	}
	int testException() {
		return 8;
	}
}
