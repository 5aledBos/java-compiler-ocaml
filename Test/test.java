package com.tus.test;

import static test.te;
import java.lang.*;
import static java.io.*;


public class test extends machin implements testable, truc  {

	transient public int i, j;
	public final String test;

	private test(int i) {
	super(5, 6, 8);
    // assignments
    abc *= 6.2;
    array[5] = 5;
    
    t = new long[3][5][];

    // assert
    assert a = 1;
    assert a = 1 : b = 3;
    
    // field access
    u = "field".access;
    
    // method invocation
    v = method.invoc("ation", 10);
    
    // array access
    w = arr.ay["access"];
    x = abc.this[10];
  }

}


public class secondClass {
		public test(int i, int j) {
		}
}


public enum testable {
	BLACK, RED, LIGHT;
	public int test(int i) {
	}
}
