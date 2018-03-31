package net.xapxinh.center.observable;

import java.util.Observable;
import java.util.Observer;

public class ValueChangedHandler implements Observer {
	public String name;

	public ValueChangedHandler(String name) {
		this.name = name;
	}

	public void update(Observable obj, Object arg) {
		System.out.println("Update called");
		try {
			Thread.sleep(2000);
		}
		catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		System.out.println(name + " update finished");
	}
}
