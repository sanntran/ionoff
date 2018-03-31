package net.xapxinh.center.observable;

import java.util.HashMap;
import java.util.Map;


public class ObservableDemo {

	public static void main(String[] args) {
		final ObserverbleObject eventSource1 = new ObserverbleObject("EventSource1");
		final ObserverbleObject eventSource2 = new ObserverbleObject("EventSource2");

		final ValueChangedHandler handler = new ValueChangedHandler("Watcher");

		eventSource1.addObserver(handler);
		eventSource1.setValue("EventSource12");
		System.out.println("main set value EventSource12");
		eventSource2.addObserver(handler);
		eventSource2.setValue("EventSource22");
		System.out.println("main set value EventSource22");

		final Map<Integer, Boolean> x = new HashMap<Integer, Boolean>();
		System.out.println(x.get(1));
	}
}
