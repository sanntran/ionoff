package net.ionoff.things.e4;

import java.util.ArrayList;
import java.util.List;

public class E4Status {
	
	private final List<Boolean> inputStates;
	private final List<Boolean> outputStates;
	
	public E4Status() {
		inputStates = new ArrayList<>(4); 
		outputStates = new ArrayList<>(4);
		
		for (int i = 0; i < 4; i++) {
			inputStates.add(null);
			outputStates.add(null);
		}
	}
	
	public List<Boolean> getInputStates() {
		return inputStates;
	}
	
	public List<Boolean> getOutputStates() {
		return outputStates;
	}
	
	public static E4Status fromStrings(String inputStatus, String outputStatus) {
		E4Status status = new E4Status();
		for (int i = 0; i < 4; i++) {
			status.getInputStates().set(i, toBoolean(inputStatus.charAt(i)));
			status.getOutputStates().set(i, toBoolean(outputStatus.charAt(i)));
		}
		return status; 
	}
	
	private static Boolean toBoolean(char c) {
		if ('0' == c) {
			return false;
		}
		else if ('1' == c) {
			return true; 
		}
		return null;
	}
}
