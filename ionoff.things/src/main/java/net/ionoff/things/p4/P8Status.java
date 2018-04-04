package net.ionoff.things.p4;

import java.util.ArrayList;
import java.util.List;

class P8Status {
	
	private final List<Boolean> inputStates;
	private final List<Boolean> outputStates;
	
	P8Status() {
		inputStates = new ArrayList<>(8); 
		outputStates = new ArrayList<>(8);
		
		for (int i = 0; i < 8; i++) {
			inputStates.add(null);
			outputStates.add(null);
		}
	}
	
	List<Boolean> getInputStates() {
		return inputStates;
	}
	
	List<Boolean> getOutputStates() {
		return outputStates;
	}
	
	static P8Status parseP8Status(String inputStatus, String outputStatus) {
		P8Status status = new P8Status();
		for (int i = 0; i < 8; i++) {
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
