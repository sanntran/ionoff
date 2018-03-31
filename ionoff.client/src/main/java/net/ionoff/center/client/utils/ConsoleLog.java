package net.ionoff.center.client.utils;

public class ConsoleLog {
	public static native void println( String message) /*-{
	    console.log(message );
	}-*/;
}
