package net.xapxinh.player.systemtray;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;

import org.apache.log4j.Logger;

public class WindowsReqistry {
	private static final Logger LOGGER = Logger.getLogger(WindowsReqistry.class
			.getSimpleName());

	public static final String START_UP_LOCATION = "HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Run";
	public static final String XMP_VALUE = "XMP";

	private static class StreamReader extends Thread {
		private InputStream is;
		private StringWriter sw = new StringWriter();;

		public StreamReader(InputStream is) {
			this.is = is;
		}

		public void run() {
			try {
				int c;
				while ((c = is.read()) != -1)
					sw.write(c);
			} catch (IOException e) {
			} finally {
				try {
					is.close();
				} catch (IOException e) {
					LOGGER.error(e.getMessage(), e);
				}
			}
		}

		public String getResult() {
			return sw.toString();
		}
	}

	public static boolean deleteValue(String key, String valueName) {
		try {
			// Run reg query, then read output with StreamReader (internal
			// class)
			String cmd = "reg delete \"" + key + "\" /v \"" + valueName
					+ "\" /f";
			LOGGER.info("Execute " + cmd);
			Process process = Runtime.getRuntime().exec(cmd);

			StreamReader reader = new StreamReader(process.getInputStream());
			reader.start();
			process.waitFor();
			reader.join();
			String output = reader.getResult();

			// Output has the following format:
			// \n<Version information>\n\n<key>\t<registry type>\t<value>
			return output.contains("The operation completed successfully");
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
		}
		return false;
	}

	public static boolean addValue(String key, String valName, String val) {
		try {
			// Run reg query, then read output with StreamReader (internal
			// class)
			String cmd = "reg add \"" + key + "\" /v \"" + valName + "\" /d \""
					+ val + "\" /f";
			LOGGER.info("Execute " + cmd);
			Process process = Runtime.getRuntime().exec(cmd);

			StreamReader reader = new StreamReader(process.getInputStream());
			reader.start();
			process.waitFor();
			reader.join();
			String output = reader.getResult();

			// Output has the following format:
			// \n<Version information>\n\n<key>\t<registry type>\t<value>
			return output.contains("The operation completed successfully");
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
		}
		return false;
	}

	public static final String readRegistry(String location, String key) {
		try {
			// Run reg query, then read output with StreamReader (internal
			// class)
			Process process = Runtime.getRuntime().exec(
					"reg query \"" + location + "\" /v " + key);
			StreamReader reader = new StreamReader(process.getInputStream());
			reader.start();
			process.waitFor();
			reader.join();
			String result = reader.getResult();
			return result;
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
		}
		return null;
	}
}