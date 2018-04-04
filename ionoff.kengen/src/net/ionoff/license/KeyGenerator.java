package net.ionoff.license;

public class KeyGenerator {
	
	public static void main(String[] args) {
		//if (args.length > 0) {
			String mac = "48-4D-7E-BA-16-71";
			try {
				String key = Crypto.getInstance().encrypt(mac);
				System.out.println(key);
			} catch (Exception e) {
				e.printStackTrace();
			}
		//}
	}
}
