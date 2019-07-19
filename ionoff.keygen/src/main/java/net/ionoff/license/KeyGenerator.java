package net.ionoff.license;

public class KeyGenerator {
	
	public static void main(String[] args) {
		//if (args.length > 0) {
			String mac = "b8-27-eb-73-77-2b";
			try {
				String key = Crypto.getInstance().encrypt(mac);
				System.out.println(key);
			} catch (Exception e) {
				e.printStackTrace();
			}
		//}
	}
}
