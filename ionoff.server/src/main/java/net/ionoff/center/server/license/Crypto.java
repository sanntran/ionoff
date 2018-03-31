package net.ionoff.center.server.license;

import com.googlecode.gwt.crypto.client.TripleDesCipher;
import com.googlecode.gwt.crypto.client.TripleDesKeyGenerator;

public class Crypto {

	private static final String KEY = "a49b519283fd7af2e09bc4c27c89ec7345a1a41aabadc49b";
	
	private TripleDesCipher cipher;
	private TripleDesKeyGenerator keygen;

	private static Crypto instance = null;

	public static Crypto getInstance() {
		if (instance == null) {
			instance = newInstance(KEY);
		}
		return instance;
	}
	
	private static Crypto newInstance(String key) {
		if (instance == null) {
			instance = new Crypto(key);
		}
		return instance;
	}

	private Crypto() {
		// keygen.encodeKey(keygen.generateNewKey()));
		cipher = new TripleDesCipher();
		keygen = new TripleDesKeyGenerator();
		cipher.setKey(keygen.decodeKey(KEY));
	}
	
	private Crypto(String key) {
		// keygen.encodeKey(keygen.generateNewKey()));
		cipher = new TripleDesCipher();
		keygen = new TripleDesKeyGenerator();
		cipher.setKey(keygen.decodeKey(key));
	}

	public String encrypt(String value) throws Exception {
		return cipher.encrypt(String.valueOf(value));
	}

	public String decrypt(String enc) throws Exception {
		return cipher.decrypt(enc);
	}
	
	public String generateKey() {
		return keygen.encodeKey(keygen.generateNewKey());
	}
}
