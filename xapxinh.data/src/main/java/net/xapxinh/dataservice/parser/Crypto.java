package net.xapxinh.dataservice.parser;

import com.googlecode.gwt.crypto.client.TripleDesCipher;
import com.googlecode.gwt.crypto.client.TripleDesKeyGenerator;

public class Crypto {

	private static final String SONG_KEY = "a49b519283fd7af2e09bc4c27c89ec7345a1a41aabadc49b";
	private static final String ALBUM_KEY = "0b4cba897fb5011c1c685d6d072c860875b92ab019a1680d";
	
	private TripleDesCipher cipher;
	private TripleDesKeyGenerator keygen;

	private static Crypto songInstance = null;
	private static Crypto albumInstance = null;

	public static Crypto getSongInstance() {
		if (songInstance == null) {
			songInstance = new Crypto(SONG_KEY);
		}
		return songInstance;
	}
	
	public static Crypto getAlbumInstance() {
		if (albumInstance == null) {
			albumInstance = new Crypto(ALBUM_KEY);
		}
		return albumInstance;
	}

	private Crypto() {
		// keygen.encodeKey(keygen.generateNewKey()));
		cipher = new TripleDesCipher();
		keygen = new TripleDesKeyGenerator();
		cipher.setKey(keygen.decodeKey(SONG_KEY));
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
