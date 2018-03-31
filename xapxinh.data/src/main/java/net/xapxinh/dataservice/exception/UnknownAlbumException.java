package net.xapxinh.dataservice.exception;

public class UnknownAlbumException extends Exception {

	private static final long serialVersionUID = 1L;

	public UnknownAlbumException(String albumName) {
		super("Unknown album: " + albumName);
	}
}
