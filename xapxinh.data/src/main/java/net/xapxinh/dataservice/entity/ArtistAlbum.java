package net.xapxinh.dataservice.entity;

import java.io.Serializable;

public class ArtistAlbum implements Serializable {
	
	private static final long serialVersionUID = 1L;
	
	private Long id;	
	private Artist artist;	
	private Album album;
	
	public Long getId() {
		return id;
	}
	public void setId(Long id) {
		this.id = id;
	}
	
	public Artist getArtist() {
		return artist;
	}
	public void setArtist(Artist artist) {
		this.artist = artist;
	}
	
	public Album getAlbum() {
		return album;
	}
	public void setAlbum(Album album) {
		this.album = album;
	}
}
