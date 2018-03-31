package net.xapxinh.dataservice.entity;

import java.io.Serializable;

public class ArtistSong implements Serializable {
	
	private static final long serialVersionUID = 1L;
	
	private Long id;
	private Artist artist;
	private Song song;

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
	
	public Song getSong() {
		return song;
	}
	public void setSong(Song song) {
		this.song = song;
	}
}
