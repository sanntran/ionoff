package net.xapxinh.dataservice.entity;

import java.io.Serializable;

public class AlbumSong implements Serializable {
	
	private static final long serialVersionUID = 1L;
	
	private Long id;
	private Album album;
	private Song song;
	private Integer songIndex;
	
	public Long getId() {
		return id;
	}
	public void setId(Long id) {
		this.id = id;
	}
	
	public Album getAlbum() {
		return album;
	}
	public void setAlbum(Album album) {
		this.album = album;
	}
	
	public Song getSong() {
		return song;
	}
	public void setSong(Song song) {
		this.song = song;
	}
	
	public Integer getSongIndex() {
		return songIndex;
	}
	public void setSongIndex(Integer songIndex) {
		this.songIndex = songIndex;
	}
}
