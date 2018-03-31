package net.xapxinh.dataservice.entity;

import java.io.Serializable;
import java.util.Set;

public class Artist implements Serializable {
	
	private static final long serialVersionUID = 1L;
	
	private Long id;
	private String name;
	private Set<ArtistAlbum> artistAlbums;
	private Set<ArtistSong> artistSongs;
	
	public Long getId() {
		return id;
	}
	public void setId(Long id) {
		this.id = id;
	}
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public Set<ArtistAlbum> getArtistAlbums() {
		return artistAlbums;
	}
	public void setArtistAlbums(Set<ArtistAlbum> artistAlbums) {
		this.artistAlbums = artistAlbums;
	}
	public Set<ArtistSong> getArtistSongs() {
		return artistSongs;
	}
	public void setArtistSongs(Set<ArtistSong> artistSongs) {
		this.artistSongs = artistSongs;
	}
}
