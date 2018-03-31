package net.xapxinh.dataservice.entity;

import java.io.Serializable;
import java.util.Set;

public class Author implements Serializable {
	
	private static final long serialVersionUID = 1L;
	
	private Long id;
	private String name;
	
	private Set<AuthorAlbum> authorAlbums;
	private Set<Song> songs;
	
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
	
	public Set<AuthorAlbum> getAuthorAlbums() {
		return authorAlbums;
	}
	public void setAuthorAlbums(Set<AuthorAlbum> authorAlbums) {
		this.authorAlbums = authorAlbums;
	}
	
	public Set<Song> getSongs() {
		return songs;
	}
	public void setSongs(Set<Song> songs) {
		this.songs = songs;
	}
}
