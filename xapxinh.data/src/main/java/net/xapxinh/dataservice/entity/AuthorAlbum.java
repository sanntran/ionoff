package net.xapxinh.dataservice.entity;

import java.io.Serializable;

public class AuthorAlbum implements Serializable {
	
	private static final long serialVersionUID = 1L;
	
	private Long id;
	private Author author;
	private Album album;
	
	public Long getId() {
		return id;
	}
	public void setId(Long id) {
		this.id = id;
	}
	public Author getAuthor() {
		return author;
	}
	public void setAuthor(Author author) {
		this.author = author;
	}
	
	public Album getAlbum() {
		return album;
	}
	public void setAlbum(Album album) {
		this.album = album;
	}
}
