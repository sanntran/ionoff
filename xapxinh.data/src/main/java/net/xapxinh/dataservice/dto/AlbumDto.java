package net.xapxinh.dataservice.dto;

import java.util.List;

public class AlbumDto {

	private long id;
	private String name;
	private String title;
	private String releaseDate;
	private String artist;
	private String author;
	private String image;
	private List<SongDto> songs;
	private Integer listenCount;

	public long getId() {
		return id;
	}
	public void setId(long id) {
		this.id = id;
	}
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public String getTitle() {
		return title;
	}
	public void setTitle(String title) {
		this.title = title;
	}
	public String getArtist() {
		return artist;
	}
	public void setArtist(String artist) {
		this.artist = artist;
	}
	public String getAuthor() {
		return author;
	}
	public void setAuthor(String author) {
		this.author = author;
	}
	public String getImage() {
		return image;
	}
	public void setImage(String image) {
		this.image = image;
	}
	public List<SongDto> getSongs() {
		return songs;
	}
	public void setSongs(List<SongDto> songs) {
		this.songs = songs;
	}
	public String getReleaseDate() {
		return releaseDate;
	}
	public void setReleaseDate(String releaseDate) {
		this.releaseDate = releaseDate;
	}

	public Integer getListenCount() {
		return listenCount;
	}
	public void setListenCount(Integer listenCount) {
		this.listenCount = listenCount;
	}
}
