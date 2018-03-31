package net.xapxinh.dataservice.entity;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Album implements Serializable {

	private static final long serialVersionUID = 1L;

	private Long id;
	private String name;
	private String title;
	private String image;
	private String releaseDate;
	private String description;
	private Integer listenCount;
	private Date uploadedDate;
	private Set<ArtistAlbum> artistAlbums;
	private Set<AuthorAlbum> authorAlbums;
	private List<AlbumSong> albumSongs;

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
	public String getTitle() {
		return title;
	}
	public void setTitle(String title) {
		this.title = title;
	}
	public String getImage() {
		return image;
	}
	public void setImage(String image) {
		this.image = image;
	}
	public String getDescription() {
		return description;
	}
	public void setDescription(String description) {
		this.description = description;
	}
	public Integer getListenCount() {
		return listenCount;
	}
	public void setListenCount(Integer listenCount) {
		this.listenCount = listenCount;
	}

	public Date getUploadedDate() {
		return uploadedDate;
	}
	public void setUploadedDate(Date uploadedDate) {
		this.uploadedDate = uploadedDate;
	}

	public String getReleaseDate() {
		return releaseDate;
	}
	public void setReleaseDate(String releaseDate) {
		this.releaseDate = releaseDate;
	}
	public Set<ArtistAlbum> getArtistAlbums() {
		return artistAlbums;
	}
	public void setArtistAlbums(Set<ArtistAlbum> artistAlbums) {
		this.artistAlbums = artistAlbums;
	}
	public Set<AuthorAlbum> getAuthorAlbums() {
		return authorAlbums;
	}
	public void setAuthorAlbums(Set<AuthorAlbum> authorAlbums) {
		this.authorAlbums = authorAlbums;
	}
	public List<AlbumSong> getAlbumSongs() {
		return albumSongs;
	}
	public void setAlbumSongs(List<AlbumSong> albumSongs) {
		this.albumSongs = albumSongs;
	}

	public Set<Artist> getArtists() {
		if (artistAlbums == null || artistAlbums.isEmpty()) {
			return Collections.emptySet();
		}
		final Set<Artist> artists = new HashSet<Artist>();
		for (final ArtistAlbum artistAlbum : artistAlbums) {
			artists.add(artistAlbum.getArtist());
		}
		return artists;
	}

	public Set<Author> getAuthors() {
		if (authorAlbums == null || authorAlbums.isEmpty()) {
			return Collections.emptySet();
		}
		final Set<Author> authors = new HashSet<Author>();
		for (final AuthorAlbum authorAlbum : authorAlbums) {
			authors.add(authorAlbum.getAuthor());
		}
		return authors;
	}

	public List<Song> getSongs() {
		if (albumSongs == null || albumSongs.isEmpty()) {
			return Collections.emptyList();
		}
		final List<Song> songs = new ArrayList<Song>();
		for (final AlbumSong albumSong : albumSongs) {
			if (albumSong != null) {
				songs.add(albumSong.getSong());
			}
		}
		return songs;
	}
}
