package net.xapxinh.dataservice.entity;

import java.io.File;
import java.io.Serializable;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

public class Song implements Serializable {
	
	private static final long serialVersionUID = 1L;
	
	private Long id;
	private String name;
	private String title;
	private String folder;
	private String duration;
	private Author author;
	private Set<AlbumSong> albumSongs;
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
	public String getFolder() {
		return folder;
	}
	public void setFolder(String folder) {
		this.folder = folder;
	}
	public String getTitle() {
		return title;
	}
	public void setTitle(String title) {
		this.title = title;
	}
	public String getDuration() {
		return duration;
	}
	public void setDuration(String duration) {
		this.duration = duration;
	}
	public Author getAuthor() {
		return author;
	}
	public void setAuthor(Author author) {
		this.author = author;
	}
	public Set<AlbumSong> getAlbumSongs() {
		return albumSongs;
	}
	public void setAlbumSongs(Set<AlbumSong> albumSongs) {
		this.albumSongs = albumSongs;
	}
	public Set<ArtistSong> getArtistSongs() {
		return artistSongs;
	}
	public void setArtistSongs(Set<ArtistSong> artistSongs) {
		this.artistSongs = artistSongs;
	}
	public String getPath() {
		return folder + File.separator + name;
	}
	public Set<Artist> getArtists() {
		if (artistSongs == null || artistSongs.isEmpty()) {
			return Collections.emptySet();
		}
		Set<Artist> artists = new HashSet<Artist>();
		for (ArtistSong artistSong : artistSongs) {
			artists.add(artistSong.getArtist());
		}
		return artists;
	}
	
	public String getArtistNames() {
		if (artistSongs == null) {
			return null;
		}
		String names = "";
		for (ArtistSong artistSong : artistSongs) {
			names = names + ", " + artistSong.getArtist().getName();
		}
		return names.replaceFirst(", ", "");
	}
	
	public String getAuthorName() {
		if (author == null) {
			return null;
		}
		return author.getName();
	}
}
