package net.xapxinh.dataservice.entity;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;


public class EFactory {
	
	public static Player newPlayer() {
		return new Player();
	}

	public static List<AlbumSong> createAlbumSongs(List<Song> songs, Album album) {
		List<AlbumSong> albumSongList = new ArrayList<AlbumSong>();
		if (songs == null || songs.isEmpty()) {
			return Collections.emptyList();
		}
		int size = songs.size();
		for (int i = 0; i < size; i++) {
			AlbumSong albumSong = new AlbumSong();
			albumSong.setAlbum(album);
			albumSong.setSong(songs.get(i));
			albumSong.setSongIndex(i);
			albumSongList.add(albumSong);
		}
		return albumSongList;
	}

	public static Set<AuthorAlbum> createAuthorAlbums(Set<Author> authors, Album album) {
		Set<AuthorAlbum> authorAlbumSet = new HashSet<AuthorAlbum>();
		if (authors == null || authors.isEmpty()) {
			return Collections.emptySet();
		}
		for (Author author : authors) {
			AuthorAlbum authorAlbum = new AuthorAlbum();
			authorAlbum.setAlbum(album);
			authorAlbum.setAuthor(author);
			authorAlbumSet.add(authorAlbum);
		}
		return authorAlbumSet;
	}

	public static Set<ArtistAlbum> createArtistAlbums(Set<Artist> artists, Album album) {
		Set<ArtistAlbum> artistAlbumSet = new HashSet<ArtistAlbum>();
		if (artists == null || artists.isEmpty()) {
			return Collections.emptySet();
		}
		for (Artist artist : artists) {
			ArtistAlbum artistAlbum = new ArtistAlbum();
			artistAlbum.setAlbum(album);
			artistAlbum.setArtist(artist);
			artistAlbumSet.add(artistAlbum);
		}
		return artistAlbumSet;
	}

	public static Set<ArtistSong> createArtistSongs(Set<Artist> artists, Song song) {
		Set<ArtistSong> artistSongSet = new HashSet<ArtistSong>();
		if (artists == null || artists.isEmpty()) {
			Collections.emptySet();
		}
		for (Artist artist : artists) {
			ArtistSong artistSong = new ArtistSong();
			artistSong.setArtist(artist);
			artistSong.setSong(song);
			artistSongSet.add(artistSong);
		}
		return artistSongSet;
	}
}