package net.xapxinh.dataservice.parser;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import net.xapxinh.dataservice.entity.Album;
import net.xapxinh.dataservice.entity.*;
import org.apache.log4j.Logger;

import java.io.File;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

public class XmlAlbumParser {
	private static final Logger logger = Logger.getLogger(XmlAlbumParser.class.getName());

	public static Album parseXmlAlbum(File xmlAlbumFile) throws Exception {

		logger.info("Reading xml album file: " + xmlAlbumFile.getAbsolutePath());

		XmlMapper xmlMapper = new XmlMapper();
		xmlMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

		// read file and put contents into the string
		String readContent = new String(Files.readAllBytes(xmlAlbumFile.toPath()));

		// deserialize from the XML into a Phone object
		net.xapxinh.dataservice.parser.Album album
				= xmlMapper.readValue(readContent, net.xapxinh.dataservice.parser.Album.class);

		return createAlbum(album);
	}

	private static Album createAlbum(net.xapxinh.dataservice.parser.Album xmlAlbum) {
		final Album album = new Album();
		logger.info("Album ------------------------------------");

		setAlbumAttribute(xmlAlbum, album);
		final List<Song> songs = new ArrayList<Song>();
		for (int i = 0; i < xmlAlbum.getMusicFiles().size(); i++) {
			final MusicFile musicFile = xmlAlbum.getMusicFiles().get(i);
			final Song song = createSong(musicFile);
			songs.add(song);
		}
		album.setAlbumSongs(EFactory.createAlbumSongs(songs, album));

		logger.info("Finish album ------------------------------------");
		return album;
	}

	private static Song createSong(MusicFile musicFile) {
		logger.info("\tMusic file ------------------------------------------");
		final Song song = new Song();

		logger.info("\t\tName:\t" + musicFile.getName());
		song.setName(musicFile.getName());

		logger.info("\t\tTitle:\t" + musicFile.getTitle());
		song.setTitle(musicFile.getTitle());

		logger.info("\t\tArtists: " + musicFile.getArtists());
		song.setArtistSongs(EFactory.createArtistSongs(createArtistSet(musicFile.getArtists()), song));

		logger.info("\t\tAuthors: " + musicFile.getAuthor());
		song.setAuthor(createAuthor(musicFile.getAuthor()));

		return song;
	}

	private static void setAlbumAttribute(net.xapxinh.dataservice.parser.Album xmlAlbum, Album album) {
		logger.info("\tTitle:\t\t " + xmlAlbum.getTitle());
		album.setTitle(xmlAlbum.getTitle());

		logger.info("\tRelease date:\t" + xmlAlbum.getReleaseDate());
		album.setReleaseDate(xmlAlbum.getReleaseDate());

		logger.info("\tDescription:\t" + xmlAlbum.getDescription());
		album.setDescription(xmlAlbum.getDescription());

		logger.info("\tArtists:\t" + xmlAlbum.getArtists());
		album.setArtistAlbums(EFactory.createArtistAlbums(createArtistSet(xmlAlbum.getArtists()), album));

		logger.info("\tAuthors:\t" + xmlAlbum.getAuthors());
		album.setAuthorAlbums(EFactory.createAuthorAlbums(createAuthorSet(xmlAlbum.getAuthors()), album));
	}

	private static Author createAuthor(String authorName) {
		if (isName(authorName)) {
			return null;
		}
		final Author author = new Author();
		author.setName(authorName.trim());
		return author;
	}

	private static Set<Author> createAuthorSet(String authorNames) {
		if (isName(authorNames)) {
			return null;
		}
		final String[] authorNameArr = authorNames.split(",");
		final Set<Author> authors = new LinkedHashSet<Author>();
		for (final String authorName : authorNameArr) {
			authors.add(createAuthor(authorName));
		}
		return authors;
	}

	private static Set<Artist> createArtistSet(String artistNames) {
		if (isName(artistNames)) {
			return null;
		}
		final String[] artistNameArr = artistNames.split(",");
		final Set<Artist> artists = new LinkedHashSet<Artist>();
		for (final String artistName : artistNameArr) {
			artists.add(createArtist(artistName));
		}
		return artists;
	}

	private static Artist createArtist(String artistName) {
		if (isName(artistName)) {
			return null;
		}
		final Artist artist = new Artist();
		artist.setName(artistName.trim());
		return artist;
	}

	private static boolean isName(String name) {
		return name == null || name.isEmpty() || name.equals("null");
	}
}
