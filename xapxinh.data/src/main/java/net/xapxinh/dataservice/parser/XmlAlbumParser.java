package net.xapxinh.dataservice.parser;

import java.io.File;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.log4j.Logger;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import net.xapxinh.dataservice.entity.Album;
import net.xapxinh.dataservice.entity.Artist;
import net.xapxinh.dataservice.entity.Author;
import net.xapxinh.dataservice.entity.EFactory;
import net.xapxinh.dataservice.entity.Song;

public class XmlAlbumParser {
	private static final Logger logger = Logger.getLogger(XmlAlbumParser.class.getName());

	public static Album parseXmlAlbum(File xmlAlbumFile) throws Exception {

		logger.info("Reading xml album file: " + xmlAlbumFile.getAbsolutePath());

		final DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
		final DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
		final Document doc = dBuilder.parse(xmlAlbumFile);

		// optional, but recommended
		// read http://stackoverflow.com/questions/13786607/normalization-in-dom-parsing-with-java-how-does-it-work
		doc.getDocumentElement().normalize();
		final Element albumElement = doc.getDocumentElement();
		return createAlbum(albumElement);
	}

	private static Album createAlbum(Element albumElement) {
		final Album album = new Album();
		logger.info("Album ------------------------------------");

		setAlbumAttribute(albumElement, album);

		final NodeList songsNodeList = albumElement.getElementsByTagName("musicFiles");
		final Node songsNode = songsNodeList.item(0);

		final NodeList songNodeList = songsNode.getChildNodes();
		final List<Song> songs = new ArrayList<Song>();

		final Set<Album> albums = new LinkedHashSet<Album>();
		albums.add(album);
		for (int i = 0; i < songNodeList.getLength(); i++) {
			final Node songNode = songNodeList.item(i);
			if (songNode.getNodeType() == Node.ELEMENT_NODE) {
				final Song song = createSong(songNode, albums);
				songs.add(song);
			}
		}
		album.setAlbumSongs(EFactory.createAlbumSongs(songs, album));

		logger.info("Finish album ------------------------------------");
		return album;
	}

	private static Song createSong(Node songNode, Set<Album> albums) {
		logger.info("\tMusic file ------------------------------------------");
		final Song song = new Song();

		final Element element = (Element) songNode;

		final String name = element.getAttribute("name");
		logger.info("\t\tName:\t" + name);
		song.setName(name.trim());

		final String title = element.getAttribute("title");
		logger.info("\t\tTitle:\t" + title);
		song.setTitle(title.trim());

		final String artists = element.getAttribute("artists");
		logger.info("\t\tArtists: " + artists.trim());
		song.setArtistSongs(EFactory.createArtistSongs(createArtistSet(artists), song));

		final String authors = element.getAttribute("author");
		logger.info("\t\tAuthors: " + authors.trim());
		song.setAuthor(createAuthor(authors));

		return song;
	}

	private static void setAlbumAttribute(Element albumElement, Album album) {
		final String albumTitle = albumElement.getElementsByTagName("title").item(0).getNodeValue();
		logger.info("\tTitle:\t\t " + albumTitle.trim());
		album.setTitle(albumTitle);

		final String albumReleaseDate = albumElement.getElementsByTagName("releaseDate").item(0).getNodeValue();
		logger.info("\tRelease date:\t" + albumReleaseDate.trim());
		album.setReleaseDate(albumReleaseDate);

		final String albumDescription = albumElement.getElementsByTagName("description").item(0).getNodeValue().trim();
		logger.info("\tDescription:\t" + albumDescription);
		album.setDescription(albumDescription);

		final String albumArtists = albumElement.getElementsByTagName("artists").item(0).getNodeValue();
		logger.info("\tArtists:\t" + albumArtists.trim());
		album.setArtistAlbums(EFactory.createArtistAlbums(createArtistSet(albumArtists), album));

		final String albumAuthors = albumElement.getElementsByTagName("authors").item(0).getNodeValue();
		logger.info("\tAuthors:\t" + albumAuthors.trim());
		album.setAuthorAlbums(EFactory.createAuthorAlbums(createAuthorSet(albumAuthors), album));
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
