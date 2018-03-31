package net.xapxinh.dataservice.webmvc;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;

import net.xapxinh.dataservice.AppContext;
import net.xapxinh.dataservice.config.DataServiceConfig;
import net.xapxinh.dataservice.entity.Album;
import net.xapxinh.dataservice.entity.AlbumSong;
import net.xapxinh.dataservice.entity.Artist;
import net.xapxinh.dataservice.entity.ArtistAlbum;
import net.xapxinh.dataservice.entity.ArtistSong;
import net.xapxinh.dataservice.entity.Author;
import net.xapxinh.dataservice.entity.AuthorAlbum;
import net.xapxinh.dataservice.entity.EFactory;
import net.xapxinh.dataservice.entity.Song;
import net.xapxinh.dataservice.parser.Crypto;
import net.xapxinh.dataservice.parser.XmlAlbumParser;

public class AlbumCreator {

	public static String scanAlbumFolder(HttpServletRequest httpRequest) throws Exception {
		final String folder = httpRequest.getParameter("folder");
		final File albumFolder = new File(folder);
		final File albumXmlFile = getAlbumFile(albumFolder);
		final Album album = XmlAlbumParser.parseXmlAlbum(albumXmlFile);

		validateSongNames(album, albumFolder);

		Album storedAlbum = storeAlbum(album);

		final String albumName = Crypto.getAlbumInstance().encrypt(storedAlbum.getId() + "");
		storedAlbum.setName(albumName);
		storedAlbum = AppContext.getAlbumService().update(storedAlbum);

		final String newAlbumFolder = DataServiceConfig.getInstance().DATA_FILES_MUSIC_REAL_PATH + File.separator + albumName;
		renameAlbumFolder(albumFolder, newAlbumFolder);
		renameSongs(storedAlbum.getId(), newAlbumFolder, albumName);

		return DataServiceMesage.getSuccessMessageJSON();
	}

	private static void validateSongNames(Album album, File albumFolder) throws FileNotFoundException {
		final List<Song> songs = album.getSongs();
		final File[] files = albumFolder.listFiles();

		for (final Song song : songs) {
			getFileByName(song.getName(), files);
		}
	}

	private static void renameSongs(Long albumId, String newAlbumFolder, String albumName) throws Exception {
		final Album album = AppContext.getAlbumService().findById(albumId);
		final List<Song> songs = album.getSongs();

		final File albumFolder = new File(newAlbumFolder);
		final File[] files = albumFolder.listFiles();

		for (final Song song : songs) {
			final File file = getFileByName(song.getName(), files);


			final String newFileName = Crypto.getSongInstance()
								.encrypt(song.getId() + "")
								+ "." +  getFileExt(file);

			final String toNewFileName = newAlbumFolder + File.separator + newFileName;
			file.renameTo(new File(toNewFileName));

			song.setName(newFileName);
			song.setFolder(albumName);
			AppContext.getSongService().update(song);
		}
	}

	private static String getFileNameWithOutExt(File file) {
		return getFileNameWithOutExt(file.getName());
	}

	private static String getFileNameWithOutExt(String fileName) {
		return fileName.split("\\.")[0];
	}

	private static String getFileExt(File file) {
		return file.getName().split("\\.")[1];
	}

	private static File getFileByName(String name, File[] files) throws FileNotFoundException {
		for (final File file : files) {
			if (file.getName().equals(name) ||
					getFileNameWithOutExt(file).startsWith(getFileNameWithOutExt(name))) {
				return file;
			}
		}
		throw new FileNotFoundException("File name " + name + " is not found");
	}

	private static void renameAlbumFolder(File albumFolder, String toAlbumFolder) throws Exception {
		final boolean renamed = albumFolder.renameTo(new File(toAlbumFolder));
		if (!renamed) {
			throw new Exception("Cannot rename album to: " + toAlbumFolder);
		}
	}

	private static Album storeAlbum(Album album) throws Exception {
		final Album newAlbum = new Album();
		
		newAlbum.setTitle(album.getTitle());
		newAlbum.setDescription(album.getDescription());
		newAlbum.setReleaseDate(album.getReleaseDate());
		newAlbum.setUploadedDate(new Date());

		Album storedAlbum = AppContext.getAlbumService().findByTitle(newAlbum.getTitle());
		if (storedAlbum != null) {
			throw new Exception("Album " + newAlbum.getTitle() + " existed");
		}
		else {
			storedAlbum = AppContext.getAlbumService().insert(newAlbum);
		}

		final List<Song> storedSongs = storeSong(album.getSongs());
		final List<AlbumSong> albumSongs = EFactory.createAlbumSongs(storedSongs, storedAlbum);
		for (final AlbumSong albumSong : albumSongs) {
			AppContext.getAlbumSongService().insert(albumSong);
		}

		final Set<Author> storedAuhors = storeAuthors(album.getAuthors());
		final Set<Artist> storedArtists = storeAtists(album.getArtists());
		storeAuthorAlbums(EFactory.createAuthorAlbums(storedAuhors, storedAlbum));
		storeArtistAlbums(EFactory.createArtistAlbums(storedArtists, storedAlbum));

		storedAlbum.setAlbumSongs(albumSongs);
		return storedAlbum;
	}

	private static void storeAuthorAlbums(Set<AuthorAlbum> authorAlbums) {
		if (authorAlbums == null) {
			return;
		}
		for (final AuthorAlbum authorAlbum : authorAlbums) {
			AppContext.getAuthorAlbumService().saveOrUpdate(authorAlbum);
		}
	}

	private static void storeArtistAlbums(Set<ArtistAlbum> artistAlbums) {
		if (artistAlbums == null) {
			return;
		}
		for (final ArtistAlbum artistAlbum : artistAlbums) {
			AppContext.getArtistAlbumService().saveOrUpdate(artistAlbum);
		}
	}

	private static List<Song> storeSong(List<Song> songs) {
		final List<Song> storedSongs = new ArrayList<Song>();
		for (final Song song : songs) {

			final Author storedAuthor = storeAuthor(song.getAuthor());
			song.setAuthor(storedAuthor);
			final Song storedSong = AppContext.getSongService().insert(song);
			storedSongs.add(storedSong);

			final Set<Artist> storedArtists = storeAtists(song.getArtists());
			storeArtistSongs(EFactory.createArtistSongs(storedArtists, storedSong));
		}
		return storedSongs;
	}

	private static void storeArtistSongs(Set<ArtistSong> artistSongs) {
		if (artistSongs == null) {
			return;
		}
		for (final ArtistSong artistSong : artistSongs) {
			final ArtistSong storedArtistSong = AppContext.getArtistSongService()
				.findByIds(artistSong.getArtist().getId(), artistSong.getSong().getId());
			if (storedArtistSong == null) {
				AppContext.getArtistSongService().insert(artistSong);
			}
		}
	}

	private static Set<Author> storeAuthors(Set<Author> authors) {
		final Set<Author> storedAuthors = new LinkedHashSet<Author>();

		if (authors == null) {
			return storedAuthors;
		}

		for (final Author author : authors) {
			final Author storedAuthor = storeAuthor(author);
			storedAuthors.add(storedAuthor);
		}
		return storedAuthors;
	}

	private static Author storeAuthor(Author author) {
		if (author == null) {
			return null;
		}

		final Author storedAuthor = AppContext.getAuthorService().findByName(author.getName());
		if (storedAuthor == null) {
			AppContext.getAuthorService().insert(author);
			return author;
		}
		else {
			return storedAuthor;
		}
	}

	private static Set<Artist> storeAtists(Set<Artist> artists) {
		final Set<Artist> storedArtists = new LinkedHashSet<Artist>();
		if (artists == null) {
			return storedArtists;
		}
		for (final Artist artist : artists) {
			Artist storedArtist = AppContext.getArtistService().findByName(artist.getName());
			if (storedArtist == null) {
				storedArtist = AppContext.getArtistService().insert(artist);
				storedArtists.add(storedArtist);
			}
			else {
				storedArtists.add(storedArtist);
			}
		}
		return storedArtists;
	}

	private static File getAlbumFile(File albumFolder) throws FileNotFoundException {
		final File[] files = albumFolder.listFiles();
		for (final File file : files) {
			if (file.getName().equals("_info.xml")) {
				return file;
			}
		}
		throw new FileNotFoundException("Album file is not found");
	}
}
