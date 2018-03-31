package net.xapxinh.dataservice.dto.mapper;

import java.util.ArrayList;
import java.util.List;

import net.xapxinh.dataservice.config.DataServiceConfig;
import net.xapxinh.dataservice.dto.AlbumDto;
import net.xapxinh.dataservice.dto.SongDto;
import net.xapxinh.dataservice.entity.Album;
import net.xapxinh.dataservice.entity.Artist;
import net.xapxinh.dataservice.entity.Author;
import net.xapxinh.dataservice.entity.Song;

public final class AlbumDtoMapping {

	private static final String DLM = ", ";

	public static AlbumDto toAlbumDto(Album album, String baseUrl, String mac) {
		if (album == null) {
			return null;
		}
		final AlbumDto albumDto = new AlbumDto();
		albumDto.setId(album.getId());
		albumDto.setName(album.getName());

		albumDto.setTitle(album.getTitle());
		if (album.getArtists() != null && !album.getArtists().isEmpty()) {
			final StringBuilder strBuilder = new StringBuilder();
			for (final Artist artist : album.getArtists()) {
				strBuilder.append(DLM + artist.getName());
			}
			albumDto.setArtist(strBuilder.toString().replaceFirst(DLM, ""));
		}
		if (album.getAuthors() != null && !album.getAuthors().isEmpty()) {
			final StringBuilder strBuilder = new StringBuilder();
			for (final Author author : album.getAuthors()) {
				strBuilder.append(DLM + author.getName());
			}
			albumDto.setAuthor(strBuilder.toString().replaceFirst(DLM, ""));
		}
		albumDto.setImage(getAlbumImageUrl(album.getName()));
		albumDto.setReleaseDate(album.getReleaseDate());
		albumDto.setListenCount(album.getListenCount());
		albumDto.setSongs(new ArrayList<SongDto>());
		return albumDto;
	}

	public static AlbumDto toAlbumDtoIncludingSongs(Album album, String baseUrl, String mac) {
		final AlbumDto albumDto = toAlbumDto(album, baseUrl, mac);
		if (albumDto == null) {
			return null;
		}
		albumDto.setSongs(toSongDtos(album.getSongs(), album.getName()));
		return albumDto;
	}

	private static List<SongDto> toSongDtos(List<Song> songs, String albumName) {
		if (songs == null) {
			return null;
		}
		final List<SongDto> songDtos = new ArrayList<SongDto>();
		for (final Song song : songs) {
			songDtos.add(SongDtoMapping.toSongDto(song, getAlbumFolderUrl(albumName), DataServiceConfig.getInstance().FILESERVICE_IS_LOSSLESS));
		}
		return songDtos;
	}

	/**
	 *
	 * @param albumName Name (folder) of album
	 * @return Url of album image
	 */
	public static String getAlbumImageUrl(String albumName) {
		final StringBuilder strBuilder = new StringBuilder();
		strBuilder.append(DataServiceConfig.getInstance().FILESERVICE_URL);
		strBuilder.append(DataServiceConfig.getInstance().FILE_SERVICE_MUSIC);
		strBuilder.append("/").append(albumName).append("/album.jpg");
		return strBuilder.toString();
	}

	/**
	 *
	 * @param albumName Name (folder) of album
	 * @return Url of album image
	 */
	public static String getAlbumFolderUrl(String albumName) {
		final StringBuilder strBuilder = new StringBuilder();
		strBuilder.append(DataServiceConfig.getInstance().FILESERVICE_URL);
		strBuilder.append(DataServiceConfig.getInstance().FILE_SERVICE_MUSIC);
		strBuilder.append("/").append(albumName);
		return strBuilder.toString();
	}
}
