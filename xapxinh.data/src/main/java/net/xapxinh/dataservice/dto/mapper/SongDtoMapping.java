package net.xapxinh.dataservice.dto.mapper;

import net.xapxinh.dataservice.dto.SongDto;
import net.xapxinh.dataservice.entity.Song;

public final class SongDtoMapping {

	public static SongDto toSongDto(Song song, String folderUrl, boolean isLossless) {
		final SongDto songDto = new SongDto();
		songDto.setId(song.getId());
		songDto.setTitle(song.getTitle());
		songDto.setArtists(song.getArtistNames());
		songDto.setAuthor(song.getAuthorName());
		if (isLossless) {
			songDto.setName(song.getName());
		}
		else {
			songDto.setName(song.getName().substring(0, song.getName().lastIndexOf(".")) + ".mp3");
		}
		
		songDto.setFolderUrl(folderUrl);
		return songDto;
	}
}
