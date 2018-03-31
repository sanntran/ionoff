package net.xapxinh.dataservice.persistence.service;

import net.xapxinh.dataservice.entity.ArtistSong;

import org.springframework.transaction.annotation.Transactional;

@Transactional
public interface ArtistSongService extends IGenericService<ArtistSong> {

	ArtistSong findByIds(Long artistId, Long songId);

}
