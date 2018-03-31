package net.xapxinh.dataservice.persistence.dao;

import net.xapxinh.dataservice.entity.ArtistSong;

import org.springframework.transaction.annotation.Transactional;

@Transactional
public interface ArtistSongDao extends IGenericDao<ArtistSong> {

	ArtistSong findByIds(Long artistId, Long songId);
}
