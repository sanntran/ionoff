package net.xapxinh.dataservice.persistence.service.impl;

import org.springframework.transaction.annotation.Transactional;

import net.xapxinh.dataservice.entity.ArtistSong;
import net.xapxinh.dataservice.persistence.dao.ArtistSongDao;
import net.xapxinh.dataservice.persistence.service.ArtistSongService;

@Transactional
public class ArtistSongServiceImpl extends AbstractGenericService<ArtistSong> implements ArtistSongService {

	private ArtistSongDao artistSongDao;
	
	public ArtistSongServiceImpl(ArtistSongDao artistSongDao) {
		this.artistSongDao = artistSongDao;
	}

	@Override
	protected ArtistSongDao getDao() {
		return artistSongDao;
	}

	@Override
	public ArtistSong findByIds(Long artistId, Long songId) {
		return getDao().findByIds(artistId, songId);
	}
}
