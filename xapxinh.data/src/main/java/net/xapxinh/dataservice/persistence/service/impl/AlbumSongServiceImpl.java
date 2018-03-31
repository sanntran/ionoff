package net.xapxinh.dataservice.persistence.service.impl;

import org.springframework.transaction.annotation.Transactional;

import net.xapxinh.dataservice.entity.AlbumSong;
import net.xapxinh.dataservice.persistence.dao.AlbumSongDao;
import net.xapxinh.dataservice.persistence.service.AlbumSongService;

@Transactional
public class AlbumSongServiceImpl extends AbstractGenericService<AlbumSong> implements AlbumSongService {

	private AlbumSongDao albumSongDao;
	
	public AlbumSongServiceImpl(AlbumSongDao albumSongDao) {
		this.albumSongDao = albumSongDao;
	}

	@Override
	protected AlbumSongDao getDao() {
		return albumSongDao;
	}
}
