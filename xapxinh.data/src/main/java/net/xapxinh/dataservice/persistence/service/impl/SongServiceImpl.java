package net.xapxinh.dataservice.persistence.service.impl;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.xapxinh.dataservice.entity.Song;
import net.xapxinh.dataservice.persistence.dao.SongDao;
import net.xapxinh.dataservice.persistence.service.SongService;

@Transactional
public class SongServiceImpl extends AbstractGenericService<Song> implements SongService {

	private SongDao songDao;
	
	public SongServiceImpl(SongDao songDao) {
		this.songDao = songDao;
	}

	@Override
	protected SongDao getDao() {
		return songDao;
	}

	@Override
	public Song findByName(String name) {
		return getDao().findByName(name);
	}

	@Override
	public List<Song> findByTitle(String title) {
		return getDao().findByTitle(title);
	}
}
