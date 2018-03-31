package net.xapxinh.dataservice.persistence.service.impl;

import java.util.List;
import java.util.Map;

import net.xapxinh.dataservice.entity.Album;
import net.xapxinh.dataservice.persistence.dao.AlbumDao;
import net.xapxinh.dataservice.persistence.service.AlbumService;

import org.springframework.transaction.annotation.Transactional;

@Transactional
public class AlbumServiceImpl extends AbstractGenericService<Album> implements AlbumService {

	private final AlbumDao albumDao;

	public AlbumServiceImpl(AlbumDao albumDao) {
		this.albumDao = albumDao;
	}

	@Override
	protected AlbumDao getDao() {
		return albumDao;
	}

	@Override
	public Album findByName(String name) {
		return getDao().findByName(name);
	}

	@Override
	public Album findByTitle(String title) {
		return getDao().findByTitle(title);
	}

	@Override
	public List<Album> getMostListenCount() {
		return getDao().getMostListenCount();
	}

	@Override
	public List<Album> getLatestUploaded() {
		return getDao().getLatestUploaded();
	}

	@Override
	public Map<Long, Integer> getListenCounts(List<Long> albumIds) {
		return getDao().getListenCounts(albumIds);
	}
}
