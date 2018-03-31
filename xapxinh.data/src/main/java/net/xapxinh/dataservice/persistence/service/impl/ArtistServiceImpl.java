package net.xapxinh.dataservice.persistence.service.impl;

import org.springframework.transaction.annotation.Transactional;

import net.xapxinh.dataservice.entity.Artist;
import net.xapxinh.dataservice.persistence.dao.ArtistDao;
import net.xapxinh.dataservice.persistence.service.ArtistService;

@Transactional
public class ArtistServiceImpl extends AbstractGenericService<Artist> implements ArtistService {

	private ArtistDao artistDao;
	
	public ArtistServiceImpl(ArtistDao artistDao) {
		this.artistDao = artistDao;
	}

	@Override
	protected ArtistDao getDao() {
		return artistDao;
	}

	@Override
	public Artist findByName(String name) {
		return getDao().findByName(name);
	}
}
