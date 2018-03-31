package net.xapxinh.dataservice.persistence.service.impl;

import org.springframework.transaction.annotation.Transactional;

import net.xapxinh.dataservice.entity.ArtistAlbum;
import net.xapxinh.dataservice.persistence.dao.ArtistAlbumDao;
import net.xapxinh.dataservice.persistence.service.ArtistAlbumService;

@Transactional
public class ArtistAlbumServiceImpl extends AbstractGenericService<ArtistAlbum> implements ArtistAlbumService {

	private ArtistAlbumDao artistAlbumDao;
	
	public ArtistAlbumServiceImpl(ArtistAlbumDao artistAlbumDao) {
		this.artistAlbumDao = artistAlbumDao;
	}

	@Override
	protected ArtistAlbumDao getDao() {
		return artistAlbumDao;
	}
}
