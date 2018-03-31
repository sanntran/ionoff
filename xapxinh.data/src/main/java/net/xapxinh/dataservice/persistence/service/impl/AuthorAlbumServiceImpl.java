package net.xapxinh.dataservice.persistence.service.impl;

import org.springframework.transaction.annotation.Transactional;

import net.xapxinh.dataservice.entity.AuthorAlbum;
import net.xapxinh.dataservice.persistence.dao.AuthorAlbumDao;
import net.xapxinh.dataservice.persistence.service.AuthorAlbumService;

@Transactional
public class AuthorAlbumServiceImpl extends AbstractGenericService<AuthorAlbum> implements AuthorAlbumService {

	private AuthorAlbumDao authorAlbumDao;
	
	public AuthorAlbumServiceImpl(AuthorAlbumDao authorAlbumDao) {
		this.authorAlbumDao = authorAlbumDao;
	}

	@Override
	protected AuthorAlbumDao getDao() {
		return authorAlbumDao;
	}
}
