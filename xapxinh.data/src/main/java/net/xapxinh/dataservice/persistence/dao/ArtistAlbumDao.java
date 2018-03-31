package net.xapxinh.dataservice.persistence.dao;

import net.xapxinh.dataservice.entity.ArtistAlbum;

import org.springframework.transaction.annotation.Transactional;

@Transactional
public interface ArtistAlbumDao extends IGenericDao<ArtistAlbum> {
}
