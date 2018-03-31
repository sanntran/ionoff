package net.xapxinh.dataservice.persistence.dao;

import net.xapxinh.dataservice.entity.Artist;

import org.springframework.transaction.annotation.Transactional;

@Transactional
public interface ArtistDao extends IGenericDao<Artist> {
	
	public Artist findByName(String name);
}
