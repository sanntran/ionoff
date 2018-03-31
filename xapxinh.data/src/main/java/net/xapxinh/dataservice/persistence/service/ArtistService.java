package net.xapxinh.dataservice.persistence.service;

import net.xapxinh.dataservice.entity.Artist;

import org.springframework.transaction.annotation.Transactional;

@Transactional
public interface ArtistService extends IGenericService<Artist> {

	public Artist findByName(String name);
}
