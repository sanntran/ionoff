package net.xapxinh.dataservice.persistence.dao;

import java.util.List;

import net.xapxinh.dataservice.entity.Song;

import org.springframework.transaction.annotation.Transactional;

@Transactional
public interface SongDao extends IGenericDao<Song> {
	
	public Song findByName(String name);

	public List<Song> findByTitle(String title);
}
