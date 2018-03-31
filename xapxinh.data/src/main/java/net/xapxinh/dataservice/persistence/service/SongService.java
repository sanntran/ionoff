package net.xapxinh.dataservice.persistence.service;

import java.util.List;

import net.xapxinh.dataservice.entity.Song;

import org.springframework.transaction.annotation.Transactional;

@Transactional
public interface SongService extends IGenericService<Song> {

	public Song findByName(String name);

	public List<Song> findByTitle(String title);
}
