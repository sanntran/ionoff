package net.xapxinh.dataservice.persistence.service;

import java.util.List;
import java.util.Map;

import net.xapxinh.dataservice.entity.Album;

import org.springframework.transaction.annotation.Transactional;

@Transactional
public interface AlbumService extends IGenericService<Album> {
	
	public Album findByName(String name);
	
	public Album findByTitle(String title);
	
	public List<Album> getMostListenCount();
	
	public List<Album> getLatestUploaded();

	public Map<Long, Integer> getListenCounts(List<Long> albumIds);
}
