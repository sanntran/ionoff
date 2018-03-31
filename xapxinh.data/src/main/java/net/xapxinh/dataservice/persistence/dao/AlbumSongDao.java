package net.xapxinh.dataservice.persistence.dao;

import net.xapxinh.dataservice.entity.AlbumSong;

import org.springframework.transaction.annotation.Transactional;

@Transactional
public interface AlbumSongDao extends IGenericDao<AlbumSong> {
}
