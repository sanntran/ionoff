package net.xapxinh.dataservice.persistence.dao.impl;

import net.xapxinh.dataservice.entity.ArtistSong;
import net.xapxinh.dataservice.persistence.dao.ArtistSongDao;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

@Transactional
public class ArtistSongDaoImpl extends AbstractGenericDao<ArtistSong> implements ArtistSongDao {

	protected ArtistSongDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(ArtistSong.class);
	}

	@Override
	public ArtistSong findByIds(Long artistId, Long songId) {
		
		String sql = "select distinct artistSong "
		 		+ "from ArtistSong as artistSong " 
		 		+ "where artistSong.artist.id = :artistId "
		 		+ "and artistSong.song.id = :songId ";

		Query query = getCurrentSession().createQuery(sql)
					.setParameter("artistId", artistId)
					.setParameter("songId", songId);
		
		return getFirst(findMany(query));
	}
}
