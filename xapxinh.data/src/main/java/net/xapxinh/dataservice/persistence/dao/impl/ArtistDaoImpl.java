package net.xapxinh.dataservice.persistence.dao.impl;

import java.util.List;

import net.xapxinh.dataservice.entity.Artist;
import net.xapxinh.dataservice.persistence.dao.ArtistDao;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

@Transactional
public class ArtistDaoImpl extends AbstractGenericDao<Artist> implements ArtistDao {

	protected ArtistDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(Artist.class);
	}

	@Override
	public Artist findByName(String name) {
		String sql = "select distinct artist "
			 		+ "from Artist as artist " 
			 		+ "where artist.name = :name ";
	
		Query query = getCurrentSession().createQuery(sql)
					.setParameter("name", name);
		
		// hibernate does not care about accent, so we have to check accent here 
		List<Artist> artists = findMany(query);
		if (artists == null) { // 
			return null;
		}
		for (Artist artist : artists) {
			if (artist.getName().equals(name)) {
				return artist;
			}
		}
		return null;
	}
}
