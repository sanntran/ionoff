package net.xapxinh.dataservice.persistence.dao.impl;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.xapxinh.dataservice.entity.Album;
import net.xapxinh.dataservice.persistence.dao.AlbumDao;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

@Transactional
public class AlbumDaoImpl extends AbstractGenericDao<Album> implements AlbumDao {

	protected AlbumDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(Album.class);
	}

	@Override
	public Album findByName(String name) {
		final String sql = "select distinct album "
				+ "from Album as album "
				+ "where album.name = :name ";

		final Query query = getCurrentSession().createQuery(sql)
				.setParameter("name", name);

		return getFirst(findMany(query));
	}
	

	@Override
	public Album findByTitle(String title) {
		final String sql = "select distinct album "
				+ "from Album as album "
				+ "where album.title = :keyword ";
		
		final Query query = getCurrentSession().createQuery(sql)
				.setParameter("keyword", title);

		return getFirst(findMany(query));
	}

	@Override
	public List<Album> getMostListenCount() {
		final String sql = "select distinct album "
				+ "from Album as album "
				+ "order by album.listenCount desc ";

		final Query query = getCurrentSession().createQuery(sql);
		query.setMaxResults(10);

		return findMany(query);
	}

	@Override
	public List<Album> getLatestUploaded() {
		final String sql = "select distinct album "
				+ "from Album as album "
				+ "order by album.uploadedDate desc ";

		final Query query = getCurrentSession().createQuery(sql);
		query.setMaxResults(10);

		return findMany(query);
	}

	@Override
	public Map<Long, Integer> getListenCounts(List<Long> albumIds) {
		final String sql = "select album.id, album.listenCount "
				+ "from Album as album "
				+ "where album.id in :albumIds";
		Query query = getCurrentSession().createQuery(sql);
		query.setParameterList("albumIds", albumIds);
		
		@SuppressWarnings("unchecked")
		List<Object> objs = query.list();
		Map<Long, Integer> listenCountMap = new HashMap<>();
		for (Object obj : objs) {
			Object[] objArr = (Object[])obj;
			listenCountMap.put((Long)objArr[0], (Integer)objArr[1]);
		}
		return listenCountMap;
	}
}
