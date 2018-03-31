package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.Token;
import net.ionoff.center.server.persistence.dao.ITokenDao;

@Transactional
public class TokenDaoImpl extends AbstractGenericDao<Token> implements ITokenDao {

	public TokenDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(Token.class);
	}

	@Override
	public Token findByToken(String tokenString) {
		final String sql = "select token"
				+ " from Token as token"
				+ " where token.token = :tokenString";
		final Query query = getCurrentSession().createQuery(sql)
				.setParameter("tokenString", tokenString);
		
		return getFirst(findMany(query));
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<Token> findByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}
}