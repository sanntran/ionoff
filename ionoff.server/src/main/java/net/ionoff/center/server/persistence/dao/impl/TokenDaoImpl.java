package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import javax.persistence.Query;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.Token;
import net.ionoff.center.server.persistence.dao.ITokenDao;

@Repository
@Transactional
public class TokenDaoImpl extends AbstractGenericDao<Token> implements ITokenDao {

	public TokenDaoImpl() {
		super();
		setClass(Token.class);
	}

	@Override
	public Token findByToken(String tokenString) {
		final String sql = "select token"
				+ " from Token as token"
				+ " where token.token = :tokenString";
		final Query query = entityManager.createQuery(sql)
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