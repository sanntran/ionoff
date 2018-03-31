package net.ionoff.center.server.persistence.dao;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Token;

@Transactional
public interface ITokenDao extends IGenericDao<Token> {

	Token findByToken(String tokenString);
}
