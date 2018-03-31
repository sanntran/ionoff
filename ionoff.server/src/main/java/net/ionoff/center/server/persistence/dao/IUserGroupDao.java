package net.ionoff.center.server.persistence.dao;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.UserGroup;

@Transactional
public interface IUserGroupDao extends IGenericDao<UserGroup> {

	UserGroup findByName(String name);
	
}
