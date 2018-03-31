package net.ionoff.center.server.persistence.dao;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.User;

@Transactional
public interface IUserDao extends IGenericDao<User> {

	public User findByName(String userName);

	public List<User> findByGroupId(int groupId);

	public List<User> findByProjectId(long projectId);

}
