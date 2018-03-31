package net.ionoff.center.server.persistence.dao;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.UserProject;

@Transactional
public interface IUserProjectDao extends IGenericDao<UserProject> {
	
	List<UserProject> findByUserId(Long userId);

}
