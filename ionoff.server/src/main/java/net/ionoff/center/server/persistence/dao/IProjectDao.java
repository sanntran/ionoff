package net.ionoff.center.server.persistence.dao;

import java.util.List;
import java.util.Optional;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Project;

@Transactional
public interface IProjectDao extends IGenericDao<Project> {

	List<Project> findByUserId(Long userId);

    Optional<Project> findFirst();
}
