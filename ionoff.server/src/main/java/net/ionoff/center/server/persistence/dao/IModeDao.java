package net.ionoff.center.server.persistence.dao;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Mode;

@Transactional
public interface IModeDao extends IGenericDao<Mode> {
	
	List<Mode> findByProjectId(long projectId);

	List<Mode> findByScheduleTime(String hhMMAmPm);
	
}
