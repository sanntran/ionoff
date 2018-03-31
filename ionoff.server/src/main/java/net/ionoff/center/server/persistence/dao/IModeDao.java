package net.ionoff.center.server.persistence.dao;

import java.util.List;

import net.ionoff.center.server.entity.Mode;

import org.springframework.transaction.annotation.Transactional;

@Transactional
public interface IModeDao extends IGenericDao<Mode> {
	
	List<Mode> findByProjectId(long projectId);

	List<Mode> findByScheduleTime(long projectId, String hhMMAmPm);
	
}
