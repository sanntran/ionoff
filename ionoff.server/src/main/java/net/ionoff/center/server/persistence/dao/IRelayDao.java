package net.ionoff.center.server.persistence.dao;

import java.util.List;

import net.ionoff.center.server.entity.Relay;

import org.springframework.transaction.annotation.Transactional;

@Transactional
public interface IRelayDao extends IGenericDao<Relay> {
	
	List<Relay> findByProjectId(long projectId);
	List<Relay> findByRelayDriverId(long relayDriverId);
	List<Relay> findByDeviceId(long deviceId);
}
