package net.ionoff.center.server.persistence.dao;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Device;
import net.ionoff.center.server.entity.MediaPlayer;

@Transactional
public interface IDeviceDao extends IGenericDao<Device> {

	MediaPlayer findPlayerByMac(String key);

	long countByProjectId(long projectId);

	List<Device> findByUserZoneId(long userId, long zoneId);
	
	List<Device> findByUserProjectId(long userId, long projectId);

    void updateDeviceStatus(Device device);
}
