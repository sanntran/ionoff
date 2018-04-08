package net.ionoff.center.server.persistence.dao;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Device;
import net.ionoff.center.server.entity.Player;
import net.ionoff.center.server.entity.WeighScale;

@Transactional
public interface IDeviceDao extends IGenericDao<Device> {

	Player findPlayerByMac(String key);

	WeighScale findWeighScaleByMac(String mac);
	
	long countByProjectId(long projectId);

	List<Device> findByZoneId(long userId, long zoneId);
	
	List<Device> findByUserProjectId(long userId, long projectId);
}
