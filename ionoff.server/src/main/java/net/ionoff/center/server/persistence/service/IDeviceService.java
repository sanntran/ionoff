package net.ionoff.center.server.persistence.service;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Device;
import net.ionoff.center.server.entity.Player;
import net.ionoff.center.server.entity.Sensor;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.entity.WeighScale;
import net.ionoff.center.server.entity.Zone;
import net.ionoff.center.shared.dto.DeviceDto;
import net.ionoff.center.shared.dto.StatusDto;
import net.xapxinh.center.server.exception.UnknownPlayerException;

@Transactional
public interface IDeviceService extends IGenericService<Device, DeviceDto> {		

	void update(Device device, Long zoneId);
	
	Player findPlayerByMac(String mac);

	WeighScale findWeighScaleByMac(String mac);
	
	void moveDevice(Device device, Zone fromZone, Zone toZone);

	List<DeviceDto> findDtoByUserZoneId(User user, long zoneId);
	
	List<DeviceDto> findDtoByUserProjectId(User user, long projectId);
	
	List<StatusDto> getStatusByZoneId(User user, Long zoneId);

	List<StatusDto> getStatusByProjectId(User user, Long projectId);
	
	net.xapxinh.center.server.entity.Player getPlayer(Long playerId) throws UnknownPlayerException;

	void updateSensorStatus(Sensor sensor);

	void onSensorStatusChanged(Sensor sensor);

}
