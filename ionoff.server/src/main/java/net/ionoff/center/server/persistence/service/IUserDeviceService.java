package net.ionoff.center.server.persistence.service;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.UserDevice;
import net.ionoff.center.shared.dto.UserDeviceDto;

@Transactional
public interface IUserDeviceService extends IGenericService<UserDevice, UserDeviceDto> {

	List<UserDevice> findByUserProject(Long userId, Long projectId);

	List<UserDeviceDto> findDtoByUserProject(long userId, long projectId);
	
	List<UserDevice> findByUserZone(Long userId, Long zoneId);

	List<UserDeviceDto> findDtoByUserZone(long userId, long zoneId);
}
