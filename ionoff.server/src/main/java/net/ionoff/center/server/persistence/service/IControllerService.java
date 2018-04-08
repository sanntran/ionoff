package net.ionoff.center.server.persistence.service;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Controller;
import net.ionoff.center.server.entity.Switch;
import net.ionoff.center.shared.dto.ControllerDto;

@Transactional
public interface IControllerService extends IGenericService<Controller, ControllerDto> {

	List<Controller> findByProjectId(long projectId);

	List<Controller> findByMac(String mac);

	List<Controller> findByIp(String ip);

	List<ControllerDto> findDtoByProjectId(Long projectId);
	
	void insertSwitches(Controller controller);
	
	Switch updateSwitch(Switch zwitch);
}
