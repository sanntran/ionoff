package net.ionoff.center.server.persistence.service;

import net.ionoff.center.server.entity.Controller;
import net.ionoff.center.server.entity.Sensor;
import net.ionoff.center.shared.dto.ControllerDto;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Transactional
public interface IControllerService extends IGenericService<Controller, ControllerDto> {

    List<Controller> findByIsLazy();

    List<Controller> findByProjectId(long projectId);

	List<Controller> findByMac(String mac);

	List<Controller> findByIp(String ip);

	List<ControllerDto> findDtoByProjectId(Long projectId);
	
	void insertSensors(Controller controller);
	
	Sensor updateSensor(Sensor sensor);

}
