package net.ionoff.center.server.persistence.dao;

import java.util.List;

import net.ionoff.center.server.entity.Controller;

import org.springframework.transaction.annotation.Transactional;

@Transactional
public interface IControllerDao extends IGenericDao<Controller> {

	List<Controller> findByProjectId(long projectId);

	List<Controller> findByIpPort(String ip, Integer port);

	List<Controller> findByMac(String mac);

	List<Controller> findByIp(String ip);
}
