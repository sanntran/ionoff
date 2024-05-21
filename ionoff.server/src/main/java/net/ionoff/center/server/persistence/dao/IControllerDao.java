package net.ionoff.center.server.persistence.dao;

import java.util.List;
import java.util.Optional;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Controller;

@Transactional
public interface IControllerDao extends IGenericDao<Controller> {

    long countByProjectId(long projectId);

    List<Controller> findByIsLazy();

    List<Controller> findByProjectId(long projectId);

	List<Controller> findByIpPort(String ip, Integer port);

    Optional<Controller> findByKey(String mac);

	List<Controller> findByIp(String ip);

    List<Controller> findOfflineInProject(long projectId);
}
