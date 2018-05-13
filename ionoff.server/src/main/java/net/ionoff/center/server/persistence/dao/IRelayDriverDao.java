package net.ionoff.center.server.persistence.dao;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.RelayDriver;
import net.ionoff.center.shared.entity.RelayDriverModel;

@Transactional
public interface IRelayDriverDao extends IGenericDao<RelayDriver> {

	List<RelayDriver> findByProjectId(long projectId);

	List<RelayDriver> findByIpPort(String ip, Integer port);

	List<RelayDriver> findByMac(String mac);

	List<RelayDriver> findByIp(String ip);

	List<RelayDriver> findByModel(RelayDriverModel model);
}
