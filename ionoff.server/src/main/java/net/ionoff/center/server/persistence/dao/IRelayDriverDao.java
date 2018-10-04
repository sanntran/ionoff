package net.ionoff.center.server.persistence.dao;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.RelayDriver;

@Transactional
public interface IRelayDriverDao extends IGenericDao<RelayDriver> {

	List<RelayDriver> findByProjectId(long projectId);

	List<RelayDriver> findByIpPort(String ip, Integer port);

	List<RelayDriver> findByMac(String mac);

	List<RelayDriver> findByIp(String ip);

}
