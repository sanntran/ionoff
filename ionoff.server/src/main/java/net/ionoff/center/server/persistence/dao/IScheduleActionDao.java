package net.ionoff.center.server.persistence.dao;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.ScheduleAction;

@Transactional
public interface IScheduleActionDao extends IGenericDao<ScheduleAction> {

	void deleteByRelayId(long relayId);
}
