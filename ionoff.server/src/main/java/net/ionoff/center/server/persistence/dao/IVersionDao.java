package net.ionoff.center.server.persistence.dao;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Version;

@Transactional
public interface IVersionDao extends IGenericDao<Version> {

}
