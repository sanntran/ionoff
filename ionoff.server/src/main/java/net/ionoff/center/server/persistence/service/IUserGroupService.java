package net.ionoff.center.server.persistence.service;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.UserGroup;
import net.ionoff.center.shared.dto.UserGroupDto;

@Transactional
public interface IUserGroupService extends IGenericService<UserGroup, UserGroupDto> {

	UserGroup findByName(String name);
}
