package net.ionoff.center.server.persistence.service.impl;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.UserGroup;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.persistence.dao.IUserGroupDao;
import net.ionoff.center.server.persistence.service.IUserGroupService;
import net.ionoff.center.shared.dto.UserGroupDto;

@Transactional
public class UserGroupServiceImpl extends AbstractGenericService<UserGroup, UserGroupDto> implements IUserGroupService {

	private IUserGroupDao groupDao;
	
	public UserGroupServiceImpl(IUserGroupDao groupDao) {
		this.groupDao = groupDao;
	}

	@Override
	protected IUserGroupDao getDao() {
		return groupDao;
	}

	@Override
	public UserGroup findByName(String name) {
		return getDao().findByName(name);
	}

	@Override
	public UserGroupDto insertDto(User user, UserGroupDto dto) {
		throw new UnsupportedOperationException();
	}

	@Override
	public UserGroupDto updateDto(User user, UserGroupDto dto) {
		throw new UnsupportedOperationException();
	}

	@Override
	public UserGroupDto requireDtoById(long id) {
		throw new UnsupportedOperationException();
	}

	@Override
	public void deleteDtoById(User user, long id) {
		throw new UnsupportedOperationException();
	}

	@Override
	protected List<UserGroupDto> createDtoList(List<UserGroup> entities) {
		throw new UnsupportedOperationException();
	}
}
