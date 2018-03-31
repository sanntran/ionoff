package net.ionoff.center.server.persistence.service.impl;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.entity.Version;
import net.ionoff.center.server.persistence.dao.IVersionDao;
import net.ionoff.center.server.persistence.service.IVersionService;
import net.ionoff.center.shared.dto.VersionDto;

@Transactional
public class VersionServiceImpl extends AbstractGenericService<Version, VersionDto> implements IVersionService {

	private IVersionDao sessionDao;
	
	public VersionServiceImpl(IVersionDao sessionDao) {
		this.sessionDao = sessionDao;
	}

	@Override
	protected IVersionDao getDao() {
		return sessionDao;
	}

	@Override
	public VersionDto requireDtoById(long id) {
		throw new UnsupportedOperationException();
	}

	@Override
	public VersionDto insertDto(User user, VersionDto dto) {
		throw new UnsupportedOperationException();
	}

	@Override
	public VersionDto updateDto(User user, VersionDto dto) {
		throw new UnsupportedOperationException();
	}

	@Override
	public void deleteDtoById(User user, long id) {
		throw new UnsupportedOperationException();
	}

	@Override
	protected List<VersionDto> createDtoList(List<Version> entities) {
		throw new UnsupportedOperationException();
	}
}
