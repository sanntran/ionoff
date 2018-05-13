package net.ionoff.center.server.persistence.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.ModeSensor;
import net.ionoff.center.server.entity.ModeSensorUser;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.objmapper.ModeMapper;
import net.ionoff.center.server.persistence.dao.IModeSensorUserDao;
import net.ionoff.center.server.persistence.service.IModeSensorService;
import net.ionoff.center.server.persistence.service.IModeSensorUserService;
import net.ionoff.center.shared.dto.ModeSensorUserDto;

@Transactional
public class ModeSensorUserServiceImpl extends AbstractGenericService<ModeSensorUser, ModeSensorUserDto> implements IModeSensorUserService {

	@Autowired
	private ModeMapper modeMapper;
	
	@Autowired
	private IModeSensorService modeSensorService;
	
	private IModeSensorUserDao modeSensorUserDao;
	
	public ModeSensorUserServiceImpl(IModeSensorUserDao modeSensorUserDao) {
		this.modeSensorUserDao = modeSensorUserDao;
	}

	@Override
	protected IModeSensorUserDao getDao() {
		return modeSensorUserDao;
	}

	@Override
	public ModeSensorUserDto insertDto(User user, ModeSensorUserDto dto) {
		throw new UnsupportedOperationException();
	}

	@Override
	public ModeSensorUserDto updateDto(User user, ModeSensorUserDto dto) {

		final ModeSensorUser modeSensorUser = requireById(dto.getId());
		modeSensorUser.setSendSms(dto.getSendSms());
		modeSensorUser.setSendEmail(dto.getSendEmail());
		update(modeSensorUser);
		return modeMapper.createModeSensorUserDto(modeSensorUser);
	}

	@Override
	public ModeSensorUserDto requireDtoById(long id) {
		return modeMapper.createModeSensorUserDto(requireById(id));
	}

	@Override
	public List<ModeSensorUserDto> findByModeSensorId(Long modeSensorId) {
		final ModeSensor modeSensor = modeSensorService.requireById(modeSensorId);
		final List<ModeSensorUserDto> modeSensorUserDtos = new ArrayList<ModeSensorUserDto>();
		for (final ModeSensorUser modeSensorUser : modeSensor.getUsers()) {
			modeSensorUserDtos.add(modeMapper.createModeSensorUserDto(modeSensorUser));
		}
		return modeSensorUserDtos;
	}

	@Override
	public void deleteDtoById(User user, long id) {
		throw new UnsupportedOperationException();
	}

	@Override
	protected List<ModeSensorUserDto> createDtoList(List<ModeSensorUser> entities) {
		throw new UnsupportedOperationException();
	}
}
