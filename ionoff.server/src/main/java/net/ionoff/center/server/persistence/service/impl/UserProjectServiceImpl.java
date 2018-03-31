package net.ionoff.center.server.persistence.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Dashboard;
import net.ionoff.center.server.entity.Mode;
import net.ionoff.center.server.entity.ModeSensor;
import net.ionoff.center.server.entity.ModeSensorUser;
import net.ionoff.center.server.entity.Project;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.entity.UserProject;
import net.ionoff.center.server.entity.UserZone;
import net.ionoff.center.server.entity.Zone;
import net.ionoff.center.server.objmapper.UserMapper;
import net.ionoff.center.server.persistence.dao.IModeSensorUserDao;
import net.ionoff.center.server.persistence.dao.IUserProjectDao;
import net.ionoff.center.server.persistence.service.IDashboardService;
import net.ionoff.center.server.persistence.service.IUserProjectService;
import net.ionoff.center.server.persistence.service.IUserZoneService;
import net.ionoff.center.shared.dto.UserProjectDto;

@Transactional
public class UserProjectServiceImpl extends AbstractGenericService<UserProject, UserProjectDto> implements IUserProjectService {
	
	private IUserProjectDao userProjectDao;
	
	@Autowired
	private IUserZoneService userZoneService;
	
	@Autowired
	private IDashboardService dashboardService;
	
	@Autowired
	private UserMapper userMapper;
	
	@Autowired
	private IModeSensorUserDao modeSensorUserDao;
	
	public UserProjectServiceImpl(IUserProjectDao userProjectDao) {
		this.userProjectDao = userProjectDao;
	}

	@Override
	protected IUserProjectDao getDao() {
		return userProjectDao;
	}

	@Override
	public UserProject update(UserProject entity) {
		super.update(entity);
		return entity;
	}

	private void removeUserRoles(User user, Project project) {
		userZoneService.removeByUserProjectId(user.getId(), project.getId());
	}

	private void addUserRoles(User user, Project project) {
		for (Zone zone : project.getZones()) {
			UserZone userZone = new UserZone();
			userZone.setZone(zone);
			userZone.setProject(project);
			if (user.hasAdminRole()) {
				userZone.setRole(true);
			}
			else {
				userZone.setRole(false);
			}
			userZone.setUser(user);
			userZoneService.insert(userZone);
		}
	}

	@Override
	public List<UserProject> findByUserId(Long userId) {
		return userProjectDao.findByUserId(userId);
	}

	@Override
	public UserProjectDto requireDtoById(long id) {
		throw new UnsupportedOperationException();
	}

	@Override
	public UserProjectDto insertDto(User user, UserProjectDto dto) {
		throw new UnsupportedOperationException();
	}

	@Override
	public UserProjectDto updateDto(User user, UserProjectDto dto) {
		final UserProject usrPrj = requireById(dto.getId());
		usrPrj.setRole(dto.getRole());
		updateRole(usrPrj);
		return userMapper.toUserProjectDto(usrPrj);
	}

	private void removeUserDashboards(UserProject usrPrj) {
		dashboardService.removeByUserProject(usrPrj.getUser(), usrPrj.getProject().getId());
	}

	private void addUserDashboard(UserProject usrPrj) {
		Dashboard projDashboard = new Dashboard();
		projDashboard.setUser(usrPrj.getUser());
		projDashboard.setProject(usrPrj.getProject());
		
		dashboardService.insert(projDashboard);
		
		for (Zone zone : usrPrj.getProject().getZones()) {
			Dashboard zoneDashboard = new Dashboard();
			zoneDashboard.setUser(usrPrj.getUser());
			zoneDashboard.setZone(zone);
			zoneDashboard.setProject(usrPrj.getProject());
			dashboardService.insert(zoneDashboard);
		}
	}

	@Override
	public void deleteDtoById(User user, long id) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<UserProjectDto> findDtoByUserId(Long userId) {
		List<UserProjectDto> userProjectDtos = new ArrayList<>();
		for (UserProject userProject : findByUserId(userId)) {
			userProjectDtos.add(userMapper.toUserProjectDto(userProject));
		}
		return userProjectDtos;
	}

	@Override
	protected List<UserProjectDto> createDtoList(List<UserProject> entities) {
		throw new UnsupportedOperationException();
	}

	@Override
	public UserProject updateRole(UserProject userProject) {
		removeUserRoles(userProject.getUser(), userProject.getProject());
		removeUserDashboards(userProject);
		removeModeSensorUsers(userProject.getUser(), userProject.getProject());
		if (userProject.hasRole()) {
			addUserRoles(userProject.getUser(), userProject.getProject());
			addUserDashboard(userProject);
			insertModeSensorUsers(userProject.getUser(), userProject.getProject());
		}
		return update(userProject);
	}

	
	private void insertModeSensorUsers(User user, Project project) {
		if (project.hasMode()) {
			for (Mode mode : project.getModes()) {
				if (mode.hasSensor()) {
					insertModeSensorUsers(user, mode);
				}
			}
		}
	}

	private void insertModeSensorUsers(User user, Mode mode) {
		for (ModeSensor modeSensor : mode.getSensors()) {
			insertModeSensorUser(modeSensor, user, true);
			insertModeSensorUser(modeSensor, user, false);
		}
	}
	
	private void insertModeSensorUser(ModeSensor modeSensor, User user, boolean detected) {
		ModeSensorUser modeSensorUser = new ModeSensorUser();
		modeSensorUser.setDetected(detected);
		modeSensorUser.setModeSensor(modeSensor);
		modeSensorUser.setUser(user);
		modeSensorUser.setSendEmail(false);
		modeSensorUser.setSendSms(false);
		modeSensorUser.setProject(modeSensor.getMode().getProject());
		modeSensorUserDao.insert(modeSensorUser);
	}

	private void removeModeSensorUsers(User user, Project project) {
		modeSensorUserDao.removeByUserProjectId(user.getId(), project.getId());
	}
		
}
