package net.ionoff.center.server.persistence.service.impl;

import java.util.List;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Project;
import net.ionoff.center.server.entity.Token;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.entity.UserProject;
import net.ionoff.center.server.exception.DeleteEntityException;
import net.ionoff.center.server.exception.UpdateEntityException;
import net.ionoff.center.server.locale.Messages;
import net.ionoff.center.server.persistence.mapper.UserMapper;
import net.ionoff.center.server.persistence.dao.IDeviceDao;
import net.ionoff.center.server.persistence.dao.ITokenDao;
import net.ionoff.center.server.persistence.dao.IUserDao;
import net.ionoff.center.server.persistence.service.IProjectService;
import net.ionoff.center.server.persistence.service.IUserProjectService;
import net.ionoff.center.server.persistence.service.IUserService;
import net.ionoff.center.shared.dto.UserDto;

@Service ("userService")
@Transactional
public class UserServiceImpl extends AbstractGenericService<User, UserDto> implements IUserService {
	
	private IUserDao userDao;
	
	@Autowired
	private ITokenDao tokenDao;
	@Autowired
	private IDeviceDao deviceDao;
	@Autowired
	private IProjectService projectService;
	@Autowired
	private IUserProjectService userProjectService;
	@Autowired
	private UserMapper userMapper;

	@Autowired
	public UserServiceImpl(IUserDao userDao) {
		this.userDao = userDao;
	}

	@Override
	protected IUserDao getDao() {
		return userDao;
	}
	
	protected IDeviceDao getDeviceDao() {
		return deviceDao;
	}
	
	@Override
	public User insert(User user) {
		userDao.insert(user);
		insertUserProjects(user);
		return user;
	}	

	private void insertUserProjects(User user) {
		for (Project proj : projectService.loadAll()) {
			UserProject userProject = new UserProject();
			userProject.setUser(user);
			userProject.setProject(proj);
			userProject.setRole(false);
			userProjectService.insert(userProject);
		}
	}

	@Override
	public User findByName(String userName) {
		return getDao().findByName(userName);
	}

	@Override
	public List<User> findByGroupId(int groupId) {
		return getDao().findByGroupId(groupId);
	}

	@Override
	public List<User> findByProjectId(long projectId) {
		return getDao().findByProjectId(projectId);
	}
	
	@Override
	public User findByToken(String tokenString) {
		Token accessToken = tokenDao.findByToken(tokenString);
        if (null == accessToken) {
            return null;
        }
        if (accessToken.isExpired()) {
        	tokenDao.delete(accessToken);
            return null;
        }
        return accessToken.getUser();
	}

	@Override
	public Token createToken(User user) {
		 Token accessToken = new Token();
		 accessToken.setUser(user);
		 accessToken.setValue(UUID.randomUUID().toString());
	     return tokenDao.insert(accessToken);
	}

	@Override
	public UserDetails loadUserByUsername(String userName) throws UsernameNotFoundException {
		return userDao.findByName(userName);
	}

	@Override
	public UserDto requireDtoById(long id) {
		throw new UnsupportedOperationException();
	}

	@Override
	public UserDto insertDto(User user, UserDto dto) {
		validateUser(dto, user.getLanguage());
		User newUser = userMapper.createUser(dto);
		insert(newUser);
		return userMapper.createUserDto(newUser);
	}

	@Override
	public UserDto updateDto(User user, UserDto dto) {
		validateUser(dto, user.getLanguage());
		User updateUser = requireById(dto.getId());
		userMapper.updateUser(updateUser, dto);
		update(updateUser);
		return userMapper.createUserDto(updateUser);
	}

	@Override
	public void deleteDtoById(User user, long id) {
		User usr = findById(id);
		if (User.LORD.equals(usr.getName())) {
			throw new DeleteEntityException(Messages.get(user.getLanguage()).errorDeleteUserLord());
		}
		if (user.isSystemAdmin()) {
			delete(usr);
			return;
		}
		int projs = 0;
		for (UserProject usrProj : usr.getProjects()) {
			if (usrProj.hasRole()) {
				projs ++;
			}
		}
		if (projs < 2) {
			delete(usr);
			return;
		}
		throw new AccessDeniedException(Messages.get(user.getLanguage()).errorUserIsNotSystemAdmin());
	}

	@Override
	public UserDto insertDto(User user, UserDto userDto, Long projectId) {
		validateUser(userDto, user.getLanguage());
		User newUser = userMapper.createUser(userDto);
		insert(newUser);
		grantProjectPermission(newUser, projectId);
		return userMapper.createUserDto(newUser);
	}

	private void grantProjectPermission(User user, Long projectId) {
		for (UserProject userProject : userProjectService.findByUserId(user.getId())) {
			if (userProject.getProject().getId() == projectId.longValue()) {
				userProject.setRole(true);
				userProjectService.updateRole(userProject);
			}
		}
	}

	private void validateUser(UserDto userDto, String locale) throws UpdateEntityException {
		final User lord = findByName(User.LORD);
		if (lord != null && lord.getId() == userDto.getId()) {
			if (!lord.getName().equals(userDto.getName())) {
				// update lord user name
				throw new UpdateEntityException(Messages.get(locale).errorChangeSuperAdminUserName(User.LORD));
			}
		}
		final User user = findByName(userDto.getName());
		if (user != null && user.getId() != userDto.getId()) {
			throw new UpdateEntityException(Messages.get(locale).userNameIsDuplicated(userDto.getName()));
		}
	}

	@Override
	public void deleteDtoById(User user, Long id, Long projectId) {
		final List<User> users = findByProjectId(projectId);
		if (users.size() < 2) {
			throw new DeleteEntityException(Messages.get(user.getLanguage()).errorDeleteSingleEntity());
		}
		User u = findById(id);
		if (User.LORD.equals(u.getName())) {
			throw new DeleteEntityException(Messages.get(user.getLanguage()).errorDeleteUserLord());
		}
		delete(u);
	}

	@Override
	public List<UserDto> findDtoByProjectId(Long projectId) {
		List<User> users = findByProjectId(projectId);
		return userMapper.createUserDtoList(users);
	}

	@Override
	protected List<UserDto> createDtoList(List<User> entities) {
		return userMapper.createUserDtoList(entities);
	}

	@Override
	public UserDto updateLanguage(User user, String language) {
		user	.setLanguage(language);
		super.update(user);
		return userMapper.createUserDto(user);
	}
	
}
