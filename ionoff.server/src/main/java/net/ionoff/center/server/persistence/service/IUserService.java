package net.ionoff.center.server.persistence.service;

import java.util.List;

import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Token;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.shared.dto.UserDto;

@Transactional
public interface IUserService extends IGenericService<User, UserDto>, UserDetailsService {	
	
	User findByName(String userName);
	
	User findByToken(String token);

    Token createToken(User user);

	List<User> findByGroupId(int groupId);

	List<User> findByProjectId(long projectId);

	UserDto insertDto(User user, UserDto userDto, Long projectId);
	
	void deleteDtoById(User user, Long userId, Long projectId);

	List<UserDto> findDtoByProjectId(Long projectId);
}
