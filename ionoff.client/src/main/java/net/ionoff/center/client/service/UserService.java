package net.ionoff.center.client.service;

import java.util.List;

import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.QueryParam;

import org.fusesource.restygwt.client.MethodCallback;

import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.ProjectDto;
import net.ionoff.center.shared.dto.QueryCriteriaDto;
import net.ionoff.center.shared.dto.UserDeviceDto;
import net.ionoff.center.shared.dto.UserDto;
import net.ionoff.center.shared.dto.UserProjectDto;
import net.ionoff.center.shared.dto.UserSceneDto;
import net.ionoff.center.shared.dto.UserZoneDto;
import net.ionoff.center.shared.dto.VersionDto;
import net.ionoff.center.shared.dto.ZoneDto;

/**
 * @author Sann Tran
 */
public interface UserService extends EntityService<UserDto> {
	
	@Override
	@POST
	@Path("users/search")
	void searchByCriteria(QueryCriteriaDto criteriaDto,
			MethodCallback<List<UserDto>> methodCallback);

	@Override
	@POST
	@Path("users/count")
	void countByCriteria(QueryCriteriaDto criteriaDto,
			MethodCallback<Long> methodCallback);
	
	@PUT
	@Path("projects/{projectId}/users/{userId}")
	void save(@PathParam("projectId") Long projectId, @PathParam("userId") Long userId, 
			UserDto userDto, MethodCallback<UserDto> callback);
	
	@PUT
	@Path("users/{userId}")
	void save(@PathParam("userId") Long userId, 
			UserDto userDto, MethodCallback<UserDto> callback);
	
	@Override
	@DELETE
	@Path("users/{userId}")
	void delete(@PathParam("userId") Long userId, MethodCallback<MessageDto> callback);
	
	@GET
	@Path("users/{userId}")
	void findById(@PathParam("userId") Long userId, MethodCallback<UserDto> callback);
	
	@GET
	@Path("users/{userName}")
	void findByName(@PathParam("userName") String userName, MethodCallback<UserDto> callback);
	
	@POST
	@Path("versions/upgrade")
	void updateNewVersion(MethodCallback<VersionDto> callback);
	
	@GET
	@Path("versions/latest")
	void getLatestVersion(MethodCallback<VersionDto> methodCallback);
	
	@GET
	@Path("versions/current")
	void getCurrentVersion(MethodCallback<VersionDto> methodCallback);
	
	@GET
	@Path("versions/check")
	void checkLatestVersion(MethodCallback<VersionDto> methodCallback);
	
	@GET
	@Path("users/{userId}/projects")
	void getProjectsByUser(@PathParam("userId") Long userId, MethodCallback<List<ProjectDto>> methodCallback);
	
	@POST
	@Path("userprojects/{userProjectId}")
	void updateUserProject(@PathParam("userProjectId") Long userProjectId, UserProjectDto userProjectDto, MethodCallback<UserProjectDto> callback);

	@GET
	@Path("users/{userId}/zones")
	void getZonesByProjectId(@PathParam("userId") Long userId, @QueryParam("projectId") Long projectId, MethodCallback<List<ZoneDto>> methodCallback);
	
	@GET
	@Path("userzones")
	void getUserZonesByProjectId(@QueryParam("userId") Long userId,
			@QueryParam("projectId") Long projectId, MethodCallback<List<UserZoneDto>> callback);
	
	@POST
	@Path("userzones/{userZoneId}")
	void updateUserZone(@PathParam("userZoneId") Long userZoneId, UserZoneDto userZoneDto, MethodCallback<UserZoneDto> callback);
	
	@POST
	@Path("userdevices/{userDeviceId}")
	void updateUserDevice(@PathParam("userDeviceId") Long userDeviceId, UserDeviceDto userDeviceDto, MethodCallback<UserDeviceDto> callback);
	
	@POST
	@Path("userscenes/{userSceneId}")
	void updateUserScene(@PathParam("userSceneId") Long userSceneId, UserSceneDto userSceneDto, MethodCallback<UserSceneDto> callback);
	
	@GET
	@Path("userprojects")
	void getUserProjectsByUser(@QueryParam("userId") Long userId, MethodCallback<List<UserProjectDto>> methodCallback);
	
	@POST
	@Path("users/{userId}/language")
	void changeLanguage(@PathParam("userId") Long userId, @QueryParam("language") String language, MethodCallback<UserDto> callback);
}
