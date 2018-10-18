package net.ionoff.center.server.restcontroller;

import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.context.SecurityContextHolder;

import net.ionoff.center.server.entity.Area;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.entity.UserProject;
import net.ionoff.center.server.entity.UserZone;
import net.ionoff.center.server.security.InvalidTokenException;

public class RequestContextHolder {

	static User getUser() {
		try {
			User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
			return user;
		}
		catch (Exception e) {
			throw new InvalidTokenException(SecurityContextHolder.getContext().getAuthentication().getPrincipal().toString());
		}
	}
	
	static String getLocale() {
		User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
		return user.getLanguage();
	}

	static String getUserName() {
		User user = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
		return user.getUsername();
	}

	static void checkProjectPermission(User user, Long projectId) {
		if (projectId == null) {
			if (user.isSystemAdmin()) {
				return;
			}
			throw new AccessDeniedException("Access denied. (User: " + user.getName() + " is not system admin)");
		}
		for (UserProject userProject : user.getProjects()) {
			if (userProject.hasRole() && projectId.longValue() == userProject.getProject().getId()) {
				return;
			}
		}
		throw new AccessDeniedException("Access denied. (User: " + user.getName() + ", Project ID: " + projectId + ")");
	}
	
	static void checkZonePermission(User user, Long zoneId) {
		for (UserZone userZone : user.getZones()) {
			if (userZone.hasRole() && zoneId != null && zoneId.longValue() == userZone.getZone().getId()) {
				return ;
			}
		}
		throw new AccessDeniedException("Access denied. (User: " + user.getName() + ", Zone ID: " + zoneId + ")");
	}

	public static void checkAreaPermission(User user, Long areaId) {
		for (UserProject userProject : user.getProjects()) {
			if (userProject.hasRole()) {
				for (Area area : userProject.getProject().getAreas()) {
					if (areaId != null && areaId.longValue() == area.getId()) {
						return;
					}
				}
			}
		}
		throw new AccessDeniedException("Access denied. (User: " + user.getName() + ", Area ID: " + areaId + ")");
	}

	public static void checkAdminPermission(User user) {
		if (!user.hasAdminRole()) {
			throw new AccessDeniedException("Access denied");
		}
	}
}
