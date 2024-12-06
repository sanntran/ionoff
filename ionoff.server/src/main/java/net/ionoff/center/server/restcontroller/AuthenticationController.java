package net.ionoff.center.server.restcontroller;

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.OPTIONS;
import javax.ws.rs.Path;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpStatus;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.exception.AppUnactivatedException;
import net.ionoff.center.server.license.LicenseManager;
import net.ionoff.center.server.locale.Messages;
import net.ionoff.center.server.persistence.mapper.UserMapper;
import net.ionoff.center.server.persistence.mapper.ZoneMapper;
import net.ionoff.center.server.persistence.service.IProjectService;
import net.ionoff.center.server.persistence.service.IUserService;
import net.ionoff.center.server.security.InvalidTokenException;
import net.ionoff.center.server.security.JwtAuthenticationTokenFilter;
import net.ionoff.center.server.security.JwtObject;
import net.ionoff.center.server.security.JwtTokenUtil;
import net.ionoff.center.shared.cookie.Kookie;
import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.ProjectDto;

@RestController
public class AuthenticationController {

	private final Logger logger = LoggerFactory.getLogger(AuthenticationController.class.getName());

	@Autowired
	private IUserService userService;
	
	@Autowired
	private IProjectService projectService;
	
	@Autowired
	private JwtTokenUtil jwtTokenUtil;
	
	@Autowired
    @Qualifier("authenticationManager")
    private AuthenticationManager authManager;

	@Autowired
	private UserMapper userMapper;
	
	@Autowired
	private ZoneMapper zoneMapper;
	
	@RequestMapping(
			value = "users/authenticate",
			method = RequestMethod.DELETE,
			produces = "application/json; charset=utf-8")

	public Kookie deleteAuthen(HttpServletRequest request) {
		String authToken = request.getHeader(JwtAuthenticationTokenFilter.TOKEN_HEADER);
		logger.info("User logout: " + ". Token: " + authToken);
		return new Kookie();
	}

	@RequestMapping(
			value = "users/authenticate",
			params = {},
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")

	public Kookie requestAuthen(
			@RequestParam(value="projectId", required=false) Long projectId, HttpServletRequest request) {
		

		String authToken = request.getHeader(JwtAuthenticationTokenFilter.TOKEN_HEADER);
        JwtObject jwtObj = jwtTokenUtil.getJwtObjectFromToken(authToken);
		
        if (jwtObj.getUser() == null) {
        	throw new InvalidTokenException("Username from token is null");
        }
        User user = (User) userService.loadUserByUsername(jwtObj.getUser());
    	if (user == null) {
        	throw new InvalidTokenException("Username from token is not existed in DB");
    	}
    	if (!user.getPassword().equals(jwtObj.getHash())) {
        	throw new InvalidTokenException("User password from token is not valid");
    	}
    	if (jwtObj.isExpired()) {
        	throw new InvalidTokenException("Token is not valuable (expired)");
        }
    	if (Boolean.TRUE.equals(jwtObj.getExpired())) {
    		logger.info("Refresh authentication token. Token " + authToken);
    		UsernamePasswordAuthenticationToken authentication = new UsernamePasswordAuthenticationToken(user, null, user.getAuthorities());
            authentication.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));
            SecurityContextHolder.getContext().setAuthentication(authentication);
            String newToken = jwtTokenUtil.refreshToken(authToken);
            return createKookie(user, newToken, projectId);
        }
        return createKookie(user, authToken, projectId);
	}
	
	@RequestMapping(
			value = "users/authenticate",
			params = {"username", "password", "remember", "language"},
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")

	public Kookie requestAuthen( @RequestParam("username") String username, @RequestParam("password") String password,
								@RequestParam("remember") boolean remember, @RequestParam("language") String language) {
		
		UsernamePasswordAuthenticationToken authenticationToken =
                new UsernamePasswordAuthenticationToken(username, password);
        
        Authentication authentication = this.authManager.authenticate(authenticationToken);
        
        SecurityContextHolder.getContext().setAuthentication(authentication);
        Object principal = authentication.getPrincipal();
        if (!(principal instanceof User)) {
            throw new WebApplicationException(Response.Status.UNAUTHORIZED);
        }
        User user = (User) principal;
 		final String token = jwtTokenUtil.generateToken(user, remember);
		logger.info("Login successfully. User: " + username + ". Token: " + token);
		Kookie kookie = createKookie(user, token, null);
		return kookie;
    }
	
	private Kookie createKookie(User user, String token, Long projectId) {
		final Kookie cookie = new Kookie();
		cookie.setUser(userMapper.createUserDto(user));
		
		List<ProjectDto> projectDtos = projectService.findDtoByUserId(user.getId());
		cookie.getUser().setProjects(projectDtos);
		
		if (!cookie.getUser().getProjects().isEmpty()) {
			if (projectId != null) {
				cookie.setProjectId(projectId);
			}
			else {
				cookie.setProjectId(cookie.getUser().getProjects().get(0).getId());
			}
		}
		cookie.setJwtToken(token);
		
		return cookie;
	}

	@ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
	@ExceptionHandler(AppUnactivatedException.class)

	public MessageDto handleInvalidTokenException(HttpServletRequest request, AppUnactivatedException e) {
		MessageDto message = new MessageDto(HttpStatus.INTERNAL_SERVER_ERROR.value(), 
				Messages.get(null).applicationUnactivated());
		message.setCode("license");
		return message;
	}
	
	@ResponseStatus(HttpStatus.UNAUTHORIZED)
	@ExceptionHandler(AuthenticationException.class)

	public MessageDto handleInvalidTokenException(HttpServletRequest request, AuthenticationException e) {
		logger.error("Authentication exception. Login unsuccessfully.");
		return new MessageDto(HttpStatus.UNAUTHORIZED.value(), "Authentication exception");
	}

}
