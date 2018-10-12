package net.ionoff.center.client.service;

import javax.ws.rs.DELETE;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.QueryParam;

import org.fusesource.restygwt.client.MethodCallback;
import org.fusesource.restygwt.client.Options;
import org.fusesource.restygwt.client.RestService;

import net.ionoff.center.shared.cookie.Kookie;
import net.ionoff.center.shared.dto.MessageDto;

/**
 * @author Sann Tran
 */
public interface LoginService  extends RestService {

	@POST
	@Path("users/license")
	public void activate(@QueryParam("licenseKey") String licenseKey, MethodCallback<MessageDto> methodCallback);
	
	@POST
	@Path("users/authenticate")
	@Options(timeout = 6000)
	public void requestAuthen(
			@QueryParam("projectId") Long projectId,
			MethodCallback<Kookie> callback);
	
	@POST
	@Path("users/authenticate")
	public void requestAuthen(@QueryParam("username") String username, 
			@QueryParam("password") String hashPass,
			@QueryParam("remember") Boolean remember,
			@QueryParam("language") String language,
			MethodCallback<Kookie> callback);

	@DELETE
	@Path("users/authenticate")
	public void deleteAuthen(MethodCallback<Kookie> callback);
	
}
