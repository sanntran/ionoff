package net.ionoff.center.client.service;

import javax.ws.rs.GET;
import javax.ws.rs.Path;

import org.fusesource.restygwt.client.MethodCallback;
import org.fusesource.restygwt.client.RestService;

import net.ionoff.center.shared.dto.DateTimeDto;
import net.ionoff.center.shared.dto.ServerInfoDto;

/**
 * @author Sann Tran
 */
public interface SystemService extends RestService {

	@GET
	@Path("api/system/info")
	void getServerInfo(MethodCallback<ServerInfoDto> methodCallback);
	
	@GET
	@Path("api/system/datetime")
	void getServerDateTime(MethodCallback<DateTimeDto> methodCallback);
}
