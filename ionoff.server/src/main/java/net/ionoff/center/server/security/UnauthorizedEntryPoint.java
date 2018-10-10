package net.ionoff.center.server.security;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.AuthenticationEntryPoint;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.HandlerExceptionResolver;

/**
 * {@link AuthenticationEntryPoint} that rejects all requests with an unauthorized error message.
 */
@Component
public class UnauthorizedEntryPoint implements AuthenticationEntryPoint {

	@Autowired
	private HandlerExceptionResolver handlerExceptionResolver;

	@Override
	public void commence(HttpServletRequest request, HttpServletResponse response,
			AuthenticationException authException) throws IOException, ServletException {

		//handlerExceptionResolver.resolveException(request, response, null, authException);
		response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "Request is unauthorized");
	}
}
