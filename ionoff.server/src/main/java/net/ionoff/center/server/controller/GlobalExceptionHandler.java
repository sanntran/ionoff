package net.ionoff.center.server.controller;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.BadRequestException;

import org.apache.log4j.Logger;
import org.springframework.http.HttpStatus;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;

import net.ionoff.center.server.exception.DeleteEntityException;
import net.ionoff.center.server.exception.EntityNotFoundException;
import net.ionoff.center.server.exception.RelayLockedException;
import net.ionoff.center.server.exception.UpdateEntityException;
import net.ionoff.center.server.locale.Messages;
import net.ionoff.center.server.relaydriver.exception.RelayDriverConnectException;
import net.ionoff.center.server.security.InvalidTokenException;
import net.ionoff.center.shared.dto.MessageDto;

@ControllerAdvice
public class GlobalExceptionHandler {

	private final Logger logger = Logger.getLogger(GlobalExceptionHandler.class.getName());

	@ResponseStatus(HttpStatus.UNAUTHORIZED)
	@ExceptionHandler(InvalidTokenException.class)
	@ResponseBody
	public MessageDto handleInvalidTokenException(HttpServletRequest request, InvalidTokenException e) {
		logger.error(e.getMessage());
		return new MessageDto(HttpStatus.UNAUTHORIZED.value(), "Unauthorised request");
	}
	
	@ExceptionHandler(value = Throwable.class)
	@ResponseBody
	public MessageDto defaultErrorHandler(HttpServletRequest request, HttpServletResponse response,
			Throwable e) {
		if (e instanceof ServletException
				|| e instanceof DeleteEntityException
				|| e instanceof UpdateEntityException) {
			logger.error(e.getMessage());
		}
		else {
			logger.error(e.getMessage(), e);
		}
		response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
		return new MessageDto(HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
				e.getMessage());
	}
	
	@ExceptionHandler(RelayDriverConnectException.class)
	@ResponseBody
	public MessageDto handleRelayDriverConnectException(HttpServletRequest request, HttpServletResponse response, 
			RelayDriverConnectException e) {
		final String locale = (String) request.getAttribute("locale");
		logger.error(e.getMessage());
		response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
		MessageDto message = new MessageDto(HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
				Messages.get(locale).errorConnectRelayDriver(e.getMessage()));
		message.setCode(RelayDriverConnectException.class.getSimpleName());
		return message;
	}

	@ExceptionHandler(EntityNotFoundException.class)
	@ResponseBody
	public MessageDto handleUnknownRelayDriverModelException(HttpServletRequest request, HttpServletResponse response, EntityNotFoundException e) {
		logger.error(e.getMessage());
		response.setStatus(HttpServletResponse.SC_NOT_FOUND);
		
		if (e.getResourceName() != null && e.getResourceId() != null) {
			final String locale = (String) request.getAttribute("locale");
			return new MessageDto(HttpServletResponse.SC_NOT_FOUND, 
					Messages.get(locale).resourceIdNotFound(e.getResourceName(), e.getResourceId()));
		}
		
		else {
			MessageDto message = new MessageDto();
			message.setMessage(e.getMessage());
			message.setStatus(HttpServletResponse.SC_NOT_FOUND);
			return message;
		}
	}
	
	@ExceptionHandler(BadRequestException.class)
	@ResponseBody
	public MessageDto handleUnknownRelayDriverModelException(HttpServletRequest request, HttpServletResponse response, BadRequestException e) {
		logger.error(e.getMessage(), e);
		response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
		return new MessageDto(HttpServletResponse.SC_BAD_REQUEST, e.getMessage());
	}
	

	@ExceptionHandler(AccessDeniedException.class)
	@ResponseBody
	public MessageDto handleAccessDeniedException(HttpServletRequest request, HttpServletResponse response, AccessDeniedException e) {
		logger.error(e.getMessage());
		response.setStatus(HttpServletResponse.SC_FORBIDDEN);
		return new MessageDto(HttpServletResponse.SC_FORBIDDEN, e.getMessage());
	}
	
	@ExceptionHandler(RelayLockedException.class)
	@ResponseBody
	public MessageDto handleRelayLockedException(HttpServletRequest request, HttpServletResponse response, 
			RelayLockedException e) {
		final String locale = (String) request.getAttribute("locale");
		logger.error(e.getMessage());
		response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
		MessageDto message = new MessageDto(HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
				Messages.get(locale).errorRelayLocked(e.getMessage()));
		message.setCode(RelayDriverConnectException.class.getSimpleName());
		return message;
	}
	
}
