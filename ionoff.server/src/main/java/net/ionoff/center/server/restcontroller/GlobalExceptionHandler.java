package net.ionoff.center.server.restcontroller;

import net.ionoff.center.server.controller.exception.ControllerConnectException;
import net.ionoff.center.server.exception.DeleteEntityException;
import net.ionoff.center.server.exception.EntityNotFoundException;
import net.ionoff.center.server.exception.RelayLockedException;
import net.ionoff.center.server.exception.UpdateEntityException;
import net.ionoff.center.server.locale.Messages;
import net.ionoff.center.server.security.InvalidTokenException;
import net.ionoff.center.shared.dto.MessageDto;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.servlet.NoHandlerFoundException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.BadRequestException;

@ControllerAdvice
public class GlobalExceptionHandler {

	private final Logger logger = LoggerFactory.getLogger(GlobalExceptionHandler.class.getName());

	@ExceptionHandler(NoHandlerFoundException.class)
	public MessageDto handleNoHandlerFoundException(HttpServletRequest request, HttpServletResponse response,
	                                                NoHandlerFoundException e) {
		logger.error(e.getMessage());
		response.setStatus(HttpServletResponse.SC_NOT_FOUND);
		MessageDto message = new MessageDto();
		message.setMessage(e.getMessage());
		message.setStatus(HttpServletResponse.SC_NOT_FOUND);
		return message;
	}

	@ResponseStatus(HttpStatus.UNAUTHORIZED)
	@ExceptionHandler(InvalidTokenException.class)
	public MessageDto handleInvalidTokenException(HttpServletRequest request, InvalidTokenException e) {
		logger.error(e.getMessage());
		return new MessageDto(HttpStatus.UNAUTHORIZED.value(), "Unauthorised request");
	}
	
	@ExceptionHandler(value = Throwable.class)
	public ResponseEntity<MessageDto> defaultErrorHandler(HttpServletRequest request, HttpServletResponse response,
										Throwable e) {
		if (e instanceof ServletException
				|| e instanceof DeleteEntityException
				|| e instanceof UpdateEntityException) {
			logger.error(e.getMessage());
		}
		else {
			logger.error(e.getMessage(), e);
		}
		return new ResponseEntity<>(new MessageDto(HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
				e.getClass().getSimpleName() + ": " + e.getMessage()), HttpStatus.INTERNAL_SERVER_ERROR);
	}
	
	@ExceptionHandler(ControllerConnectException.class)
	public MessageDto handleControllerConnectException(HttpServletRequest request, HttpServletResponse response,
			ControllerConnectException e) {
		final String locale = (String) request.getAttribute("locale");
		logger.error(e.getMessage());
		response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
		MessageDto message = new MessageDto(HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
				Messages.get(locale).errorConnectController(e.getMessage()));
		message.setCode(ControllerConnectException.class.getSimpleName());
		return message;
	}

	@ExceptionHandler(EntityNotFoundException.class)
	public MessageDto handleUnknownControllerModelException(HttpServletRequest request, HttpServletResponse response, EntityNotFoundException e) {
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
	public MessageDto handleUnknownControllerModelException(HttpServletRequest request, HttpServletResponse response, BadRequestException e) {
		logger.error(e.getMessage(), e);
		response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
		return new MessageDto(HttpServletResponse.SC_BAD_REQUEST, e.getMessage());
	}
	

	@ExceptionHandler(AccessDeniedException.class)
	public MessageDto handleAccessDeniedException(HttpServletRequest request, HttpServletResponse response, AccessDeniedException e) {
		logger.error(e.getMessage());
		response.setStatus(HttpServletResponse.SC_FORBIDDEN);
		return new MessageDto(HttpServletResponse.SC_FORBIDDEN, e.getMessage());
	}
	
	@ExceptionHandler(RelayLockedException.class)
	public MessageDto handleRelayLockedException(HttpServletRequest request, HttpServletResponse response,
			RelayLockedException e) {
		final String locale = (String) request.getAttribute("locale");
		logger.error(e.getMessage());
		response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
		MessageDto message = new MessageDto(HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
				Messages.get(locale).errorRelayLocked(e.getMessage()));
		message.setCode(ControllerConnectException.class.getSimpleName());
		return message;
	}
	
}
