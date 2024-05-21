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
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.servlet.NoHandlerFoundException;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.BadRequestException;

@ControllerAdvice
@Transactional
public class GlobalExceptionHandler extends ResponseEntityExceptionHandler {

	private final Logger logger = LoggerFactory.getLogger(GlobalExceptionHandler.class.getName());

	@Override
	protected ResponseEntity<Object> handleNoHandlerFoundException(NoHandlerFoundException ex, HttpHeaders headers, HttpStatus status, WebRequest request) {
		logger.error(ex.getMessage());
		MessageDto message = new MessageDto();
		message.setMessage(ex.getMessage());
		message.setStatus(HttpServletResponse.SC_NOT_FOUND);
		return new ResponseEntity<>(message, HttpStatus.NOT_FOUND);
	}

	@ResponseBody
	@ResponseStatus(HttpStatus.UNAUTHORIZED)
	@ExceptionHandler(InvalidTokenException.class)
	public ResponseEntity<MessageDto> handleInvalidTokenException(HttpServletRequest request, InvalidTokenException e) {
		logger.error(e.getMessage());
		MessageDto message =  new MessageDto(HttpStatus.UNAUTHORIZED.value(), "Unauthorised request");
		return new ResponseEntity<>(message, HttpStatus.UNAUTHORIZED);
	}

	@ResponseBody
	@ExceptionHandler(value = Throwable.class)
	public ResponseEntity<MessageDto> defaultErrorHandler(HttpServletRequest request,
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

	@ResponseBody
	@ExceptionHandler(ControllerConnectException.class)
	public ResponseEntity<MessageDto> handleControllerConnectException(HttpServletRequest request,
			ControllerConnectException e) {
		final String locale = (String) request.getAttribute("locale");
		logger.error(e.getMessage());
		MessageDto message = new MessageDto(HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
				Messages.get(locale).errorConnectController(e.getMessage()));
		message.setCode(ControllerConnectException.class.getSimpleName());
		return new ResponseEntity<>(message, HttpStatus.INTERNAL_SERVER_ERROR);
	}

	@ResponseBody
	@ExceptionHandler(EntityNotFoundException.class)
	public ResponseEntity<MessageDto> handleUnknownControllerModelException(HttpServletRequest request, EntityNotFoundException e) {
		logger.error(e.getMessage());
		if (e.getResourceName() != null && e.getResourceId() != null) {
			final String locale = (String) request.getAttribute("locale");

			MessageDto message = new MessageDto(HttpServletResponse.SC_NOT_FOUND,
					Messages.get(locale).resourceIdNotFound(e.getResourceName(), e.getResourceId()));
			return new ResponseEntity<>(message, HttpStatus.NOT_FOUND);
		} else {
			MessageDto message = new MessageDto();
			message.setMessage(e.getMessage());
			message.setStatus(HttpServletResponse.SC_NOT_FOUND);
			return new ResponseEntity<>(message, HttpStatus.NOT_FOUND);
		}
	}

	@ResponseBody
	@ExceptionHandler(BadRequestException.class)
	public ResponseEntity<MessageDto> handleUnknownControllerModelException(HttpServletRequest request, BadRequestException e) {
		logger.error(e.getMessage(), e);
		MessageDto message = new MessageDto(HttpServletResponse.SC_BAD_REQUEST, e.getMessage());
		return new ResponseEntity<>(message, HttpStatus.BAD_REQUEST);
	}

	@ResponseBody
	@ExceptionHandler(AccessDeniedException.class)
	public ResponseEntity<MessageDto> handleAccessDeniedException(HttpServletRequest request, AccessDeniedException e) {
		logger.error(e.getMessage());
		MessageDto message = new MessageDto(HttpServletResponse.SC_FORBIDDEN, e.getMessage());
		return new ResponseEntity<>(message, HttpStatus.FORBIDDEN);
	}

	@ResponseBody
	@ExceptionHandler(RelayLockedException.class)
	public ResponseEntity<MessageDto> handleRelayLockedException(HttpServletRequest request,
			RelayLockedException e) {
		final String locale = (String) request.getAttribute("locale");
		logger.error(e.getMessage());
		MessageDto message = new MessageDto(HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
				Messages.get(locale).errorRelayLocked(e.getMessage()));
		message.setCode(ControllerConnectException.class.getSimpleName());
		return new ResponseEntity<>(message, HttpStatus.INTERNAL_SERVER_ERROR);
	}
	
}
