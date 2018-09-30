package net.ionoff.webhook.controller;

import net.ionoff.webhook.dto.MessageDto;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.NoHandlerFoundException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

@ControllerAdvice
public class ExceptionsHandler {

	private static final Logger logger = LoggerFactory.getLogger(ExceptionsHandler.class);

	@ExceptionHandler(NoHandlerFoundException.class)
	@ResponseBody
	public MessageDto handleUnknownRelayDriverModelException(HttpServletRequest request, HttpServletResponse response,
															 NoHandlerFoundException e) {
		logger.error(e.getMessage());
		response.setStatus(HttpServletResponse.SC_NOT_FOUND);
		MessageDto message = new MessageDto();
		message.setMessage(e.getMessage());
		message.setStatus(HttpServletResponse.SC_NOT_FOUND);
		return message;
	}
}
