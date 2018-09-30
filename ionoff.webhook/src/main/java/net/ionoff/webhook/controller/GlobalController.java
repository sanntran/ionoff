package net.ionoff.webhook.controller;

import net.ionoff.webhook.dto.MessageDto;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.http.HttpServletRequest;
import java.text.SimpleDateFormat;


@RestController
public class GlobalController {
	
	private static final Logger logger = LoggerFactory.getLogger(GlobalController.class);
	private static final String NOT_FOUND = "NotFound";

	private static final SimpleDateFormat TIME_FORMATTER = new SimpleDateFormat("HH:mm");
	private static final SimpleDateFormat DATE_FORMATTER = new SimpleDateFormat("dd/MM/yyyy");

	@ResponseStatus(HttpStatus.NOT_FOUND)
	@RequestMapping(value = "/*", method = RequestMethod.GET)
	public MessageDto handleGETNotFound(HttpServletRequest request) {
		return new MessageDto(HttpStatus.NOT_FOUND.value(), NOT_FOUND,
				"Resource not found: " + request.getRequestURI());
	}

	@ResponseStatus(HttpStatus.NOT_FOUND)
	@RequestMapping(value = "/*", method = RequestMethod.PUT)
	public MessageDto handlePUTNotFound(HttpServletRequest request) {
		return new MessageDto(HttpStatus.NOT_FOUND.value(), NOT_FOUND,
                "Resource not found: " + request.getRequestURI());
	}

	@ResponseStatus(HttpStatus.NOT_FOUND)
	@RequestMapping(value = "/*", method = RequestMethod.POST)
	public MessageDto handlePOSTNotFound(HttpServletRequest request) {
		return new MessageDto(HttpStatus.NOT_FOUND.value(), NOT_FOUND,
                "Resource not found: " + request.getRequestURI());
	}

	@ResponseStatus(HttpStatus.NOT_FOUND)
	@RequestMapping(value = "/*", method = RequestMethod.PATCH)
	public MessageDto handlePATCHNotFound(HttpServletRequest request) {
		return new MessageDto(HttpStatus.NOT_FOUND.value(), NOT_FOUND,
                "Resource not found: " + request.getRequestURI());
	}

	@ResponseStatus(HttpStatus.NOT_FOUND)
	@RequestMapping(value = "/*", method = RequestMethod.OPTIONS)
	public MessageDto handleOPTIONSNotFound(HttpServletRequest request) {
		return new MessageDto(HttpStatus.NOT_FOUND.value(), NOT_FOUND,
                "Resource not found: " + request.getRequestURI());
	}

	@ResponseStatus(HttpStatus.NOT_FOUND)
	@RequestMapping(value = "/*", method = RequestMethod.DELETE)
	public MessageDto handleDELETENotFound(HttpServletRequest request) {
		return new MessageDto(HttpStatus.NOT_FOUND.value(), NOT_FOUND,
                "Resource not found: " + request.getRequestURI());
	}

	@ResponseStatus(HttpStatus.NOT_FOUND)
	@RequestMapping(value = "/*", method = RequestMethod.HEAD)
	public MessageDto handleHEADNotFound(HttpServletRequest request) {
		return new MessageDto(HttpStatus.NOT_FOUND.value(), NOT_FOUND,
                "Resource not found: " + request.getPathInfo());
	}

	@ResponseStatus(HttpStatus.NOT_FOUND)
	@RequestMapping(value = "/*", method = RequestMethod.TRACE)
	public MessageDto handleTRACENotFound(HttpServletRequest request) {
		return new MessageDto(HttpStatus.NOT_FOUND.value(), NOT_FOUND,
                "Resource not found: " + request.getRequestURI());
	}

	public static void main(String args[]) {
		System.out.println(String.valueOf(HttpStatus.NOT_FOUND));
	}
}
