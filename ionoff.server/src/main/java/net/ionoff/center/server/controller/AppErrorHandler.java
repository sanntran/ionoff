package net.ionoff.center.server.controller;

import javax.servlet.http.HttpServletRequest;

import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;

import net.ionoff.center.shared.dto.MessageDto;

@Controller
public class AppErrorHandler {

	@ResponseStatus(HttpStatus.NOT_FOUND)
    @RequestMapping("errors/404")
    @ResponseBody
    public MessageDto resourceNotFound(HttpServletRequest request) throws Exception {
        return new MessageDto(HttpStatus.NOT_FOUND.value(), "Resource not found");
    }

    @ResponseStatus(HttpStatus.UNAUTHORIZED)
    @RequestMapping("errors/401")
    @ResponseBody
    public MessageDto unAuthorised(HttpServletRequest request) throws Exception {
    	return new MessageDto(HttpStatus.UNAUTHORIZED.value(), "Unauthorised request");
    }
    
    @ResponseStatus(HttpStatus.FORBIDDEN)
    @RequestMapping("errors/403")
    @ResponseBody
    public MessageDto forbidden(HttpServletRequest request) throws Exception {
    	return new MessageDto(HttpStatus.FORBIDDEN.value(), "Access is denied");
    }
    
    @ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
    @RequestMapping("errors/500")
    @ResponseBody
    public MessageDto internalServerError(HttpServletRequest request) throws Exception {
    	return new MessageDto(HttpStatus.INTERNAL_SERVER_ERROR.value(), "Internal server error!");
    }
}
