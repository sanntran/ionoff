package net.xapxinh.player.handler;

import java.util.Map;

import net.xapxinh.player.AppProperties;
import net.xapxinh.player.model.Schedule;
import net.xapxinh.player.server.exception.DateTimeFormatException;
import net.xapxinh.player.server.exception.UnknownCommandException;

public class ScheduleRequestHandler {
	
	public Schedule handleRequest(Map<String, String> parameters) throws UnknownCommandException, DateTimeFormatException {
		String command = parameters.get("command");
		return handleRequest(command, parameters);
	}
	
	private Schedule handleRequest(String command, Map<String, String> parameters) throws UnknownCommandException, DateTimeFormatException {
		if ("get".equals(command)) {
			return getSchedule();
		}
		if ("update".equals(command)) {
			return updateSchedule(parameters);
		}
		throw new UnknownCommandException(command);
	}
	
	private Schedule updateSchedule(Map<String, String> params) throws DateTimeFormatException {
		String action = params.get("action");
		String dateTime = params.get("dateTime");
		AppProperties.getSchedule().setAction(action);
		AppProperties.getSchedule().setDateTime(dateTime);
		AppProperties.writeProperties();
		return AppProperties.getSchedule();
	}

	private Schedule getSchedule() {
		return AppProperties.getSchedule();
	}
}
