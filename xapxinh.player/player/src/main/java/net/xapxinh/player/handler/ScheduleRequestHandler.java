package net.xapxinh.player.handler;

import net.xapxinh.player.AppProperties;
import net.xapxinh.player.connection.MqttRequestMessage;
import net.xapxinh.player.model.Schedule;
import net.xapxinh.player.server.exception.DateTimeFormatException;
import net.xapxinh.player.server.exception.UnknownCommandException;

public class ScheduleRequestHandler {
	
	public Schedule handleRequest(MqttRequestMessage request) throws UnknownCommandException, DateTimeFormatException {
		String command = request.getAsString("command");
		return handleRequest(command, request);
	}
	
	private Schedule handleRequest(String command, MqttRequestMessage request) throws UnknownCommandException, DateTimeFormatException {
		if ("get".equals(command)) {
			return getSchedule();
		}
		if ("update".equals(command)) {
			return updateSchedule(request);
		}
		throw new UnknownCommandException(command);
	}
	
	private Schedule updateSchedule(MqttRequestMessage request) throws DateTimeFormatException {
		String action = request.getAsString("action");
		String dateTime = request.getAsString("dateTime");
		AppProperties.getSchedule().setAction(action);
		AppProperties.getSchedule().setDateTime(dateTime);
		AppProperties.writeProperties();
		return AppProperties.getSchedule();
	}

	private Schedule getSchedule() {
		return AppProperties.getSchedule();
	}
}
