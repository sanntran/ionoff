package net.ionoff.player.handler;

import java.util.Map;

import net.ionoff.player.exception.UnknownCommandException;
import net.ionoff.player.model.Schedule;
import net.ionoff.player.storage.LocalStorage;

public class ScheduleRequestHandler {
	
	public Schedule handleRequest(Map<String, String> parameters) {
		String command = parameters.get("command");
		return handleRequest(command, parameters);
	}
	
	private Schedule handleRequest(String command, Map<String, String> parameters) {
		if ("get".equals(command)) {
			return getSchedule();
		}
		if ("update".equals(command)) {
			return updateSchedule(parameters);
		}
		throw new UnknownCommandException(command);
	}
	
	private Schedule updateSchedule(Map<String, String> params) {
		String action = params.get("action");
		String dateTime = params.get("dateTime");
		LocalStorage.INSTANCE.getData().getSchedule().setAction(action);
		LocalStorage.INSTANCE.getData().getSchedule().setDateTime(dateTime);
		LocalStorage.INSTANCE.write();
		return LocalStorage.INSTANCE.getData().getSchedule();
	}

	private Schedule getSchedule() {
		return LocalStorage.INSTANCE.getData().getSchedule();
	}
}
