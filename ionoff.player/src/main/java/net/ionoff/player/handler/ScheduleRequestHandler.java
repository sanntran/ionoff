package net.ionoff.player.handler;

import net.ionoff.player.connection.MqttRequestMessage;
import net.ionoff.player.exception.UnknownCommandException;
import net.ionoff.player.model.Schedule;
import net.ionoff.player.storage.LocalStorage;

public class ScheduleRequestHandler {
	
	public Schedule handleRequest(MqttRequestMessage request) {
		String command = request.getCommand();
		return handleRequest(command, request);
	}
	
	private Schedule handleRequest(String command, MqttRequestMessage request) {
		if ("get".equals(command)) {
			return getSchedule();
		}
		if ("update".equals(command)) {
			return updateSchedule(request);
		}
		throw new UnknownCommandException(command);
	}
	
	private Schedule updateSchedule(MqttRequestMessage request) {
		String action = request.getAsString("action");
		String dateTime = request.getAsString("dateTime");
		LocalStorage.INSTANCE.getData().getSchedule().setAction(action);
		LocalStorage.INSTANCE.getData().getSchedule().setDateTime(dateTime);
		LocalStorage.INSTANCE.write();
		return LocalStorage.INSTANCE.getData().getSchedule();
	}

	private Schedule getSchedule() {
		return LocalStorage.INSTANCE.getData().getSchedule();
	}
}
