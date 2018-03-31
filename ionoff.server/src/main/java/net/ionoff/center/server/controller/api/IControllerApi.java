package net.ionoff.center.server.controller.api;

import net.ionoff.center.server.entity.Controller;

public interface IControllerApi {
	
	ControllerStatus getStatus(Controller connection) throws ControllerException;
	
	void openRelay(Controller connection, int relayIndex) throws ControllerException;
	
	void closeRelay(Controller connection, int relayIndex) throws ControllerException;
	
	void closeOpenRelay(Controller connection, int relayIndex)throws ControllerException;
}
