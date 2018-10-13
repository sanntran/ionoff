package net.ionoff.center.server.service;

import net.ionoff.center.server.entity.Device;
import net.ionoff.center.server.entity.Mode;
import net.ionoff.center.server.entity.MediaPlayer;
import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.entity.Scene;
import net.ionoff.center.shared.dto.StatusDto;

public interface IControlService {
	 
	void activateMode(Mode mode);
	
	void playScene(Scene scene);
	
	void switchRelayToOn(Relay relay);
	
	void switchRelayToOff(Relay relay);
	
	void executeRelayAction(Relay relay, String action);
	
	void executePlayerAction(MediaPlayer player, String action, String volume, String album, String albumType);
	
	StatusDto switchOn(long relayId);
	
	StatusDto switchOff(long relayId);
	
	void setRelayState(Relay relay, Boolean state);

	StatusDto turnOnDevice(Device device);

	StatusDto turnOffDevice(Device device);

	StatusDto getStatusDto(Relay relay);
}
