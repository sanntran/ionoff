package net.ionoff.center.server.control;

import net.ionoff.center.server.entity.Mode;
import net.ionoff.center.server.entity.Player;
import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.entity.RelayDriver;
import net.ionoff.center.server.entity.Scene;
import net.ionoff.center.server.relaydriver.api.RelayDriverStatus;
import net.ionoff.center.shared.dto.StatusDto;

public interface IControlService {
	 
	void activateMode(Mode mode);
	
	void playScene(Scene scene);
	
	RelayDriverStatus getRelayDriverStatus(RelayDriver relayDriver);
	
	StatusDto turnOnDevice(long deviceId);
	
	StatusDto turnOffDevice(long deviceId);
	
	void setRelayOn(Relay relay);
	
	void setRelayOff(Relay relay);
	
	void switchRelayOnOff(Relay relay);
	
	void executeRelayAction(Relay relay, String action);
	
	void executePlayerAction(Player player, String action, String volume, String album, String albumType);
	
	boolean ping(RelayDriver relayDriver);

	StatusDto switchOn(long relayId);
	
	StatusDto switchOff(long relayId);
	
	StatusDto switchOnOff(long relayId);

	void playScene(long sceneId);

	void setRelayState(Relay relay, Boolean state);
}
