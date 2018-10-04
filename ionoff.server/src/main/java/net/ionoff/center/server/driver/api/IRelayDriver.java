package net.ionoff.center.server.driver.api;

import net.ionoff.center.server.entity.RelayDriver;

public interface IRelayDriver {
	
	void openRelay(RelayDriver driver, int relayIndex) throws RelayDriverException;
	
	void closeRelay(RelayDriver driver, int relayIndex) throws RelayDriverException;
	
	void openRelay(RelayDriver driver, int relayIndex, Integer autoRevert) throws RelayDriverException;
	
	void closeRelay(RelayDriver driver, int relayIndex, Integer autoRevert) throws RelayDriverException;
	
	RelayDriverStatus getStatus(RelayDriver driver) throws RelayDriverException;
}
