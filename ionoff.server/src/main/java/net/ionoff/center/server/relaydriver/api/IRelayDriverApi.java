package net.ionoff.center.server.relaydriver.api;

import net.ionoff.center.server.entity.RelayDriver;

public interface IRelayDriverApi {
	
	RelayDriverStatus getStatus(RelayDriver connection) throws RelayDriverException;
	
	void openRelay(RelayDriver connection, int relayIndex) throws RelayDriverException;
	
	void closeRelay(RelayDriver connection, int relayIndex) throws RelayDriverException;
	
	void closeOpenRelay(RelayDriver connection, int relayIndex)throws RelayDriverException;
}
