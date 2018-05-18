package net.ionoff.center.client.relay;

import net.ionoff.center.shared.dto.RelayDto;

class RelaySelectionHandler {
	
	private RelayGroupPresenter relayGroupPresenter;
	
	RelaySelectionHandler(RelayGroupPresenter relayGroupPresenter) {
		this.relayGroupPresenter = relayGroupPresenter;
	}

	public void onRelaySelected(RelayDto relay) {
		relayGroupPresenter.addRelayToGroup(relay);
	}
}
