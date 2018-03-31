package net.ionoff.center.client.relay;

import net.ionoff.center.shared.dto.RelayDto;

class RelaySelectionHandler {
	
	private RelayEditPresenter relayEditPresenter;
	
	RelaySelectionHandler(RelayEditPresenter relayEditPresenter) {
		this.relayEditPresenter = relayEditPresenter;
	}

	public void onRelaySelected(RelayDto relay) {
		relayEditPresenter.addRelayToGroup(relay);
	}
}
