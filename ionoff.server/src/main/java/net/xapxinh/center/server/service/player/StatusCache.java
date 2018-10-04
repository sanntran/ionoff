package net.xapxinh.center.server.service.player;

import net.xapxinh.center.shared.dto.StatusDto;

public class StatusCache extends PlayerCache {
	
	private StatusDto status;

	public StatusDto getStatus() {
		return status;
	}
	
	public void setStatus(StatusDto status) {
		this.status = status;
	}

	@Override
	protected long getLivingTime() {
		return 2000;
	}
}
