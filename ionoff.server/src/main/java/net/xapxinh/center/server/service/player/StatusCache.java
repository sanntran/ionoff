package net.xapxinh.center.server.service.player;

import net.xapxinh.center.shared.dto.Status;

public class StatusCache extends PlayerCache {
	
	private Status status;

	public Status getStatus() {
		return status;
	}
	
	public void setStatus(Status status) {
		this.status = status;
	}

	@Override
	protected long getLivingTime() {
		return 2000;
	}
}
