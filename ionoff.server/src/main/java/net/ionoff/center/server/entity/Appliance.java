package net.ionoff.center.server.entity;

public class Appliance extends Device {
	private static final long serialVersionUID = 1L;

	private Boolean status;

	@Override
	public Boolean getStatus() {
		return status;
	}

	public void setStatus(Boolean status) {
		this.status = status;
	}
}
