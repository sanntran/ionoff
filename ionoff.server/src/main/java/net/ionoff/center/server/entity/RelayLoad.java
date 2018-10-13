package net.ionoff.center.server.entity;

public class RelayLoad extends Device {
	private static final long serialVersionUID = 1L;

	private Boolean status;

	@Override
	public Boolean getStatus() {
		return status;
	}
 
	@Override
	public void setStatus(Boolean status) {
		this.status = status;
	}
}
