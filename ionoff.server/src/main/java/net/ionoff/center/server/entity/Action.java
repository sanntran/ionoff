package net.ionoff.center.server.entity;

public class Action extends BaseObj {

	private static final long serialVersionUID = 1L;

	private Integer order;
	private String action;

	public Integer getOrder() {
		return order;
	}

	public void setOrder(Integer order) {
		this.order = order;
	}

	public String getAction() {
		return action;
	}

	public void setAction(String action) {
		this.action = action;
	}
}
