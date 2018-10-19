package net.ionoff.center.server.entity;

import java.util.List;

public class Trigger extends BaseObj {

	private static final long serialVersionUID = 1L;

	private String condition;
	private List<Action> actions;

}
