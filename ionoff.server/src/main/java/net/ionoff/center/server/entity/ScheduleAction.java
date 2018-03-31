package net.ionoff.center.server.entity;

public class ScheduleAction extends BaseObj {

	private static final long serialVersionUID = 1L;
	
	public static final String NONE = "None";
	
	private String action;
	private Schedule schedule;
	
	public String getAction() {
		return action;
	}
	public void setAction(String action) {
		this.action = action;
	}
	
	public Schedule getSchedule() {
		return schedule;
	}
	public void setSchedule(Schedule schedule) {
		this.schedule = schedule;
	}
}
