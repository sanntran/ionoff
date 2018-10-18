package net.ionoff.center.server.entity;

import java.util.List;
import java.util.Set;

public class Project extends BaseObj {

	private static final long serialVersionUID = 1L;
	public static final long DEFAULT_ID = 1L;

	private String address;
	private List<Zone> zones;
	private List<Area> areas;
	private Set<UserProject> users;
	private Set<Mode> modes;
	private Set<Sensor> sensors;
	private Set<Controller> controllers;

	public String getAddress() {
		return address;
	}
	public void setAddress(String address) {
		this.address = address;
	}

	public List<Zone> getZones() {
		return zones;
	}
	public void setZones(List<Zone> zones) {
		this.zones = zones;
	}
	
	public List<Area> getAreas() {
		return areas;
	}
	public void setAreas(List<Area> areas) {
		this.areas = areas;
	}

	public Set<UserProject> getUsers() {
		return users;
	}
	public void setUsers(Set<UserProject> users) {
		this.users = users;
	}

	public Set<Mode> getModes() {
		return modes;
	}
	public void setModes(Set<Mode> modes) {
		this.modes = modes;
	}

	public Set<Sensor> getSensors() {
		return sensors;
	}
	public void setSensors(Set<Sensor> sensors) {
		this.sensors = sensors;
	}


	public Set<Controller> getControllers() {
		return controllers;
	}
	public void setControllers(Set<Controller> controllers) {
		this.controllers = controllers;
	}

	public boolean hasZone() {
		return zones != null && !zones.isEmpty();
	}

	public boolean hasArea() {
		return areas != null && !areas.isEmpty();
	}
	
	public boolean hasMode() {
		return modes != null && !modes.isEmpty();
	}
}
