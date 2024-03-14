package net.ionoff.center.server.entity;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import java.util.List;

@Getter
@Setter
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class Zone implements IEntity {

	private static final long serialVersionUID = 1L;

	@EqualsAndHashCode.Include
	private long id;
	private String name;
	private Integer order;
	private Area area;
	private Project project;
	private List<Device> devices;
	private List<Scene> scenes;

	public boolean hasScene() {
		return scenes != null && !scenes.isEmpty();
	}

	public boolean hasDevices() {
		return devices != null && !devices.isEmpty();
	}
}
