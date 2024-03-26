package net.ionoff.center.server.entity;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import java.util.Date;
import java.util.Objects;

@Getter
@Setter
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class Sensor implements IEntity {

	private static final long serialVersionUID = 1L;

	public static enum TYPE {
		ANALOG, DIGITAL;
	}
	@EqualsAndHashCode.Include
	private long id;
	private String name;
	private int index;
	private Integer order;
	private String unit;
	private String type;
	private Zone zone;
	private Device device;
	private Project project;
	private Controller controller;
	private SensorStatus status;

	public boolean updateStatus(Double value) {
		if (!Objects.equals(value, status.getValue())) {
			status.setValue(value);
			status.setTime(new Date());
			return true;
		} else {
			status.setTime(new Date());
			return false;
		}
	}
}
