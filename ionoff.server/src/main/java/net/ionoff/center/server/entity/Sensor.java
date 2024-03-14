package net.ionoff.center.server.entity;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

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
	private Integer order;
	private String unit;
	private String type;
	private Zone zone;
	private Device device;
	private Project project;
	private Switch zwitch;
	private SensorStatus status;
}
