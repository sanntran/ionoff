package net.ionoff.center.server.entity;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import java.util.Date;

@Getter
@Setter
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class SensorStatus implements IEntity {

	private static final long serialVersionUID = 1L;

	@EqualsAndHashCode.Include
	private long id;
	private String name;
	private Date time;
	private Double value;
	private Boolean alert;
	private Long index;
	private Sensor sensor;
}
