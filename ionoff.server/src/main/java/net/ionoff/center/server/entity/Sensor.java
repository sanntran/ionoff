package net.ionoff.center.server.entity;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.util.Date;
import java.util.Objects;

@Getter
@Setter
@Entity
@Table(name = "sensors")
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class Sensor implements IEntity {

	private static final long serialVersionUID = 1L;

	public static enum TYPE {
		ANALOG, DIGITAL;
	}

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column
	@EqualsAndHashCode.Include
	private long id;

	@Column
	private String name;

	@Column(name = "input")
	private int index;

	@Column(name = "idx")
	private Integer order;

	@Column
	private String unit;

	@Column
	private String type;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "zone_id", referencedColumnName = "id")
	private Zone zone;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "device_id", referencedColumnName = "id")
	private Device device;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "project_id", referencedColumnName = "id")
	private Project project;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "controller_id", referencedColumnName = "id")
	private Controller controller;

	@OneToOne(mappedBy = "sensor", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
	@PrimaryKeyJoinColumn
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
