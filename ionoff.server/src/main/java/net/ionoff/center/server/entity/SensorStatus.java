package net.ionoff.center.server.entity;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.util.Date;

@Getter
@Setter
@Entity
@Table(name = "sensors_status")
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class SensorStatus implements IEntity {

	private static final long serialVersionUID = 1L;

	@Id
	@Column
	@EqualsAndHashCode.Include
	private long id;

	@Transient
	private String name;

	@Column(name = "time_")
	private Date time;

	@Column(name = "value_")
	private Double value;

	@Column(columnDefinition = "TINYINT(1)")
	private Boolean alert;

	@Column(name = "index_")
	private Integer index;

	@OneToOne
	@MapsId
	@JoinColumn(name = "id")
	private Sensor sensor;
}
