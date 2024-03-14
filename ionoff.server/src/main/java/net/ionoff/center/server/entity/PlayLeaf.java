package net.ionoff.center.server.entity;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

@Getter
@Setter
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class PlayLeaf implements IEntity {

	private static final long serialVersionUID = 1L;

	public enum TYPE {
		file, youtube;
	}
	@EqualsAndHashCode.Include
	private long id;
	private String name;
	private Integer idx;
	private String type;
	private String image;
	private String duration;
	private String url;
	private String mrl;
	private String authors;
	private String artists;
	private PlayNode playNode;
}