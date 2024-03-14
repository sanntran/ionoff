package net.ionoff.center.server.entity;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class SceneAction implements IEntity {

	private static final long serialVersionUID = 1L;
	
	public static final String NONE = "None";

	@EqualsAndHashCode.Include
	private long id;
	private String name;
	private String action;
	private SceneDevice sceneDevice;
	
	public boolean hasAction() {
		return action != null && !action.equals(NONE);
	}
}
