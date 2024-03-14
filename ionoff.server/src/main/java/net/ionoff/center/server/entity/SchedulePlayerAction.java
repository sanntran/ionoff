package net.ionoff.center.server.entity;

import lombok.Getter;
import lombok.Setter;
import net.ionoff.center.shared.entity.PlayerAction;

@Getter
@Setter
public class SchedulePlayerAction extends ScheduleAction implements PlayerAction {

	private static final long serialVersionUID = 1L;
	
	public MediaPlayer player;
	private String volume;
	private String album;
	private String albumType;
}
