package net.ionoff.center.client.event;

import com.google.gwt.event.shared.GwtEvent;

import net.ionoff.center.shared.dto.ProjectDto;

public class ChangeProjectEvent extends GwtEvent<ChangeProjectEventHandler> {
	public static Type<ChangeProjectEventHandler> TYPE = new Type<ChangeProjectEventHandler>();

	private final ProjectDto project;
	
	public ChangeProjectEvent(ProjectDto project) {
		this.project = project;
	}

	public ProjectDto getProject() {
		return project;
	}

	@Override
	public Type<ChangeProjectEventHandler> getAssociatedType() {
		return TYPE;
	}

	@Override
	protected void dispatch(ChangeProjectEventHandler handler) {
		handler.onChangeProject(this);
	}
}
