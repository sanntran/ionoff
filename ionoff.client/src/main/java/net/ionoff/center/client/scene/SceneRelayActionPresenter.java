package net.ionoff.center.client.scene;

import com.google.gwt.event.shared.HandlerManager;

import net.ionoff.center.client.schedule.IRelayActionView;
import net.ionoff.center.client.schedule.RelayActionPresenter;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.shared.dto.SceneActionDto;
import net.ionoff.center.shared.dto.SceneRelayActionDto;

public class SceneRelayActionPresenter extends RelayActionPresenter<SceneRelayActionDto> {
	
	private SceneRelayActionDto sceneAction;

	public SceneRelayActionPresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus,
			IRelayActionView view) {
		super(rpcProvider, eventBus, view);
	}

	@Override
	protected void setTarget(SceneRelayActionDto target) {
		sceneAction = target;
		resetLblRelayNameId();
		resetListBoxActions();
	}

	@Override
	protected String getTargetClazz() {
		return SceneRelayActionDto.class + "";
	}

	@Override
	protected Long getTargetId() {
		return sceneAction.getId();
	}

	@Override
	protected Long getTargetRelayId() {
		return sceneAction.getRelayId();
	}

	@Override
	protected String getTargetRelayName() {
		return sceneAction.getRelayName();
	}

	@Override
	protected String getTargetAction() {
		return sceneAction.getAction();
	}

	@Override
	protected String getTargetRelayType() {
		return sceneAction.getRelayType();
	}

	@Override
	protected void setTargetAction(String action) {
		sceneAction.setAction(action);
	}

	@Override
	protected void save(String action) {
		setTargetAction(action);
	}

	@Override
	public void save() {
		save(getSelectedAction());
	}

	@Override
	public SceneActionDto getTarget() {
		return sceneAction;
	}
}
