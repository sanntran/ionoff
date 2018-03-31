package net.ionoff.center.client.scene;

import com.google.gwt.event.shared.HandlerManager;

import net.ionoff.center.client.schedule.IPlayerActionView;
import net.ionoff.center.client.schedule.PlayerActionPresenter;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.shared.dto.SceneActionDto;
import net.ionoff.center.shared.dto.ScenePlayerActionDto;

public class ScenePlayerActionPresenter extends PlayerActionPresenter<ScenePlayerActionDto> {
	
	private ScenePlayerActionDto sceneAction;
	
	public ScenePlayerActionPresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus,
			IPlayerActionView view) {
		super(rpcProvider, eventBus, view);
	}

	@Override
	protected void setTarget(ScenePlayerActionDto target) {
		sceneAction = target;
		updateDisplay();
	}

	@Override
	protected String getTargetClazz() {
		return ScenePlayerActionDto.class + "";
	}

	@Override
	protected Long getPlayerId() {
		return sceneAction.getPlayerId();
	}

	@Override
	protected Long getTargetId() {
		return sceneAction.getId();
	}

	@Override
	protected String getTargetAction() {
		return sceneAction.getAction();
	}

	@Override
	protected String getTargetAlbumType() {
		return sceneAction.getAlbumType();
	}

	@Override
	protected void setTargetVolume(String volume) {
		sceneAction.setVolume(volume);
	}

	@Override
	protected void setTargetAction(String action) {
		sceneAction.setAction(action);
	}

	@Override
	protected void setTargetAlbumType(String albumType) {
		sceneAction.setAlbumType(albumType);
	}

	@Override
	protected String getTargetVolume() {
		return sceneAction.getVolume();
	}

	@Override
	protected String getTargetAlbum() {
		return sceneAction.getAlbum();
	}

	@Override
	protected String getPlayerName() {
		return sceneAction.getPlayerName();
	}

	@Override
	protected void setTargetAlbum(String album) {
		sceneAction.setAlbum(album);
	}

	@Override
	public void save() {
		sceneAction.setAction(getTargetAction());
		sceneAction.setVolume(display.getIntBoxVolume().getText());
		sceneAction.setAlbum(display.getTextBoxAlbum().getText());
		sceneAction.setAlbumType(getTargetAlbumType());
	}

	@Override
	public SceneActionDto getTarget() {
		return sceneAction;
	}
}
