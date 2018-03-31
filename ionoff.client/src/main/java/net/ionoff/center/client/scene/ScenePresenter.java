package net.ionoff.center.client.scene;

import java.util.Map;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.HTMLPanel;
import com.google.gwt.user.client.ui.HasWidgets;

import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialLabel;
import net.ionoff.center.client.common.AbstractPresenter;
import net.ionoff.center.client.event.ShowMessageEvent;
import net.ionoff.center.client.locale.ProjectLocale;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.SceneDto;

public class ScenePresenter extends AbstractPresenter {
	
	public interface Display {
		HTMLPanel asPanel();
		MaterialIcon getIcon();
		MaterialLabel getLblName();
		MaterialLabel getLblTime();
		MaterialIcon getBtnPlay();
	}
	
	private Display display;
	private final IRpcServiceProvider rpcService;
	private final SceneDto scene;
	private boolean locked; // lock when sending request of control 

	public ScenePresenter(IRpcServiceProvider rpcService, HandlerManager eventBus, SceneDto scene, Display view) {
		super(eventBus);
		this.setLocked(false);
		this.rpcService = rpcService;
		this.scene = scene;
		this.display = view;
	}

	@Override
	public void go() {
		bind();
	}
	
	public void bind() {
		display.getLblName().setText(scene.getName());
		if (scene.getTime() != null) {
			display.getLblTime().setText(scene.getTime());
		}
		
		display.getBtnPlay().addClickHandler((e) -> playScene());
	}

	private void playScene() {
		rpcService.getSceneService().playById(scene.getId(), new MethodCallback<Map<String, Boolean>>() {

			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}

			@Override
			public void onSuccess(Method method, Map<String, Boolean> response) {
				String message = ProjectLocale.getProjectMessages().performedScene(scene.getName());
				eventBus.fireEvent(new ShowMessageEvent(message, ShowMessageEvent.SUCCESS));
			}
		});
	}

	@Override
	public void show(HasWidgets container) {
		container.add(display.asPanel());
	}

	protected IRpcServiceProvider getRpcProvider() {
		return rpcService;
	}

	public SceneDto getScene() {
		return scene;
	}
	
	public void updateScene(SceneDto sceneDto) {
		scene.setTime(sceneDto.getTime());
		if (scene.getTime() != null) {
			display.getLblTime().setText(scene.getTime());
		}
	}

	public boolean isLocked() {
		return locked;
	}

	public void setLocked(boolean lock) {
		locked = lock;
	}
}
