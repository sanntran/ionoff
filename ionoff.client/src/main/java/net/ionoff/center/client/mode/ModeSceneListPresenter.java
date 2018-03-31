package net.ionoff.center.client.mode;

import java.util.List;

import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HasWidgets;

import net.ionoff.center.client.common.AbstractPresenter;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.shared.dto.ModeSceneDto;

public class ModeSceneListPresenter extends AbstractPresenter {
	
	public interface Display  {
		FlowPanel asPanel();
	}
	
	private final IRpcServiceProvider rpcProvider;
	private final Display display;
	
	public ModeSceneListPresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus,
			Display view) {
		super(eventBus);
		this.rpcProvider= rpcProvider;
		this.display = view;
	}
	
	public void setModeAreaScenes(List<ModeSceneDto> modeSceneDtos) {
		display.asPanel().clear();
		if (modeSceneDtos == null) {
			return;
		}
		for (ModeSceneDto modeSceneDto : modeSceneDtos) {
			ModeSceneView areaSceneView = new ModeSceneView();
			ModeScenePresenter areaScenePresenter = new ModeScenePresenter(rpcProvider, eventBus, areaSceneView);
			areaScenePresenter.setModeScene(modeSceneDto);
			areaScenePresenter.go();
			display.asPanel().add(areaSceneView);
		}
	}
	
	@Override
	public void go() {
	}

	@Override
	public void show(HasWidgets container) {
		container.clear();
		container.add(display.asPanel());
	}
}
