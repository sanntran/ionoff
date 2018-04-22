package net.ionoff.center.client.schedule;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HasWidgets;

import net.ionoff.center.client.base.AbstractPresenter;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.shared.dto.ScheduleActionDto;
import net.ionoff.center.shared.dto.SchedulePlayerActionDto;
import net.ionoff.center.shared.dto.ScheduleRelayActionDto;

public class ScheduleActionsPresenter extends AbstractPresenter {
	
	public interface Display  {
		FlowPanel asPanel();
		FlowPanel getContainer();
	}
	
	private final IRpcServiceProvider rpcProvider;
	private final Display display;
	private final List<SceneActionPresenter> ationPresenters;
	
	public ScheduleActionsPresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus,
			Display view) {
		super(eventBus);
		this.rpcProvider= rpcProvider;
		this.display = view;
		ationPresenters = new ArrayList<SceneActionPresenter>();
	}
	
	public void setScheduleActions(List<ScheduleActionDto> scheduleActions) {
		ationPresenters.clear();
		display.getContainer().clear();
		if (scheduleActions == null) {
			return;
		}
		for (ScheduleActionDto scheduleAction : scheduleActions) {
			if (scheduleAction instanceof ScheduleRelayActionDto) {
				RelayActionView actionView = new RelayActionView();
				ScheduleRelayActionPresenter actionPresenter = new ScheduleRelayActionPresenter(rpcProvider, eventBus, actionView);
				ationPresenters.add(actionPresenter);
				actionPresenter.setTarget((ScheduleRelayActionDto)scheduleAction);
				actionPresenter.go();
				display.getContainer().add(actionView);
			}
			else if (scheduleAction instanceof SchedulePlayerActionDto) {
				PlayerActionView actionView = new PlayerActionView();
				SchedulePlayerActionPresenter actionPresenter = new SchedulePlayerActionPresenter(rpcProvider, eventBus, actionView);
				ationPresenters.add(actionPresenter);
				actionPresenter.setTarget((SchedulePlayerActionDto)scheduleAction);
				actionPresenter.go();
				display.getContainer().add(actionView);
			}
		}
	}
	
	public void save() {
		for (SceneActionPresenter ationPresenter : ationPresenters) {
			ationPresenter.save();
		}
	}
	
	public void hide() {
		display.getContainer().clear();
		display.asPanel().setVisible(false);
	}
	
	public void show() {
		display.asPanel().setVisible(true);
	}
	
	@Override
	public void go() {
	}

	@Override
	public void show(HasWidgets container) {
		//
	}
}
