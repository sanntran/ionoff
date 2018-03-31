package net.ionoff.center.client.mode;


import java.util.ArrayList;
import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Panel;

import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialRow;
import net.ionoff.center.client.common.AbstractPresenter;
import net.ionoff.center.client.event.ChangeTokenEvent;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.AppToken;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.ModeDto;

public class ModeListPresenter extends AbstractPresenter {

	public interface Display {
		Panel asPanel();
		MaterialRow getWrapper();
		MaterialIcon getIconSetting();
	}
	
	private Timer timer;
	private Display display;
	private IRpcServiceProvider rpcProvider;
	private final List<ModePresenter> modePresenters;
	
	public ModeListPresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus, Display view) {		
		super(eventBus);
		this.display = view;
		this.rpcProvider = rpcProvider;
		modePresenters = new ArrayList<>();
	}
	
	private void bind() {
		timer = new Timer() {
			@Override
			public void run() {
				if (!isVisible()) {
					timer.cancel();
				}
				else {
					rpcSyncStatus();
				}
			}
		};
		display.getIconSetting().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				String token = AppToken.newModeTableToken();
				eventBus.fireEvent(new ChangeTokenEvent(token));
			}
		});
	}
	
	private void scheduleSyncStatus() {
		timer.scheduleRepeating(5000);
	}
	
	private void rpcSyncStatus() {
		rpcProvider.getModeService().findByProjectId(AppToken.getProjectIdLong(), 
				new MethodCallback<List<ModeDto>>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, List<ModeDto> response) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				updateModesStatus(response);
			}
		});
	}
	
	private void updateModesStatus(List<ModeDto> modeDtos) {
		for (final ModePresenter modePresenter : modePresenters) {
			updateModeStatus(modePresenter, modeDtos);
		}
	}

	private void updateModeStatus(ModePresenter modePresenter, List<ModeDto> modeDtos) {
		if (modePresenter.isLocked()) {
			return;
		}
		for (final ModeDto mode : modeDtos) {
			if (mode.getId() == modePresenter.getMode().getId()) {
				modePresenter.updateMode(mode);
			}
		}
	}
	
	private void rpcGetModesByProject(long projectId) {
		rpcProvider.getModeService().findByProjectId(projectId, new MethodCallback<List<ModeDto>>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, List<ModeDto> response) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				showModes(response);
			}
		});
	}

	
	private void showModes(List<ModeDto> modes) {
		for (final ModeDto mode : modes) {
			ModeView modeView = new ModeView();
			ModePresenter scenePresenter = new ModePresenter(rpcProvider, eventBus, mode, modeView);
			scenePresenter.go();
			modePresenters.add(scenePresenter);
			scenePresenter.show(display.getWrapper());
		}
		scheduleSyncStatus();
	}

	protected boolean isVisible() {
		return AppToken.hasTokenItem(AppToken.MODES) && !AppToken.hasTokenItem(AppToken.TABLE);
	}
	
	@Override
	public void go() {
		bind();
	}
	
	@Override
	public void show(HasWidgets container) {
		container.clear();
		container.add(display.asPanel());
		modePresenters.clear();
		display.getWrapper().clear();
		rpcGetModesByProject(AppToken.getProjectIdLong());
	}
}
