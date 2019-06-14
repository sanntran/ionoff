package net.ionoff.center.client.navigation;

import java.util.List;

import net.ionoff.center.client.locale.ProjectLocale;
import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Panel;
import com.google.gwt.user.client.ui.Widget;

import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.constants.ProgressType;
import gwt.material.design.client.ui.MaterialButton;
import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialImage;
import gwt.material.design.client.ui.MaterialLabel;
import gwt.material.design.client.ui.MaterialLink;
import gwt.material.design.client.ui.MaterialNavBar;
import gwt.material.design.client.ui.MaterialSideNavDrawer;
import gwt.material.design.client.ui.MaterialTitle;
import net.ionoff.center.client.base.AbstractPresenter;
import net.ionoff.center.client.event.ChangeProjectEvent;
import net.ionoff.center.client.event.ChangeTokenEvent;
import net.ionoff.center.client.event.ChangeZoneEvent;
import net.ionoff.center.client.event.ChangeZoneEventHandler;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.locale.ClientLocale;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.storage.StorageService;
import net.ionoff.center.client.utils.AppToken;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.DateTimeDto;
import net.ionoff.center.shared.dto.ProjectDto;
import net.ionoff.center.shared.dto.UserDto;
import net.ionoff.center.shared.dto.ZoneDto;

public class NavigationsPresenter extends AbstractPresenter {

	interface Display {
		Panel asPanel();
		
		MaterialNavBar getNavBar();
		MaterialButton getNavBtnLogo();
		MaterialTitle getNavTitle();
		MaterialIcon getBtnUser();

		MaterialSideNavDrawer getSideNav();
		MaterialImage getBtnImgProject();
		MaterialTitle getProfileTitle();
		MaterialIcon getIconSystemSetting();
		MaterialLabel getLblSystemTime();
		MaterialLabel getLblSystemDate();
		MaterialButton getBtnProfileTitle();
		
		MaterialLink getMenuItemProject();
		MaterialLink getMenuItemDevice();
	    MaterialLink getMenuItemScene();
	    MaterialLink getMenuItemMode();
	    MaterialLink getMenuItemSchedule();
	    MaterialLink getMenuItemController();
	    MaterialLink getMenuItemRelay();
	    MaterialLink getMenuItemUser();
	    MaterialLink getMenuItemSensor();
	    MaterialLink getMenuItemArea();
	    MaterialLink getMenuItemZone();
	    
		PopupProjectsView getPopupProjectsView();
    }

	private Display display;
	private Timer timer;
	private IRpcServiceProvider rpcService;
	private PopupUserMenuPresenter popupUserMenuPresenter;
	
	public NavigationsPresenter(IRpcServiceProvider rpcService, HandlerManager eBus, Display view) {
		super(eBus);
		this.display = view;
		this.rpcService = rpcService;
		this.timer = new Timer() {
			@Override
			public void run() {
			if (!isVisible()) {
				timer.cancel();
			}
			else {
				getServerDateTime();
			}
			}
		};
	}

	@Override
	public void go() {
		UserDto user = StorageService.getInstance().getCookie().getUser();
		if (user != null) {
			Long projectId = AppToken.getProjectIdLong();
			Long zoneId = AppToken.getZoneIdLong();
			if (zoneId != null) {
				rpcService.getZoneService().findById(zoneId, new MethodCallback<ZoneDto>() {
					@Override
					public void onFailure(Method method, Throwable exception) {
						ClientUtil.handleRpcFailure(method, exception, eventBus);
					}
					@Override
					public void onSuccess(Method method, ZoneDto response) {
						eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
						onZoneChanged(response);
						for (ProjectDto project : user.getProjects()) {
							if (projectId != null && projectId.equals(project.getId())) {
								display.getProfileTitle().setTitle(project.getName());
								display.getProfileTitle().setDescription(project.getAddress());
								break;
							}
						}
					}
				});
			}
			else {
				for (ProjectDto project : user.getProjects()) {
					if (projectId != null && projectId.equals(project.getId())) {
						onProjectChanged(project);
						break;
					}
				}
			}
		}

		ClickHandler goToProjectHandler = event -> eventBus.fireEvent(new ChangeTokenEvent(
				AppToken.newDashboardToken(StorageService.getInstance().getCookie().getProjectId())));

		display.getNavBtnLogo().addClickHandler(goToProjectHandler);
		display.getBtnImgProject().addClickHandler(goToProjectHandler);

		display.getBtnProfileTitle().addClickHandler(event -> {
			Widget source = (Widget) event.getSource();
			int left = 0;
			int top = source.getAbsoluteTop() + 50;
			showPopupProjectsView(left, top);
		});

		ClickHandler showProjectTableHandler = event ->
				eventBus.fireEvent(new ChangeTokenEvent(AppToken.newTokenProjectTable()));
		display.getIconSystemSetting().addClickHandler(showProjectTableHandler);
				
		display.getMenuItemProject().addClickHandler(showProjectTableHandler);

		display.getMenuItemDevice().addClickHandler(event ->
				eventBus.fireEvent(new ChangeTokenEvent(AppToken.newDeviceTableToken())));
		
		display.getMenuItemScene().addClickHandler(event ->
				eventBus.fireEvent(new ChangeTokenEvent(AppToken.newSceneTableToken())));
		
		display.getMenuItemMode().addClickHandler(event ->
				eventBus.fireEvent(new ChangeTokenEvent(AppToken.newModeTableToken())));
		
		display.getMenuItemSchedule().addClickHandler(event ->
				eventBus.fireEvent(new ChangeTokenEvent(AppToken.newScheduleTableToken())));
		
		display.getMenuItemController().addClickHandler(event ->
				eventBus.fireEvent(new ChangeTokenEvent(AppToken.newControllerTableToken())));
		
		display.getMenuItemRelay().addClickHandler(event ->
				eventBus.fireEvent(new ChangeTokenEvent(AppToken.newRelayTableToken())));
		
		display.getMenuItemUser().addClickHandler(event ->
				eventBus.fireEvent(new ChangeTokenEvent(AppToken.newUserTableToken())));
		
		display.getMenuItemSensor().addClickHandler(event ->
				eventBus.fireEvent(new ChangeTokenEvent(AppToken.newSensorTableToken())));
		
		display.getMenuItemArea().addClickHandler(event ->
				eventBus.fireEvent(new ChangeTokenEvent(AppToken.newAreaTableToken())));
		
		display.getMenuItemZone().addClickHandler(event ->
				eventBus.fireEvent(new ChangeTokenEvent(AppToken.newZoneTableToken())));
		
		display.getBtnUser().addClickHandler(e -> showPopupUserMenu());

		eventBus.addHandler(ChangeZoneEvent.TYPE, event ->
				onZoneChanged(event.getZone()));
	}

	private PopupUserMenuPresenter getPopupUserMenuPresenter() {
		if (popupUserMenuPresenter == null) {
			popupUserMenuPresenter = new PopupUserMenuPresenter(rpcService, eventBus, new PopupUserMenuView());
			popupUserMenuPresenter.go();
		}
		return popupUserMenuPresenter;
	}
	
	private void showPopupUserMenu() {
		getPopupUserMenuPresenter().show(null);
	}


	@Override
	public void show(HasWidgets container) {
		container.add(display.asPanel());
		
		final String userName = StorageService.getInstance().getCookie().getUser().getName();
		if (AppToken.LORD.equals(userName)) {
			display.getIconSystemSetting().setVisible(true);
		}
		else {
			display.getIconSystemSetting().setVisible(false);
		}
		
		getServerDateTime();
		timer.scheduleRepeating(25000);
	}

	private void showPopupProjectsView(int left, int top) {
		Long userId = StorageService.getInstance().getCookie().getUser().getId();
		rpcService.getUserService().getProjectsByUser(userId, new MethodCallback<List<ProjectDto>>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, List<ProjectDto> projects) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				StorageService.getInstance().getCookie().getUser().setProjects(projects);
				showPopupProjectsView(left, top, projects);
			}
		});
	}

	private void showPopupProjectsView(int left, int top, List<ProjectDto> projects) {
		Long projectId = AppToken.getProjectIdLong();
		
		display.getPopupProjectsView().getContentWrapper().clear();
		display.getPopupProjectsView().getMenuItemViews().clear();
		
		for (ProjectDto proj : projects) {
			ProjectMenuItemView projMenuItem = new ProjectMenuItemView(proj);
			if (projectId != null && projectId.equals(proj.getId())) {
				projMenuItem.setSelected(true);
			}
			projMenuItem.addClickHandler(event -> eventBus.fireEvent(new ChangeProjectEvent(proj)));
			display.getPopupProjectsView().getContentWrapper().add(projMenuItem);
			display.getPopupProjectsView().getMenuItemViews().add(projMenuItem);
		}
		
		display.getPopupProjectsView().setPopupPosition(left, top);
		display.getPopupProjectsView().show();
	}

	private void getServerDateTime() {
		rpcService.getSystemService().getServerDateTime(new MethodCallback<DateTimeDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, DateTimeDto response) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				display.getLblSystemTime().setText(response.getTime());
				display.getLblSystemDate().setText(response.getDate());
			}
		});
	}

	private boolean isVisible() {
		return !AppToken.hasTokenItem(AppToken.LOGIN);
	}


	public void showSystemMenu() {

		display.getNavTitle().setTitle(ClientLocale.getClientConst().system());
		display.getNavTitle().setDescription(ClientLocale.getClientConst().setting());

		display.getProfileTitle().setTitle(ClientLocale.getClientConst().system());
		display.getProfileTitle().setDescription(ClientLocale.getClientConst().setting());
				
		display.getMenuItemProject().setVisible(true);
		display.getMenuItemDevice().setVisible(false);
		display.getMenuItemScene().setVisible(false);
		display.getMenuItemMode().setVisible(false);
		display.getMenuItemController().setVisible(false);
		display.getMenuItemSchedule().setVisible(false);
		display.getMenuItemRelay().setVisible(false);
		display.getMenuItemUser().setVisible(true);
		display.getMenuItemSensor().setVisible(false);
		display.getMenuItemArea().setVisible(false);
		display.getMenuItemZone().setVisible(false);

		if (AppToken.hasTokenItem(AppToken.PROJECTS)) {
			display.getSideNav().setActive(1);
		}
		else if (AppToken.hasTokenItem(AppToken.USERS)) {
			display.getSideNav().setActive(9);
		}
	}

	public void showProjectMenu() {
		Long projectId = AppToken.getProjectIdLong();
		UserDto user = StorageService.getInstance().getCookie().getUser();
		for (ProjectDto project : user.getProjects()) {
			if (projectId != null && projectId.equals(project.getId())) {
				onProjectChanged(project);
				break;
			}
		}
		
		display.getMenuItemProject().setVisible(false);

		String userGroup = null;
		try {
			userGroup = StorageService.getInstance().getCookie().getUser().getGroupName();
		} catch (Exception e) {
			//
		}
		if (userGroup == null || !userGroup.contains("Admin")) {
			display.getMenuItemDevice().setVisible(false);
			display.getMenuItemScene().setVisible(false);
			display.getMenuItemMode().setVisible(false);
			display.getMenuItemController().setVisible(false);
			display.getMenuItemRelay().setVisible(false);
			display.getMenuItemUser().setVisible(false);
			display.getMenuItemSensor().setVisible(false);
			display.getMenuItemArea().setVisible(false);
			display.getMenuItemZone().setVisible(false);
		}
		else {
			display.getMenuItemDevice().setVisible(true);
			display.getMenuItemScene().setVisible(true);
			display.getMenuItemMode().setVisible(true);
			display.getMenuItemSchedule().setVisible(true);
			display.getMenuItemController().setVisible(true);
			display.getMenuItemRelay().setVisible(true);
			display.getMenuItemUser().setVisible(true);
			display.getMenuItemSensor().setVisible(true);
			display.getMenuItemArea().setVisible(true);
			display.getMenuItemZone().setVisible(true);
		}
		
		if (AppToken.hasTokenItem(AppToken.DASHBOARD)) {
			display.getSideNav().setActive(2);
		}
		else if (AppToken.hasTokenItem(AppToken.DEVICES)) {
			display.getSideNav().setActive(3);
		}
		else if (AppToken.hasTokenItem(AppToken.SCENES)) {
			display.getSideNav().setActive(4);
		}
		else if (AppToken.hasTokenItem(AppToken.MODES)) {
			display.getSideNav().setActive(5);
		}
		else if (AppToken.hasTokenItem(AppToken.SCHEDULES)) {
			display.getSideNav().setActive(6);
		}
		else if (AppToken.hasTokenItem(AppToken.CONTROLLERS)) {
			display.getSideNav().setActive(7);
		}
		else if (AppToken.hasTokenItem(AppToken.RELAYS)) {
			display.getSideNav().setActive(8);
		}
		else if (AppToken.hasTokenItem(AppToken.USERS)) {
			display.getSideNav().setActive(9);
		}
		else if (AppToken.hasTokenItem(AppToken.SENSORS)) {
			display.getSideNav().setActive(10);
		}
		else if (AppToken.hasTokenItem(AppToken.AREAS)) {
			display.getSideNav().setActive(11);
		}
		else if (AppToken.hasTokenItem(AppToken.ZONES)) {
			display.getSideNav().setActive(12);
		}
	}
	
	public void onProjectChanged(ProjectDto project) {
		display.getNavTitle().setTitle(project.getName());
		display.getNavTitle().setDescription(project.getAddress());
		
		display.getProfileTitle().setTitle(project.getName());
		display.getProfileTitle().setDescription(project.getAddress());
	}

	private void onZoneChanged(ZoneDto zone) {
		display.getNavTitle().setTitle(zone.getName());
		display.getNavTitle().setDescription(zone.getAreaName());
	}

	public void showProgress(boolean loading) {
		if (loading) {
			display.getNavBar().showProgress(ProgressType.DETERMINATE);
		}
		else {
			display.getNavBar().hideProgress();
		}
	}
}