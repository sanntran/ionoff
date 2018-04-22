package net.ionoff.center.client.navigation;

import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.i18n.client.LocaleInfo;
import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Panel;
import com.google.gwt.user.client.ui.Widget;

import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.constants.ProgressType;
import gwt.material.design.client.ui.MaterialButton;
import gwt.material.design.client.ui.MaterialCheckBox;
import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialImage;
import gwt.material.design.client.ui.MaterialLabel;
import gwt.material.design.client.ui.MaterialLink;
import gwt.material.design.client.ui.MaterialNavBar;
import gwt.material.design.client.ui.MaterialSideNavDrawer;
import gwt.material.design.client.ui.MaterialTitle;
import net.ionoff.center.client.base.AbstractPresenter;
import net.ionoff.center.client.event.ChangeLanguageEvent;
import net.ionoff.center.client.event.ChangeProjectEvent;
import net.ionoff.center.client.event.ChangeTokenEvent;
import net.ionoff.center.client.event.ChangeZoneEvent;
import net.ionoff.center.client.event.ChangeZoneEventHandler;
import net.ionoff.center.client.event.FullscreenEvent;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.event.ShowMessageEvent;
import net.ionoff.center.client.event.UserLogOutEvent;
import net.ionoff.center.client.locale.ClientLocale;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.storage.StorageService;
import net.ionoff.center.client.utils.AppToken;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.DateTimeDto;
import net.ionoff.center.shared.dto.ProjectDto;
import net.ionoff.center.shared.dto.UserDto;
import net.ionoff.center.shared.dto.VersionDto;
import net.ionoff.center.shared.dto.ZoneDto;

public class NavigationsPresenter extends AbstractPresenter {

	interface Display {
		Panel asPanel();
		
		MaterialNavBar getNavBar();
		MaterialButton getNavBtnLogo();
		MaterialIcon getNavIconDashhboard();
		MaterialButton getBtnNavTitle();
		MaterialTitle getNavTitle();
		MaterialIcon getBtnUser();
		MaterialTitle getLblUser();
		MaterialLink getUserMenuItemFullscreen();
		MaterialLink getUserMenuItemLogout();
		MaterialLink getUserMenuItemVersion();
		MaterialCheckBox getUserMenuItemVi();
		MaterialCheckBox getUserMenuItemEn();

		MaterialSideNavDrawer getSideNav();
		MaterialImage getBtnImgProject();
		MaterialTitle getProfileTitle();
		MaterialIcon getIconSelectProject();
		MaterialIcon getIconSystemSetting();
		MaterialLabel getLblSystemTime();
		MaterialLabel getLblSystemDate();
		MaterialButton getBtnProfileTitle();
		
		MaterialLink getMenuItemProject();
		MaterialLink getMenuItemDashboard();
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

		MaterialLink getMenuItemArrow();
    }
	
	private String latestVersion;
	private IRpcServiceProvider rpcService;
	private Display display;
	private Timer timer;
	private boolean expandMenu;
	
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
		expandMenu = false;
	}


	@Override
	public void go() {
		UserDto user = StorageService.getInstance().getCookie().getUser();
		if ( user != null) {
			display.getLblUser().setTitle(StorageService.getInstance().getCookie().getUser().getFullName());
			display.getLblUser().setDescription(StorageService.getInstance().getCookie().getUser().getName());
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
		
		display.getNavBtnLogo().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				eventBus.fireEvent(new ChangeTokenEvent(
						AppToken.newDashboardToken(StorageService.getInstance().getCookie().getProjectId())));
			}
		});
		
		display.getNavIconDashhboard().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (AppToken.hasTokenItem(AppToken.PROJECT)) {
					eventBus.fireEvent(new ChangeTokenEvent(AppToken.newDashboardToken()));
				}
			}
		});
		
		display.getBtnNavTitle().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (AppToken.hasTokenItem(AppToken.SYSTEM)) {
					return;
				}
				String token = AppToken.newZoneListToken();
				eventBus.fireEvent(new ChangeTokenEvent(token));
			}
		});
		
		display.getBtnImgProject().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				eventBus.fireEvent(new ChangeTokenEvent(
						AppToken.newDashboardToken(StorageService.getInstance().getCookie().getProjectId())));
			}
		});
		
		display.getIconSelectProject().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				Widget source = (Widget) event.getSource();
				int left = 0;
		        int top = source.getAbsoluteTop() + 57;
		        showPopupProjectsView(left, top);
			}
		});
		
		display.getBtnProfileTitle().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				String token = AppToken.newZoneListToken();
				eventBus.fireEvent(new ChangeTokenEvent(token));
			}
		});
		
		display.getIconSystemSetting().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				eventBus.fireEvent(new ChangeTokenEvent(AppToken.newTokenProjects()));
			}
		});
		
		display.getUserMenuItemLogout().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				eventBus.fireEvent(new UserLogOutEvent());
			}
		});
		
		display.getUserMenuItemVersion().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (latestVersion == null) {
					checkLatestVersion();
				}
				else {
					upgradeNewVersion();
				}
			}
		});
		
		display.getUserMenuItemFullscreen().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				eventBus.fireEvent(new FullscreenEvent());
			}
		});
		
		display.getUserMenuItemVi().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (!ClientLocale.vi_VN.equals(getLanguage())) {
					changeLanguage(ClientLocale.vi_VN);
				}
			}
		});
		
		display.getUserMenuItemEn().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (!ClientLocale.en_EN.equals(getLanguage())) {
					changeLanguage(ClientLocale.en_EN);
				}
			}
		});
		
		display.getMenuItemProject().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				eventBus.fireEvent(new ChangeTokenEvent(AppToken.newTokenProjects()));
			}
		});
		
		display.getMenuItemDashboard().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				eventBus.fireEvent(new ChangeTokenEvent(AppToken.newDashboardToken()));
			}
		});
		
		display.getMenuItemDevice().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				eventBus.fireEvent(new ChangeTokenEvent(AppToken.newDevicesToken()));
			}
		});
		
		display.getMenuItemScene().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				eventBus.fireEvent(new ChangeTokenEvent(AppToken.newSceneToken()));
			}
		});
		
		display.getMenuItemMode().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				eventBus.fireEvent(new ChangeTokenEvent(AppToken.newModeListToken()));
			}
		});
		
		display.getMenuItemSchedule().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				eventBus.fireEvent(new ChangeTokenEvent(AppToken.newScheduleToken()));
			}
		});
		
		display.getMenuItemController().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				eventBus.fireEvent(new ChangeTokenEvent(AppToken.newControllerToken()));
			}
		});
		
		display.getMenuItemRelay().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				eventBus.fireEvent(new ChangeTokenEvent(AppToken.newRelayToken()));
			}
		});
		
		display.getMenuItemUser().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				eventBus.fireEvent(new ChangeTokenEvent(AppToken.newUserToken()));
			}
		});
		
		display.getMenuItemSensor().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				eventBus.fireEvent(new ChangeTokenEvent(AppToken.newSensorToken()));
			}
		});
		
		display.getMenuItemArea().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				eventBus.fireEvent(new ChangeTokenEvent(AppToken.newAreaToken()));
			}
		});
		
		display.getMenuItemZone().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				eventBus.fireEvent(new ChangeTokenEvent(AppToken.newZoneListToken()));
			}
		});

		display.getMenuItemArrow().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (expandMenu == true) {
					display.getMenuItemArea().setVisible(false);
					display.getMenuItemZone().setVisible(false);
					display.getMenuItemSensor().setVisible(false);
					display.getMenuItemUser().setVisible(false);
					display.getMenuItemRelay().setVisible(false);
					display.getMenuItemController().setVisible(false);
					display.getMenuItemArrow().setIconType(IconType.KEYBOARD_ARROW_DOWN);
					expandMenu = false;
				}
				else {
					display.getMenuItemArea().setVisible(true);
					display.getMenuItemZone().setVisible(true);
					display.getMenuItemSensor().setVisible(true);
					display.getMenuItemUser().setVisible(true);
					display.getMenuItemRelay().setVisible(true);
					display.getMenuItemController().setVisible(true);
					display.getMenuItemArrow().setIconType(IconType.KEYBOARD_ARROW_UP);
					expandMenu = true;
				}
			}
		});
		
		display.getMenuItemArea().setVisible(false);
		display.getMenuItemZone().setVisible(false);
		display.getMenuItemSensor().setVisible(false);
		display.getMenuItemUser().setVisible(false);
		display.getMenuItemRelay().setVisible(false);
		display.getMenuItemController().setVisible(false);
		display.getMenuItemArrow().setVisible(false);
		
		eventBus.addHandler(ChangeZoneEvent.TYPE, new ChangeZoneEventHandler() {
			@Override
			public void onChangeZone(ChangeZoneEvent event) {
				onZoneChanged(event.getZone());
			}
		});
	}

	@Override
	public void show(HasWidgets container) {
		container.add(display.asPanel());
		if (ClientLocale.en_EN.equals(getLanguage())) {
			display.getUserMenuItemEn().setValue(true);
			display.getUserMenuItemVi().setValue(false);
		}
		else {
			display.getUserMenuItemEn().setValue(false);
			display.getUserMenuItemVi().setValue(true);
		}
		
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
	
	public void onClickProjectImage() {
		final String userName = StorageService.getInstance().getCookie().getUser().getName();
		if (AppToken.LORD.equals(userName)) {
			eventBus.fireEvent(new ChangeTokenEvent(AppToken.newTokenProjects()));
		}
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
			projMenuItem.addClickHandler(new ClickHandler() {
				@Override
				public void onClick(ClickEvent event) {
					eventBus.fireEvent(new ChangeProjectEvent(proj));
				}
			});
			display.getPopupProjectsView().getContentWrapper().add(projMenuItem);
			display.getPopupProjectsView().getMenuItemViews().add(projMenuItem);
		}
		
		display.getPopupProjectsView().setPopupPosition(left, top);
		display.getPopupProjectsView().show();
	}
	
	private void checkLatestVersion() {
		rpcService.getUserService().checkLatestVersion(new MethodCallback<VersionDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, VersionDto response) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				latestVersion = response.getName();
				if (latestVersion == null) {
					display.getUserMenuItemVersion().setText(ClientLocale.getClientConst().checkLatestVersion());
				}
				else {
					display.getUserMenuItemVersion().setText(ClientLocale.getClientConst().upgrade() + latestVersion);
				}
				if (latestVersion == null) {
					eventBus.fireEvent(new ShowMessageEvent(ClientLocale.getClientMessage()
							.currentVersionIsUptoDate(), ShowMessageEvent.NORMAL));
				}
			}
		});
	}

	private void upgradeNewVersion() {
		if (latestVersion == null) {
			return;
		}
		rpcService.getUserService().updateNewVersion(new MethodCallback<VersionDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, VersionDto response) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				eventBus.fireEvent(new ShowMessageEvent(ClientLocale.getClientMessage()
						.upgradingNewVersion(latestVersion), ShowMessageEvent.NORMAL));
			}
		});
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

	private void changeLanguage(String language) {
		eventBus.fireEvent(new ChangeLanguageEvent(language));
	}

	private String getLanguage() {
		String language = ClientLocale.vi_VN;
		if (LocaleInfo.getCurrentLocale().getLocaleName().equals(ClientLocale.en_EN)) {
			language = ClientLocale.en_EN;
		}
		return language;
	}

	private boolean isVisible() {
		return !AppToken.hasTokenItem(AppToken.LOGIN);
	}


	public void showSystemMenu() {
		display.getBtnNavTitle().removeStyleName("zone");
		
		display.getNavTitle().setTitle(ClientLocale.getClientConst().system());
		display.getNavTitle().setDescription(ClientLocale.getClientConst().setting());

		display.getProfileTitle().setTitle(ClientLocale.getClientConst().system());
		display.getProfileTitle().setDescription(ClientLocale.getClientConst().setting());
				
		display.getNavIconDashhboard().setIconType(IconType.SETTINGS);
		display.getMenuItemProject().setVisible(true);
		display.getMenuItemDashboard().setVisible(false);
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
		display.getMenuItemArrow().setVisible(false);
		
		if (AppToken.hasTokenItem(AppToken.PROJECTS)) {
			display.getSideNav().setActive(1);
		}
		else if (AppToken.hasTokenItem(AppToken.USERS)) {
			display.getSideNav().setActive(9);
		}
	}

	public void showProjectMenu() {
		display.getBtnNavTitle().removeStyleName("zone");
		
		Long projectId = AppToken.getProjectIdLong();
		UserDto user = StorageService.getInstance().getCookie().getUser();
		for (ProjectDto project : user.getProjects()) {
			if (projectId != null && projectId.equals(project.getId())) {
				onProjectChanged(project);
				break;
			}
		}
		
		display.getNavIconDashhboard().setIconType(IconType.DASHBOARD);
		display.getMenuItemProject().setVisible(false);
		display.getMenuItemDashboard().setVisible(true);
		display.getMenuItemDevice().setVisible(true);
		display.getMenuItemScene().setVisible(true);
		display.getMenuItemMode().setVisible(true);
		display.getMenuItemSchedule().setVisible(true);
		
		display.getMenuItemArrow().setVisible(true);
		if (expandMenu) {
			display.getMenuItemController().setVisible(true);
			display.getMenuItemRelay().setVisible(true);
			display.getMenuItemUser().setVisible(true);
			display.getMenuItemSensor().setVisible(true);
			display.getMenuItemArea().setVisible(true);
			display.getMenuItemZone().setVisible(true);
		}
		else {
			display.getMenuItemController().setVisible(false);
			display.getMenuItemRelay().setVisible(false);
			display.getMenuItemUser().setVisible(false);
			display.getMenuItemSensor().setVisible(false);
			display.getMenuItemArea().setVisible(false);
			display.getMenuItemZone().setVisible(false);
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
		
		display.getProfileTitle().setTitle(zone.getName());
		display.getProfileTitle().setDescription(zone.getAreaName());
	}
	
	public void showZoneMenu() {
		display.getBtnNavTitle().removeStyleName("zone");
		display.getBtnNavTitle().addStyleName("zone");
		
		display.getNavIconDashhboard().setIconType(IconType.DASHBOARD);
		display.getMenuItemProject().setVisible(false);
		display.getMenuItemDashboard().setVisible(true);
		display.getMenuItemDevice().setVisible(true);
		display.getMenuItemScene().setVisible(true);
		display.getMenuItemController().setVisible(false);
		display.getMenuItemMode().setVisible(false);
		display.getMenuItemSchedule().setVisible(false);
		display.getMenuItemRelay().setVisible(false);
		display.getMenuItemUser().setVisible(false);
		display.getMenuItemSensor().setVisible(false);
		display.getMenuItemArea().setVisible(false);
		display.getMenuItemZone().setVisible(false);
		display.getMenuItemArrow().setVisible(false);

		if (AppToken.hasTokenItem(AppToken.DASHBOARD)) {
			display.getSideNav().setActive(2);
		}
		else if (AppToken.hasTokenItem(AppToken.DEVICES)) {
			display.getSideNav().setActive(3);
		}
		else if (AppToken.hasTokenItem(AppToken.SCENES)) {
			display.getSideNav().setActive(4);
		}
		else if (AppToken.hasTokenItem(AppToken.SCHEDULES)) {
			display.getSideNav().setActive(6);
		}
		else if (AppToken.hasTokenItem(AppToken.SENSORS)) {
			display.getSideNav().setActive(10);
		}
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