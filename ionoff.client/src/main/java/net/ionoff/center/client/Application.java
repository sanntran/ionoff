package net.ionoff.center.client;

import com.google.gwt.core.client.GWT;
import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.event.logical.shared.ValueChangeHandler;
import com.google.gwt.event.shared.HandlerManager;
import org.fusesource.restygwt.client.Defaults;

import com.google.gwt.user.client.History;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.inject.Inject;

import net.ionoff.center.client.event.ChangeTokenEvent;
import net.ionoff.center.client.event.UserEnterEvent;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.storage.StorageService;
import net.ionoff.center.client.utils.AppToken;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.client.utils.ConsoleLog;
import net.ionoff.center.client.utils.TokenUtil;
import net.ionoff.center.shared.dto.ProjectDto;

import java.util.HashMap;

/**
 * @author Sann Tran
 */
public class Application implements IClientApp, ValueChangeHandler<String> {

	protected IRpcServiceProvider rpcProvider;

	@Inject
	protected HandlerManager eventBus;

	@Inject
	protected IAppEventHandler eventHandler;

	@Inject
	protected IAppController appController;

	@Inject
	protected RestyGwtConfig restyGwtConfig;

	@Inject
	public Application(IRpcServiceProvider rpcService) {
		this.rpcProvider = rpcProvider;
	}

	@Override
	public void onValueChange(ValueChangeEvent<String> event) {
		if (TokenUtil.getSourceChangeTokenEvent() == true) {
			handleHistoryTokenChanged();
		}
	}

	@Override
	public HashMap<String, String> getParamsMap(String procedure) {
		return ClientUtil.getParamsMap(eventBus, procedure);
	}

	@Override
	public String getBaseUrl() {
		return GWT.getModuleBaseURL().replaceAll((GWT.getModuleName() + "/"), "");
	}

	@Override
	public void go() {
		eventHandler.bind();
		History.addValueChangeHandler(this);
		Defaults.setDispatcher(restyGwtConfig.getDispatcher());

		try {
			StorageService.getInstance().loadStorage();
		} catch (Exception e) {
			GWT.log("Error loading local storage: " + e.getMessage(), e);
			appController.showLogin();
			return;
		}

		if (StorageService.getInstance().getCookie().getJwtToken() == null) {
			if (AppToken.LOGIN.equals(AppToken.getToken())) {
				appController.showLogin();
			}
			else {
				TokenUtil.setSourceChangeTokenEvent(true);
				History.newItem(AppToken.LOGIN);
			}
		}
		else {
			eventBus.fireEvent(new UserEnterEvent());
			startApplication();
		}
	}
	
	protected void startApplication() {
		String token = History.getToken();
		if (AppToken.PROJECTS.equals(token) &&
				AppToken.LORD.equals(StorageService.getInstance().getCookie().getUser().getName())) {
				handleHistoryTokenChanged();
		}
		else if (AppToken.getProjectId().equals(StorageService.getInstance().getCookie().getProjectId() + "")) {
			handleHistoryTokenChanged();
		}
		else {
			String historyToken = StorageService.getInstance().getCookie().getHistoryToken();
			if (historyToken != null && !historyToken.equals(token)) {
				eventBus.fireEvent(new ChangeTokenEvent(historyToken));
			}
			else {
				eventBus.fireEvent(new ChangeTokenEvent(AppToken.newDashboardToken(StorageService.getInstance().getCookie().getProjectId())));
			}
		}
	}

	@Override
	public void showMain(ProjectDto project) {
		ConsoleLog.println(AppToken.getToken());
		Long projectId = StorageService.getInstance().getCookie().getProjectId();
		if (!AppToken.isValidToken(AppToken.getToken()) || 
				!String.valueOf(projectId).equals(AppToken.getProjectId())) {
			String token = AppToken.newDashboardToken(projectId);
			TokenUtil.setSourceChangeTokenEvent(true);
			History.newItem(token);
		}
		else if (isDifferentId(projectId, AppToken.getProjectId())) {
			TokenUtil.setSourceChangeTokenEvent(true);
			History.newItem(AppToken.changeTokenProject(String.valueOf(projectId)));
		}
		else {
			handleHistoryTokenChanged();
		}
	}
	
	private boolean isDifferentId(Long cookieProjectId, String tokenProjectId) {
		return ClientUtil.isLongNumber(tokenProjectId) && !tokenProjectId.equals(cookieProjectId + "");
	}
	
	@Override
	public void handleHistoryTokenChanged() {
		TokenUtil.setSourceChangeTokenEvent(false);
		String token = AppToken.getToken();
		StorageService.getInstance().getCookie().setHistoryToken(token);
		StorageService.getInstance().writeCookie();
		
		if (AppToken.LOGIN.equals(token)) {
			appController.showLogin();
			return;
		}
		
		if (AppToken.hasTokenItem(AppToken.SYSTEM)) {
			if (AppToken.hasTokenItem(AppToken.PROJECTS)) {
				appController.showProjects();
			}
			else if (AppToken.hasTokenItem(AppToken.USERS)) {
				appController.showUsers();
			}
			appController.showSystemMenu();
		}
		if (AppToken.hasTokenItem(AppToken.PROJECT)) {
			if (AppToken.hasTokenItem(AppToken.DASHBOARD)) {
				appController.showDashboard();
			}
			else if (AppToken.hasTokenItem(AppToken.DEVICES)) {
				appController.showDevices();
			}
			else if (AppToken.hasTokenItem(AppToken.DEVICE)) {
				appController.showDevice();
			}
			else if (AppToken.hasTokenItem(AppToken.SCENES)) {
				appController.showScenes();
			}
			else if (AppToken.hasTokenItem(AppToken.SCHEDULES)) {
				appController.showSchedules();
			}
			else if (AppToken.hasTokenItem(AppToken.MODES)) {
				appController.showModes();
			}
			else if (AppToken.hasTokenItem(AppToken.CONTROLLERS)) {
				appController.showControllers();
			}
			else if (AppToken.hasTokenItem(AppToken.RELAYS)) {
				appController.showRelays();
			}
			else if (AppToken.hasTokenItem(AppToken.USERS)) {
				appController.showUsers();
			}
			else if (AppToken.hasTokenItem(AppToken.SENSORS)) {
				appController.showSensors();
			}
			else if (AppToken.hasTokenItem(AppToken.AREAS)) {
				if (AppToken.hasTokenItem(AppToken.TABLE)) {
					appController.showAreasTable();
				} else {
					appController.showAreasGrid();
				}

			}
			else if (AppToken.hasTokenItem(AppToken.ZONES)) {
				appController.showZones();
			}
			else {
				String newToken = AppToken.newDashboardToken(StorageService.getInstance().getCookie().getProjectId());
				eventBus.fireEvent(new ChangeTokenEvent(newToken));
			}
			if (AppToken.hasTokenItem(AppToken.ZONE)) {
				appController.showZoneMenu();
			}
			else {
				appController.showProjectMenu();
			}
		}
	}

	@Override
	public void showLogin() {
		appController.showLogin();
	}

	@Override
	public void show(HasWidgets container) {
		//
	}
}
