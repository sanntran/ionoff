package net.ionoff.center.client;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.History;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.RootPanel;
import com.google.inject.Inject;

import net.ionoff.center.client.content.ContentPresenter;
import net.ionoff.center.client.content.ContentView;
import net.ionoff.center.client.event.ChangeTokenEvent;
import net.ionoff.center.client.login.LoginPresenter;
import net.ionoff.center.client.login.LoginView;
import net.ionoff.center.client.navigation.NavigationsPresenter;
import net.ionoff.center.client.navigation.NavigationsView;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.storage.StorageService;
import net.ionoff.center.client.utils.AppToken;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.ProjectDto;
import net.ionoff.center.shared.dto.UserDto;


public class AppControllerImpl implements IAppController {
	
	private LoginPresenter loginPresenter;
	private NavigationsPresenter navigationsPresenter;
	private ContentPresenter contentPresenter;
	
	protected IRpcServiceProvider rpcProvider;	
	protected HandlerManager eventBus;
	
	@Inject
	public AppControllerImpl(IRpcServiceProvider rpcProvider, HandlerManager eventBus) {
		this.eventBus = eventBus;
		this.rpcProvider = rpcProvider;
	}

	@Override
	public void logout() {
		StorageService.getInstance().detroyCookie();
		String url = ClientUtil.getClientUrl();
		Window.open(url, "_self", "");
	}
	
	@Override
	public void changeLanguage(String language) {
		
		rpcProvider.getUserService().changeLanguage(StorageService.getInstance().getCookie().getUser().getId(), language, new MethodCallback<UserDto>() {

			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}

			@Override
			public void onSuccess(Method method, UserDto response) {
				StorageService.getInstance().getCookie().getUser().setLanguage(language);
				StorageService.getInstance().writeCookie();
				String url = ClientUtil.getUrl(language, "index.html");
				url = url + "#" + History.getToken();
				Window.open(url, "_self", "");
			}
		});
		
	}

	private NavigationsPresenter getNavigationsPresenter() {
		if (navigationsPresenter == null) {
			navigationsPresenter = new NavigationsPresenter(rpcProvider, eventBus, new NavigationsView());
			navigationsPresenter.go();
		}
		return navigationsPresenter;
	}

	private LoginPresenter getLoginPresenter() {
		if (loginPresenter == null) {
			loginPresenter = new LoginPresenter(rpcProvider.getLoginService(), eventBus, new LoginView());
			loginPresenter.go();
		}
		return loginPresenter;
	}

	private ContentPresenter getContentPresenter() {
		if (contentPresenter == null) {
			contentPresenter = new ContentPresenter(rpcProvider, eventBus, new ContentView());
			contentPresenter.go();
		}
		return contentPresenter;
	}

	@Override
	public void onClickLogo() {
		eventBus.fireEvent(new ChangeTokenEvent(
				AppToken.newDashboardToken(StorageService.getInstance().getCookie().getProjectId())));
	}

	@Override
	public void showLogin() {
		StorageService.getInstance().detroyCookie();
		RootPanel.get().setStyleName("login");
		getLoginPresenter().show(RootPanel.get());
	}

	@Override
	public void showLoading(boolean loading) {
		if (AppToken.LOGIN.equals(AppToken.getToken())) {
			return;
		}
		getNavigationsPresenter().showProgress(loading);
	}

	@Override
	public void showMain() {
		RootPanel container = RootPanel.get();
		container.clear();
		container.setStyleName("main");
		
		getNavigationsPresenter().show(container);
		getContentPresenter().show(container);
	}
	
	@Override
	public void showDevice() {
		getContentPresenter().showDevice();
	}

	@Override
	public void showDashboard() {
		getContentPresenter().showDashboard();
	}
	
	@Override
	public void showScenes() {
		getContentPresenter().showSceneTable();
	}

	@Override
	public void showDevices() {
		getContentPresenter().showDeviceTable();
	}

	@Override
	public void showSchedules() {
		getContentPresenter().showScheduleTable();
	}
	
	@Override
	public void showModes() {
		getContentPresenter().showModeTable();
	}
	
	@Override
	public void showControllers() {
		getContentPresenter().showControllerTable();
	}

	@Override
	public void showRelays() {
		getContentPresenter().showRelayTable();
	}

	@Override
	public void showUsers() {
		getContentPresenter().showUserTable();
	}

	@Override
	public void showSensors() {
		getContentPresenter().showSensorTable();
	}

	@Override
	public void showAreas() {
		getContentPresenter().showAreaTable();
	}

	@Override
	public void showZones() {
		getContentPresenter().showZoneTable();
	}

	@Override
	public void showProjects() {
		getContentPresenter().showProjectTable();
	}

	@Override
	public void showSystemMenu() {
		getNavigationsPresenter().showSystemMenu();
	}

	@Override
	public void showProjectMenu() {
		getNavigationsPresenter().showProjectMenu();
	}
	
	@Override
	public void showZoneMenu() {
		// does nothing
	}

	@Override
	public void changeProject(ProjectDto project) {
		Long projId = StorageService.getInstance().getCookie().getProjectId();
		if (projId != null && projId.equals(project.getId())) {
			eventBus.fireEvent(new ChangeTokenEvent(AppToken.newDashboardToken(project.getId())));
		}
		else {
			StorageService.getInstance().getCookie().setProjectId(project.getId());
			StorageService.getInstance().writeCookie();
			eventBus.fireEvent(new ChangeTokenEvent(AppToken.newDashboardToken(project.getId())));
		}
	}
}
