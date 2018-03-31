package net.ionoff.center.client.login;

import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.dom.client.KeyCodes;
import com.google.gwt.event.dom.client.KeyUpEvent;
import com.google.gwt.event.dom.client.KeyUpHandler;
import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.event.logical.shared.ValueChangeHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.http.client.Response;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Panel;
import com.google.gwt.user.client.ui.RootPanel;

import gwt.material.design.client.ui.MaterialButton;
import gwt.material.design.client.ui.MaterialCard;
import gwt.material.design.client.ui.MaterialCheckBox;
import gwt.material.design.client.ui.MaterialLoader;
import gwt.material.design.client.ui.MaterialTextBox;
import net.ionoff.center.client.event.ChangeTokenEvent;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.event.ShowMessageEvent;
import net.ionoff.center.client.event.UserEnterEvent;
import net.ionoff.center.client.locale.ClientLocale;
import net.ionoff.center.client.locale.LoginLocale;
import net.ionoff.center.client.service.LoginService;
import net.ionoff.center.client.storage.ApiServer;
import net.ionoff.center.client.storage.StorageService;
import net.ionoff.center.client.utils.AppToken;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.cookie.Kookie;
import net.ionoff.center.shared.dto.MessageDto;

/**
 * @author Sann Tran
 */
public class LoginPresenter {

	public interface Display {
		Panel asPanel();
		
		MaterialCard getCardLogin();

		MaterialButton getBtnSetting();
		
		MaterialTextBox getPtbPass();

		MaterialButton getBtLogin();

		MaterialTextBox getTbUser();

		MaterialCheckBox getCheckBoxRemember();

		SettingCard getCardSetting();
	}

	private final Display display;
	private final LoginService loginService;
	private final HandlerManager eventBus;

	public LoginPresenter(LoginService loginService, HandlerManager eventBus, Display view) {
		this.loginService = loginService;
		this.display = view;
		this.eventBus = eventBus;
	}

	public void bind() {
		
		display.getBtnSetting().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				display.asPanel().clear();
				display.asPanel().add(display.getCardSetting());
				List<ApiServer> servers = StorageService.getInstance().getServers();
				display.getCardSetting().getTbServer1().setText(servers.get(0).getHost());
				display.getCardSetting().getTbServer2().setText(servers.get(1).getHost());
				display.getCardSetting().getCbServer1().setValue(servers.get(0).isEnabled());
				display.getCardSetting().getCbServer2().setValue(servers.get(1).isEnabled());
			}
		});
		
		display.getCardSetting().getBtClose().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				display.asPanel().clear();
				display.asPanel().add(display.getCardLogin());
			}
		});
		
		display.getCardSetting().getCbServer1().addValueChangeHandler(new ValueChangeHandler<Boolean>() {
			@Override
			public void onValueChange(ValueChangeEvent<Boolean> event) {
				boolean value = display.getCardSetting().getCbServer1().getValue();
				if (value == true) {
					display.getCardSetting().getCbServer2().setValue(false);
				}
				else {
					display.getCardSetting().getCbServer2().setValue(true);
				}
			}
		});
		
		display.getCardSetting().getCbServer2().addValueChangeHandler(new ValueChangeHandler<Boolean>() {
			@Override
			public void onValueChange(ValueChangeEvent<Boolean> event) {
				boolean value = display.getCardSetting().getCbServer2().getValue();
				if (value == true) {
					display.getCardSetting().getCbServer1().setValue(false);
				}
				else {
					display.getCardSetting().getCbServer1().setValue(true);
				}
			}
		});
		
		display.getCardSetting().getBtSave().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				saveSetting();
			}
		});

		display.getBtLogin().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				doLogin();
			}
		});

		display.getTbUser().addKeyUpHandler(new KeyUpHandler() {
			@Override
			public void onKeyUp(KeyUpEvent event) {
				if (event.getNativeKeyCode() == KeyCodes.KEY_ENTER) {
					doLogin();
				}
			}
		});

		display.getPtbPass().addKeyUpHandler(new KeyUpHandler() {
			@Override
			public void onKeyUp(KeyUpEvent event) {
				if (event.getNativeKeyCode() == KeyCodes.KEY_ENTER) {
					doLogin();
				}
			}
		});

		display.getCheckBoxRemember().addKeyUpHandler(new KeyUpHandler() {
			@Override
			public void onKeyUp(KeyUpEvent event) {
				if (event.getNativeKeyCode() == KeyCodes.KEY_ENTER) {
					doLogin();
				}
			}
		});
	}

	private void saveSetting() {
		String server1 = display.getCardSetting().getTbServer1().getText().trim();
		String server2 = display.getCardSetting().getTbServer2().getText().trim();
		boolean server1Enabled = display.getCardSetting().getCbServer1().getValue();
		boolean server2Enabled = display.getCardSetting().getCbServer2().getValue();
		if (!server1.contains(".")) {
			eventBus.fireEvent(new ShowMessageEvent(LoginLocale.getLoginMessages().fieldInvalid(LoginLocale.getLoginConst().server() + " 1"), ShowMessageEvent.ERROR));
			return;
		}
		if (!server2.contains(".")) {
			eventBus.fireEvent(new ShowMessageEvent(LoginLocale.getLoginMessages().fieldInvalid(LoginLocale.getLoginConst().server() + " 2"), ShowMessageEvent.ERROR));
			return;
		}
		StorageService.getInstance().getServers().get(0).setHost(server1);
		StorageService.getInstance().getServers().get(0).setEnabled(server1Enabled);
		StorageService.getInstance().getServers().get(1).setHost(server2);
		StorageService.getInstance().getServers().get(1).setEnabled(server2Enabled);
		StorageService.getInstance().saveServers();
		
		String url = ClientUtil.getBaseUrl();
		Window.open(url, "_self", "");
	}

	public void show(HasWidgets container) {
		RootPanel.get().clear();
		RootPanel.get().add(display.asPanel());
		autoFillTextBoxUser();
	}

	public void go() {
		bind();
	}

	private void autoFillTextBoxUser() {
		final Kookie cookie = StorageService.getInstance().getCookie();
		if (cookie != null && cookie.getUser() != null) {
			display.getTbUser().setText(cookie.getUser().getName());
		}
	}

	private void doLogin() {
		
		final String userName = display.getTbUser().getText().trim();
		final String password = display.getPtbPass().getText();
		final boolean rememberPass = display.getCheckBoxRemember().getValue();
		if ("license".equals(userName)) {
			activate(password);
			return;
		}
		
		if (!validateUserName(userName)) {
			eventBus.fireEvent(new ShowMessageEvent(LoginLocale.getLoginMessages().fieldInvalid(LoginLocale.getLoginConst().userName()), ShowMessageEvent.ERROR));
			return;
		}
		if (!validateUserPass(password)) {
			eventBus.fireEvent(new ShowMessageEvent(LoginLocale.getLoginMessages().fieldInvalid(LoginLocale.getLoginConst().password()), ShowMessageEvent.ERROR));
			return;
		}
		else {
			try {
				login(userName, password, rememberPass);
			}
			catch (final Exception e) {
				eventBus.fireEvent(new ShowMessageEvent(LoginLocale.getLoginMessages().fieldInvalid(LoginLocale.getLoginConst().userName()), ShowMessageEvent.ERROR));
			}
		}
		
	}

	private boolean validateUserName(String userName) {
		if (userName.isEmpty() || (userName.length() < 4) || (userName.length() > 20)) {
			return false;
		}
		return true;
	}
	private boolean validateUserPass(String password) {
		if (password.length() > 20 ||  password.length() < 1) {
			return false;
		}
		return true;
	}

	protected void login(String username, String password, final boolean remember) throws Exception {
		MaterialLoader.showProgress(true, display.getCardLogin());
		loginService.requestAuthen(username, password, remember, getLanguage(), new MethodCallback<Kookie>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				MaterialLoader.showProgress(false, display.getCardLogin());
				if (method.getResponse().getStatusCode() == Response.SC_UNAUTHORIZED) {
					eventBus.fireEvent(new ShowMessageEvent(LoginLocale.getLoginMessages().loginFailed(), 
							ShowMessageEvent.ERROR));
				}
				else {
					ClientUtil.handleRpcFailure(method, exception, eventBus);
				}
			}
			@Override
			public void onSuccess(Method method, Kookie response) {
				MaterialLoader.showProgress(false, display.getCardLogin());
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				display.getPtbPass().setText("");				
				if (response.getProjectId() == null) {
					eventBus.fireEvent(new ShowMessageEvent(ClientLocale.getClientMessage().noProject(),ShowMessageEvent.ERROR));
				}
				else {
					StorageService.getInstance().setCookie(response);
					StorageService.getInstance().writeCookie();				
					eventBus.fireEvent(new UserEnterEvent());
					eventBus.fireEvent(new ChangeTokenEvent(AppToken.newDashboardToken(response.getProjectId())));
				}
			}
		});
	}

	private void activate(String password) {
		MaterialLoader.showProgress(true, display.getCardLogin());
		loginService.activate(password, new MethodCallback<MessageDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				MaterialLoader.showProgress(false, display.getCardLogin());
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, MessageDto response) {
				MaterialLoader.showProgress(false, display.getCardLogin());
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				if (response.getStatus() == Response.SC_BAD_REQUEST) {
					eventBus.fireEvent(new ShowMessageEvent(LoginLocale.getLoginMessages().invalidLicenseKey(), 
							ShowMessageEvent.ERROR));
				}
				else {
					eventBus.fireEvent(new ShowMessageEvent(LoginLocale.getLoginMessages().applicationActivated(), 
							ShowMessageEvent.SUCCESS));
				}
			}
		});
	}

	private String getLanguage() {
		return ClientLocale.vi_VN;
	}
}
