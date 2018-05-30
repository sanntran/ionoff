package net.ionoff.center.client.navigation;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.i18n.client.LocaleInfo;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.PopupPanel;

import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.ui.MaterialButton;
import gwt.material.design.client.ui.MaterialTitle;
import net.ionoff.center.client.base.AbstractPresenter;
import net.ionoff.center.client.event.ChangeLanguageEvent;
import net.ionoff.center.client.event.FullscreenEvent;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.event.ShowMessageEvent;
import net.ionoff.center.client.event.UserLogOutEvent;
import net.ionoff.center.client.locale.ClientLocale;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.storage.StorageService;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.UserDto;
import net.ionoff.center.shared.dto.VersionDto;

public class PopupUserMenuPresenter extends AbstractPresenter {

	interface Display {
		PopupPanel asPopup();
		
		MaterialTitle getLblUser();
		MaterialButton getUserMenuItemFullscreen();
		MaterialButton getUserMenuItemLogout();
		MaterialButton getUserMenuItemVersion();
		MaterialButton getUserMenuItemVi();
		MaterialButton getUserMenuItemEn();

    }
	
	private String latestVersion;
	private IRpcServiceProvider rpcService;
	private Display display;
	
	public PopupUserMenuPresenter(IRpcServiceProvider rpcService, HandlerManager eBus, Display view) {
		super(eBus);
		this.display = view;
		this.rpcService = rpcService;
	}


	@Override
	public void go() {
		UserDto user = StorageService.getInstance().getCookie().getUser();
		if ( user != null) {
			display.getLblUser().setTitle(StorageService.getInstance().getCookie().getUser().getFullName());
			display.getLblUser().setDescription(StorageService.getInstance().getCookie().getUser().getName());
		}
				
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
		
	}

	@Override
	public void show(HasWidgets container) {
		display.asPopup().show();
		if (ClientLocale.en_EN.equals(getLanguage())) {
			display.getUserMenuItemEn().setIconType(IconType.CHECK);
			display.getUserMenuItemVi().setIconType(IconType.CHECK_BOX_OUTLINE_BLANK);
		}
		else {
			display.getUserMenuItemEn().setIconType(IconType.CHECK_BOX_OUTLINE_BLANK);
			display.getUserMenuItemVi().setIconType(IconType.CHECK);
		}
			
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
					eventBus.fireEvent(new ShowMessageEvent(ClientLocale.getClientMessage()
							.currentVersionIsUptoDate(), ShowMessageEvent.NORMAL));
				}
				else {
					display.getUserMenuItemVersion().setText(ClientLocale.getClientConst().upgrade() + latestVersion);
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
}