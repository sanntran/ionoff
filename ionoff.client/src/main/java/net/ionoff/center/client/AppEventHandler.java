package net.ionoff.center.client;


import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.History;
import com.google.inject.Inject;

import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.ui.MaterialLink;
import gwt.material.design.client.ui.MaterialToast;
import net.ionoff.center.client.event.ChangeLanguageEvent;
import net.ionoff.center.client.event.ChangeLanguageEventHandler;
import net.ionoff.center.client.event.ChangeProjectEvent;
import net.ionoff.center.client.event.ChangeProjectEventHandler;
import net.ionoff.center.client.event.ChangeTokenEvent;
import net.ionoff.center.client.event.ChangeTokenEventHandler;
import net.ionoff.center.client.event.ClickLogoEvent;
import net.ionoff.center.client.event.ClickLogoEventHandler;
import net.ionoff.center.client.event.FullscreenEvent;
import net.ionoff.center.client.event.FullscreenEventHandler;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.event.ShowLoadingEventHandler;
import net.ionoff.center.client.event.ShowMessageEvent;
import net.ionoff.center.client.event.ShowMessageEventHandler;
import net.ionoff.center.client.event.UserEnterEvent;
import net.ionoff.center.client.event.UserEnterEventHandler;
import net.ionoff.center.client.event.UserLogOutEvent;
import net.ionoff.center.client.event.UserLogOutEventHandler;
import net.ionoff.center.client.fullscreen.Fullscreen;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.client.utils.TokenUtil;
import net.xapxinh.center.client.player.event.ShowPlayerMessageEvent;
import net.xapxinh.center.client.player.event.ShowPlayerMessageEventHandler;

/**
 * @author Sann Tran
 */
public class AppEventHandler implements IAppEventHandler {

	@Inject	
	private HandlerManager eventBus;	
	
	@Inject
	private IAppController appController;

	@Override
	public void bind() {
		
		eventBus.addHandler(FullscreenEvent.TYPE, new FullscreenEventHandler() {
			@Override
			public void onFullscreen(FullscreenEvent event) {
				if (Fullscreen.isFullScreen()) {
					Fullscreen.exitFullscreen();
				}
				else {
					Fullscreen.requestFullscreen();
				}
			}
		});
		
		eventBus.addHandler(ChangeProjectEvent.TYPE, new ChangeProjectEventHandler() {
			@Override
			public void onChangeProject(ChangeProjectEvent event) {
				appController.changeProject(event.getProject());
			}
		});
		
		eventBus.addHandler(net.xapxinh.center.client.player.event.RpcFailureEvent.TYPE, 
				new net.xapxinh.center.client.player.event.RpcFailureEventHandler() {
			@Override
			public void onRpcFailure(net.xapxinh.center.client.player.event.RpcFailureEvent event) {
				doShowLoading(false);
				ClientUtil.handleRpcFailure(event.getMethod(), event.getException(), eventBus);
			}
		});
		
		eventBus.addHandler(ShowMessageEvent.TYPE, new ShowMessageEventHandler() {
			@Override
			public void onShowMessage(ShowMessageEvent event) {
				doShowLoading(false);
				doShowMessage(event.getMessage(), event.getMessageType());
			}
		});
		
		eventBus.addHandler(ShowPlayerMessageEvent.TYPE, new ShowPlayerMessageEventHandler() {
			@Override
			public void onShowMessage(ShowPlayerMessageEvent event) {
				doShowLoading(false);
				doShowMessage(event.getMessage(), event.getMessageType());
			}
		});
		
		eventBus.addHandler(ChangeTokenEvent.TYPE, new ChangeTokenEventHandler() {
			@Override
			public void onChangeToken(ChangeTokenEvent event) {
				if (event.getToken() != null
						&& !event.getToken().equals(History.getToken())) {
					doChangeToken(event.getToken());
				}
			}
		});

		eventBus.addHandler(ChangeLanguageEvent.TYPE, new ChangeLanguageEventHandler() {
			@Override
			public void onChangeLanguage(ChangeLanguageEvent event) {
				doChangeLanguage(event.getLanguage());
			}
		});
		
		eventBus.addHandler(UserLogOutEvent.TYPE, new UserLogOutEventHandler() {
			@Override
			public void onUserLogOut(UserLogOutEvent event) {
				doLogOutUser();
			}
		});
		
		eventBus.addHandler(ClickLogoEvent.TYPE, new ClickLogoEventHandler() {
			@Override
			public void onClickLogo(ClickLogoEvent event) {
				appController.onClickLogo();
			}
		});

		eventBus.addHandler(ShowLoadingEvent.TYPE, new ShowLoadingEventHandler() {
			@Override
			public void onShowLoading(ShowLoadingEvent event) {
				doShowLoading(event.isLoading());
			}
		});
		
		eventBus.addHandler(net.xapxinh.center.client.player.event.ShowLoadingEvent.TYPE, 
				new net.xapxinh.center.client.player.event.ShowLoadingEventHandler() {
			@Override
			public void onShowLoading(net.xapxinh.center.client.player.event.ShowLoadingEvent event) {
				doShowLoading(event.isLoading());
			}
		});
		
		eventBus.addHandler(UserEnterEvent.TYPE, new UserEnterEventHandler() {
			@Override
			public void onUserEnter(UserEnterEvent event) {
				appController.showMain();
			}
		});
	}
	
	private void doChangeToken(String token) {
		TokenUtil.setSourceChangeTokenEvent(true);
		TokenUtil.setPrevToken(History.getToken());
		History.newItem(token);
	}

	protected void doShowLoading(boolean loading) {
		appController.showLoading(loading);
	}

	protected void doLogOutUser() {
		appController.logout();
	}

	protected void doChangeLanguage(String language) {
		appController.changeLanguage(language);		
	}


	protected void doShowMessage(String message, int messageType) {
		String style = "";
		if (ShowMessageEvent.ERROR == messageType) {
			style = "error";
		}
		else if (ShowMessageEvent.SUCCESS == messageType) {
			style = "success";
		}
		else if (ShowMessageEvent.WARNING == messageType) {
			style = "warning";
		}
		else {
			style = "info";
		}
		MaterialLink link = new MaterialLink(IconType.CLOSE);
		link.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				link.getParent().setVisible(false);
			}
		});		
		new MaterialToast(link).toast(message, style);
	}	
}
