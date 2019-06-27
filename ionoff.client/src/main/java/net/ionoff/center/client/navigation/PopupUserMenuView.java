package net.ionoff.center.client.navigation;

import com.google.gwt.core.client.GWT;
import com.google.gwt.uibinder.client.UiBinder;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiTemplate;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.HTMLPanel;
import com.google.gwt.user.client.ui.PopupPanel;
import com.google.gwt.user.client.ui.PopupPanel.AnimationType;
import com.google.gwt.user.client.ui.Widget;

import gwt.material.design.client.constants.IconSize;
import gwt.material.design.client.ui.MaterialButton;
import gwt.material.design.client.ui.MaterialTitle;
import net.ionoff.center.client.locale.ClientLocale;

public class PopupUserMenuView extends Composite implements PopupUserMenuPresenter.Display {

	@UiTemplate("PopupUserMenuView.ui.xml")
	interface PopupUserMenuViewUiBinder extends UiBinder<Widget, PopupUserMenuView> {
	}

	private static PopupUserMenuViewUiBinder uiBinder = GWT.create(PopupUserMenuViewUiBinder.class);

	@UiField
	HTMLPanel root;
	@UiField
	PopupPanel popup;
	@UiField
	MaterialTitle lblUser;
	@UiField
	MaterialButton menuItemFullscreen;
	@UiField
	MaterialButton menuItemVi;
	@UiField
	MaterialButton menuItemEn;
	@UiField
	MaterialButton menuItemVersion;
	@UiField
	MaterialButton menuItemLogout;
	
	public PopupUserMenuView() {
		uiBinder.createAndBindUi(this);
		
		popup.setAnimationEnabled(true);
		popup.setAnimationType(AnimationType.ROLL_DOWN);
		popup.setAutoHideOnHistoryEventsEnabled(true);
		popup.setAutoHideEnabled(true);

		menuItemFullscreen.setText(ClientLocale.getClientConst().fullScreen());
		menuItemEn.setText(ClientLocale.getClientConst().english());
		menuItemVi.setText(ClientLocale.getClientConst().vietnamese());
		menuItemVersion.setText(ClientLocale.getClientConst().upgrade());
		menuItemLogout.setText(ClientLocale.getClientConst().logout());
		menuItemFullscreen.setIconSize(IconSize.LARGE);
	}

	@Override
	public PopupPanel asPopup() {
		return popup;
	}

	@Override
	public MaterialTitle getLblUser() {
		return lblUser;
	}

	@Override
	public MaterialButton getUserMenuItemFullscreen() {
		return menuItemFullscreen;
	}

	@Override
	public MaterialButton getUserMenuItemLogout() {
		return menuItemLogout;
	}

	@Override
	public MaterialButton getUserMenuItemVersion() {
		return menuItemVersion;
	}

	@Override
	public MaterialButton getUserMenuItemVi() {
		return menuItemVi;
	}

	@Override
	public MaterialButton getUserMenuItemEn() {
		return menuItemEn;
	}
}
