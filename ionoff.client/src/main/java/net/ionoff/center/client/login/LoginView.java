package net.ionoff.center.client.login;

import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.Label;

import gwt.material.design.client.constants.ButtonSize;
import gwt.material.design.client.constants.ButtonType;
import gwt.material.design.client.constants.Color;
import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.constants.InputType;
import gwt.material.design.client.constants.WavesType;
import gwt.material.design.client.ui.MaterialButton;
import gwt.material.design.client.ui.MaterialCard;
import gwt.material.design.client.ui.MaterialCheckBox;
import gwt.material.design.client.ui.MaterialRow;
import gwt.material.design.client.ui.MaterialTextBox;
import net.ionoff.center.client.locale.LoginLocale;

public class LoginView extends FlowPanel implements LoginPresenter.Display {
	
	private final Label lblLogo;
	private final MaterialButton btnSetting;
	private final MaterialTextBox tbUser;
	private final MaterialTextBox ptbPass;
	private final MaterialButton btnLogin;
	private final MaterialCheckBox checkBoxRemember;
	private final MaterialCard cardLogin;
	private final SettingCard cardSetting;
	

	public LoginView() {
		
		setStyleName("form");
		
		cardLogin = new MaterialCard();
		cardLogin.addStyleName("card");
		
		MaterialRow row1 = new MaterialRow();
		cardLogin.add(row1);
		
		lblLogo = new Label();
		lblLogo.setStyleName("logo");
		row1.add(lblLogo);
		
		btnSetting = new MaterialButton();
		btnSetting.addStyleName("setting");
		btnSetting.setWaves(WavesType.DEFAULT);
		btnSetting.setType(ButtonType.FLOATING);
		btnSetting.setIconType(IconType.SETTINGS);
		btnSetting.setSize(ButtonSize.MEDIUM);
		btnSetting.setBackgroundColor(Color.WHITE);
		btnSetting.setIconColor(Color.GREEN);
		row1.add(btnSetting);
		
		MaterialRow row2 = new MaterialRow();
		cardLogin.add(row2);
		
		tbUser = new MaterialTextBox();
		tbUser.setLabel(LoginLocale.getLoginConst().userName());
		tbUser.setIconType(IconType.ACCOUNT_CIRCLE);
		tbUser.setGrid("s12 m5");
		row2.add(tbUser);

		ptbPass = new MaterialTextBox();
		ptbPass.addStyleName("password");
		ptbPass.setType(InputType.PASSWORD);
		ptbPass.setLabel(LoginLocale.getLoginConst().password());
		ptbPass.setIconType(IconType.LOCK);
		ptbPass.setGrid("s12 m6");
		row2.add(ptbPass);

		MaterialRow row3 = new MaterialRow();
		row3.addStyleName("last");
		cardLogin.add(row3);
		
		checkBoxRemember = new MaterialCheckBox(LoginLocale.getLoginConst().remeberPassword());
		checkBoxRemember.addStyleName("remember");
		checkBoxRemember.setGrid("s6");
		row3.add(checkBoxRemember);

		btnLogin = new MaterialButton();
		btnLogin.setText(LoginLocale.getLoginConst().login());
		btnLogin.setWaves(WavesType.DEFAULT);
		btnLogin.setTextColor(Color.WHITE);
		btnLogin.addStyleName("submit");
		btnLogin.setGrid("s6");
		row3.add(btnLogin);
		
		cardSetting = new SettingCard();
		
		String protocol = Window.Location.getProtocol();
		if (!protocol.contains("file")) {
			btnSetting.setVisible(false);
		}
		
		add(cardLogin);
	}

	@Override
	public MaterialTextBox getPtbPass() {
		return ptbPass;
	}

	@Override
	public MaterialButton getBtLogin() {
		return btnLogin;
	}

	@Override
	public MaterialTextBox getTbUser() {
		return tbUser;
	}
	
	@Override
	public MaterialButton getBtnSetting() {
		return btnSetting;
	}

	@Override
	public FlowPanel asPanel() {
		return this;
	}
	
	@Override
	public MaterialCard getCardLogin() {
		return cardLogin;
	}
	
	@Override
	public SettingCard getCardSetting() {
		return cardSetting;
	}

	@Override
	public MaterialCheckBox getCheckBoxRemember() {
		return checkBoxRemember;
	}
}
