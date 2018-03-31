package net.ionoff.center.client.login;

import com.google.gwt.user.client.ui.Label;

import gwt.material.design.client.constants.ButtonSize;
import gwt.material.design.client.constants.ButtonType;
import gwt.material.design.client.constants.Color;
import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.constants.WavesType;
import gwt.material.design.client.ui.MaterialButton;
import gwt.material.design.client.ui.MaterialCard;
import gwt.material.design.client.ui.MaterialCheckBox;
import gwt.material.design.client.ui.MaterialRow;
import gwt.material.design.client.ui.MaterialTextBox;
import net.ionoff.center.client.locale.LoginLocale;

public class SettingCard extends MaterialCard {
	
	private final Label lblLogo;
	private final MaterialTextBox tbServer1;
	private final MaterialTextBox tbServer2;
	private final MaterialCheckBox checkBoxServer1;
	private final MaterialCheckBox checkBoxServer2;
	private final MaterialButton btnSave;
	private final MaterialButton btnClose;
	
	public SettingCard() {
		
		addStyleName("card");
		
		MaterialRow row1 = new MaterialRow();
		add(row1);
		
		lblLogo = new Label();
		lblLogo.setStyleName("logo");
		row1.add(lblLogo);
		
		
		btnClose = new MaterialButton();
		btnClose.addStyleName("close");
		btnClose.setWaves(WavesType.DEFAULT);
		btnClose.setType(ButtonType.FLOATING);
		btnClose.setIconType(IconType.CLOSE);
		btnClose.setSize(ButtonSize.MEDIUM);
		btnClose.setBackgroundColor(Color.WHITE);
		btnClose.setIconColor(Color.RED);
				
		row1.add(btnClose);
		
		MaterialRow row2 = new MaterialRow();
		add(row2);
		
		checkBoxServer1 = new MaterialCheckBox();
		checkBoxServer1.setGrid("s1 m1");
		checkBoxServer1.addStyleName("server");
		row2.add(checkBoxServer1);
		
		tbServer1 = new MaterialTextBox();
		tbServer1.setLabel(LoginLocale.getLoginConst().server() + " 1");
		tbServer1.setGrid("s11 m5");
		row2.add(tbServer1);

		checkBoxServer2 = new MaterialCheckBox();
		checkBoxServer2.addStyleName("server");
		checkBoxServer2.setGrid("s1 m1");
		row2.add(checkBoxServer2);
		
		tbServer2 = new MaterialTextBox();
		tbServer2.setLabel(LoginLocale.getLoginConst().server() + " 2");
		tbServer2.setGrid("s11 m5");
		row2.add(tbServer2);

		MaterialRow row3 = new MaterialRow();
		row3.addStyleName("last");
		add(row3);

		btnSave = new MaterialButton();
		btnSave.setText(LoginLocale.getLoginConst().save());
		btnSave.setWaves(WavesType.DEFAULT);
		btnSave.setTextColor(Color.WHITE);
		btnSave.addStyleName("submit");
		btnSave.setGrid("s6 m5");
		row3.add(btnSave);
	}

	public MaterialTextBox getTbServer1() {
		return tbServer1;
	}
	
	public MaterialTextBox getTbServer2() {
		return tbServer2;
	}
	
	public MaterialCheckBox getCbServer1() {
		return checkBoxServer1;
	}
	
	public MaterialCheckBox getCbServer2() {
		return checkBoxServer2;
	}

	public MaterialButton getBtSave() {
		return btnSave;
	}
	
	public MaterialButton getBtClose() {
		return btnClose;
	}
}
