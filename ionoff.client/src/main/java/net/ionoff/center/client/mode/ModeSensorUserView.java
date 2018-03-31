package net.ionoff.center.client.mode;

import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.shared.dto.ModeSensorUserDto;

import com.google.gwt.user.client.ui.CheckBox;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.Label;

public class ModeSensorUserView extends FlowPanel implements ModeSensorUserPresenter.Display {
	
	private final Label lblUserIcon;
	private final Label lblUserName;
	private final CheckBox checkBoxSendSms;
	private final CheckBox checkBoxSendEmail;
	
	public ModeSensorUserView(ModeSensorUserDto modeSensorUser) {
		setStyleName("sensorUser");
		
		lblUserIcon = new Label();
		lblUserIcon.setStyleName("userIcon");
		add(lblUserIcon);
		
		lblUserName = new Label(modeSensorUser.getUserName());
		lblUserName.setStyleName("userName");
		add(lblUserName);
		
		checkBoxSendSms = new CheckBox(AdminLocale.getAdminConst().sendSms());
		checkBoxSendSms.setStyleName("checkBox");
		add(checkBoxSendSms);
		checkBoxSendSms.setValue(modeSensorUser.isSendSms());
		
		checkBoxSendEmail = new CheckBox(AdminLocale.getAdminConst().sendEmail());
		checkBoxSendEmail.setStyleName("checkBox");
		add(checkBoxSendEmail);
		checkBoxSendEmail.setValue(modeSensorUser.isSendEmail());
	}

	@Override
	public Label getLblUserName() {
		return lblUserName;
	}

	@Override
	public CheckBox getCheckBoxSendSms() {
		return checkBoxSendSms;
	}
	
	@Override
	public CheckBox getCheckBoxSendEmail() {
		return checkBoxSendEmail;
	}
}
