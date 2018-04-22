package net.ionoff.center.client.user;

import com.google.gwt.user.client.ui.FlowPanel;

import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.ui.MaterialListBox;
import gwt.material.design.client.ui.MaterialTextBox;
import gwt.material.design.client.ui.html.Option;
import net.ionoff.center.client.base.AbstractEditView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.shared.dto.UserDto;

public class UserEditView extends AbstractEditView<UserDto> implements UserEditPresenter.Display {
	 
	private MaterialTextBox textBoxFullname;
	private MaterialTextBox textBoxPassword;
	private MaterialTextBox textBoxEmail;
	private MaterialTextBox textBoxPhoneNumber;
	private MaterialListBox listBoxUserGroups;
	private UserRolesView userRoleView;
	
	public UserEditView() {
		super();
		getLblIcon().setIconType(IconType.ACCOUNT_CIRCLE);
		
		textBoxFullname = new MaterialTextBox();
		textBoxFullname.setLabel(AdminLocale.getAdminConst().fullName());
		contentPanel.add(textBoxFullname);
		
		textBoxPassword = new MaterialTextBox();
		textBoxPassword.setLabel(AdminLocale.getAdminConst().password());
		contentPanel.add(textBoxPassword);
		
		FlowPanel panel = new FlowPanel();
		panel.setHeight("65px");
		panel.addStyleName("row");
		contentPanel.add(panel);
		
		textBoxEmail = new MaterialTextBox();
		textBoxEmail.addStyleName("col s6 no-padding");
		textBoxEmail.setLabel(AdminLocale.getAdminConst().email());
		panel.add(textBoxEmail);
		
		textBoxPhoneNumber = new MaterialTextBox();
		textBoxPhoneNumber.addStyleName("col s6 no-padding");
		textBoxPhoneNumber.setLabel(AdminLocale.getAdminConst().phoneNumber());
		panel.add(textBoxPhoneNumber);
		
		listBoxUserGroups = new MaterialListBox();
		listBoxUserGroups.setPlaceholder(AdminLocale.getAdminConst().group());
		listBoxUserGroups.setEmptyPlaceHolder(AdminLocale.getAdminConst().group());		
		listBoxUserGroups.add(new Option(AdminLocale.getAdminConst().projectUser()));
		listBoxUserGroups.add(new Option(AdminLocale.getAdminConst().projectAdmin()));
		Option systemAdminOpt = new Option(AdminLocale.getAdminConst().systemAdmin());
		systemAdminOpt.setEnabled(false);
		listBoxUserGroups.add(systemAdminOpt);
		contentPanel.add(listBoxUserGroups);
		
		userRoleView  = new UserRolesView();
		contentPanel.add(userRoleView);
	}

	@Override
	public MaterialListBox getListBoxUserGroups() {
		return listBoxUserGroups;
	}
	
	@Override
	public MaterialTextBox getTextBoxFullName() {
		return textBoxFullname;
	}
	
	@Override
	public MaterialTextBox getTextBoxPassword() {
		return textBoxPassword;
	}
	
	@Override
	public MaterialTextBox getTextBoxEmail() {
		return textBoxEmail;
	}
	
	@Override
	public MaterialTextBox getTextBoxPhoneNumber() {
		return textBoxPhoneNumber;
	}

	@Override
	public UserRolesView getUserRolesView() {
		return userRoleView;
	}
}
