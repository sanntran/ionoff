package net.ionoff.center.client.user;


import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.HasWidgets;

import gwt.material.design.client.ui.MaterialListBox;
import gwt.material.design.client.ui.MaterialTextBox;
import gwt.material.design.client.ui.html.Option;
import net.ionoff.center.client.base.AbstractEditPresenter;
import net.ionoff.center.client.base.IEditView;
import net.ionoff.center.client.event.ShowMessageEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.locale.ClientLocale;
import net.ionoff.center.client.service.EntityService;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.AppToken;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.UserGroupDto;
import net.ionoff.center.shared.dto.UserDto;

public class UserEditPresenter extends AbstractEditPresenter<UserDto> {

	public interface Display extends IEditView<UserDto> {

		MaterialTextBox getTextBoxFullName();

		MaterialTextBox getTextBoxPassword();

		MaterialTextBox getTextBoxEmail(); 

		MaterialTextBox getTextBoxPhoneNumber();
		
		MaterialListBox getListBoxUserGroups();

		UserRolesPresenter.Display getUserRolesView();
	}
	
	protected IRpcServiceProvider rpcProvider;
	
	private final Display view;
	private UserDto entityDto;
	private SystemUsersPresenter userManager;
	private UserRolesPresenter userRolePresenter;

	public UserEditPresenter(IRpcServiceProvider rpcProvider, 
			HandlerManager eventBus, Display view, SystemUsersPresenter userManager) {
		super(rpcProvider, eventBus, view);
		this.rpcProvider = rpcProvider;
		this.view = view;
		this.userManager = userManager;
		this.userRolePresenter = new UserRolesPresenter(rpcProvider, eventBus, view.getUserRolesView());
	}

	@Override
	protected void bind() {
		view.getBtnSave().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				save();
			}
		});
		view.getBtnClose().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				userManager.hideEditForm();
			}
		});
		view.getBtnCancel().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				userManager.hideEditForm();
			}
		});
	}

	@Override
	public void go() {
		bind();
		userRolePresenter.go();
	}

	@Override
	public void show(HasWidgets container) {
		//
	}

	@Override
	protected void save() {
		if (entityDto == null) {
			return;
		}
		
		String newName = view.getTextBoxName().getValue();
		if (!validateInputStringValue(AdminLocale.getAdminConst().name(), newName)) {
			return;
		}
		
		String newPassword = view.getTextBoxPassword().getValue();
		if (!super.validateInputStringValue(AdminLocale.getAdminConst().password(), newPassword)) {
			return;
		}
		if (newPassword.length() > 16) {
			String message = AdminLocale.getAdminMessages().overMaximunLength(AdminLocale.getAdminConst().password());
			eventBus.fireEvent(new ShowMessageEvent(message, ShowMessageEvent.ERROR));
			return;
		}
		
		String newEmail = view.getTextBoxEmail().getValue();
		if (!super.validateInputStringValue(AdminLocale.getAdminConst().email(), newEmail)) {
			return;
		}
		if (!newEmail.contains("@")) {
			String message = ClientLocale.getClientMessage().fieldInvalid(AdminLocale.getAdminConst().email());
			eventBus.fireEvent(new ShowMessageEvent(message, ShowMessageEvent.ERROR));
			return;
		}
		
		String newPhoneNo = view.getTextBoxPhoneNumber().getValue();
		if (!super.validateInputStringValue(AdminLocale.getAdminConst().phoneNumber(), newPhoneNo)) {
			return;
		}
		
		entityDto.setName(newName);
		entityDto.setFullName(view.getTextBoxFullName().getText());
		entityDto.setPassword(newPassword);
		entityDto.setEmail(newEmail);
		entityDto.setPhoneNo(newPhoneNo);
		
		int selectedGroupIndex = view.getListBoxUserGroups().getSelectedIndex();
				
		if (selectedGroupIndex == 2) {
			entityDto.setGroupName(UserGroupDto.PROJECT_ADMIN);
		}
		else {
			entityDto.setGroupName(UserGroupDto.PROJECT_USER);
		}
		
		if (!AppToken.hasTokenItem(AppToken.PROJECT)) {
			rpcProvider.getUserService().save(entityDto.getId(), entityDto, 
					new MethodCallback<UserDto>() {
				@Override
				public void onFailure(Method method, Throwable exception) {
					ClientUtil.handleRpcFailure(method, exception, eventBus);
				}
				@Override
				public void onSuccess(Method method, UserDto result) {
					userManager.onSavedSucess(result);
				}
			});
		}
		else {
			Long projectId = Long.parseLong(AppToken.getProjectId());
			rpcProvider.getUserService().save(projectId, entityDto.getId(), entityDto, 
					new MethodCallback<UserDto>() {
				@Override
				public void onFailure(Method method, Throwable exception) {
					ClientUtil.handleRpcFailure(method, exception, eventBus);
				}
				@Override
				public void onSuccess(Method method, UserDto result) {
					userManager.onSavedSucess(result);
				}
			});
		}
		
	}

	@Override
	protected String getClazz() {
		return UserDto.class.getSimpleName();
	}

	@Override
	protected EntityService<UserDto> getRpcService() {
		return rpcProvider.getUserService();
	}

	public void setEntityDto(UserDto dto) {
		entityDto = dto;
		updateView(dto);
		userRolePresenter.setUser(entityDto);
	}

	private void updateView(UserDto dto) {
		view.getLblId().setText("#" + dto.getId());
		view.getLblName().setText(dto.getName());
		view.getTextBoxName().setText(dto.getName());
		view.getTextBoxFullName().setText(dto.getFullName());
		view.getTextBoxPassword().setText(dto.getPassword());
		view.getTextBoxEmail().setText(dto.getEmail());
		view.getTextBoxPhoneNumber().setText(dto.getPhoneNo());
		
		if (UserGroupDto.isSystemAdmin(dto.getGroupName())) {
			view.getListBoxUserGroups().clear();
			Option projectUserOpt = new Option(AdminLocale.getAdminConst().projectUser());
			Option projectAdminOpt = new Option(AdminLocale.getAdminConst().projectAdmin());
			projectUserOpt.setEnabled(false);
			projectAdminOpt.setEnabled(false);
			view.getListBoxUserGroups().add(projectUserOpt);
			view.getListBoxUserGroups().add(projectAdminOpt);
			view.getListBoxUserGroups().add(new Option(AdminLocale.getAdminConst().systemAdmin()));
			view.getListBoxUserGroups().setSelectedIndex(2);
		}
		else {
			view.getListBoxUserGroups().clear();
			view.getListBoxUserGroups().setEmptyPlaceHolder(AdminLocale.getAdminConst().group());		
			view.getListBoxUserGroups().add(new Option(AdminLocale.getAdminConst().projectUser()));
			view.getListBoxUserGroups().add(new Option(AdminLocale.getAdminConst().projectAdmin()));
			Option systemAdminOpt = new Option(AdminLocale.getAdminConst().systemAdmin());
			systemAdminOpt.setEnabled(false);
			view.getListBoxUserGroups().add(systemAdminOpt);
			if (UserGroupDto.isProjectUser(dto.getGroupName())) {
				view.getListBoxUserGroups().setSelectedIndex(1);
			} else if (UserGroupDto.isProjectAdmin(dto.getGroupName())) {
				view.getListBoxUserGroups().setSelectedIndex(2);
			}
		}
	} 
}
