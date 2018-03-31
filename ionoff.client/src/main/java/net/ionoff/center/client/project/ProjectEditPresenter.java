package net.ionoff.center.client.project;


import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.HasWidgets;

import gwt.material.design.client.ui.MaterialTextBox;
import net.ionoff.center.client.common.AbstractEditPresenter;
import net.ionoff.center.client.common.IEditView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.EntityService;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.ProjectDto;

public class ProjectEditPresenter extends AbstractEditPresenter<ProjectDto> {

	public interface Display extends IEditView<ProjectDto> {
		MaterialTextBox getTextBoxAddress();
		
	}
	protected IRpcServiceProvider rpcProvider;
	
	private final Display view;
	private ProjectDto entityDto;
	private ProjectTablePresenter projectManager;

	public ProjectEditPresenter(IRpcServiceProvider rpcProvider, 
			HandlerManager eventBus, Display view, ProjectTablePresenter projectManager) {
		super(rpcProvider, eventBus, view);
		this.rpcProvider = rpcProvider;
		this.view = view;
		this.projectManager = projectManager;
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
				projectManager.hideEditForm();
			}
		});
		view.getBtnCancel().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				projectManager.hideEditForm();
			}
		});
	}

	@Override
	public void go() {
		bind();
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
		String newAddress = view.getTextBoxAddress().getValue();
		if (!validateInputStringValue(AdminLocale.getAdminConst().address(), newAddress)) {
			return;
		}
		entityDto.setName(newName);
		entityDto.setAddress(newAddress);
		
		rpcProvider.getProjectService().save(entityDto.getId(), entityDto, 
				new MethodCallback<ProjectDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, ProjectDto result) {
				projectManager.onSavedSucess(result);
			}
		});
	}

	@Override
	protected String getClazz() {
		return ProjectDto.class.getSimpleName();
	}

	@Override
	protected EntityService<ProjectDto> getRpcService() {
		return rpcProvider.getProjectService();
	}

	public void setEntityDto(ProjectDto dto) {
		entityDto = dto;
		updateView(dto);
	}

	private void updateView(ProjectDto dto) {
		view.getLblId().setText("#" + dto.getId());
		view.getLblName().setText(dto.getName());
		view.getTextBoxName().setText(dto.getName());
		view.getTextBoxAddress().setText(dto.getAddress());
	}
}
