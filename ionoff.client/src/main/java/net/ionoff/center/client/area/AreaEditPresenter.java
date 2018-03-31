package net.ionoff.center.client.area;


import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.HasWidgets;

import gwt.material.design.client.ui.MaterialIntegerBox;
import net.ionoff.center.client.common.AbstractEditPresenter;
import net.ionoff.center.client.common.IEditView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.EntityService;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.AreaDto;

public class AreaEditPresenter extends AbstractEditPresenter<AreaDto> {

	public interface Display extends IEditView<AreaDto> {
		MaterialIntegerBox getIntBoxOrder();
	}
	
	protected IRpcServiceProvider rpcProvider;
	
	private final Display view;
	private AreaDto entityDto;
	private AreaTablePresenter areaManager;

	public AreaEditPresenter(IRpcServiceProvider rpcProvider, 
			HandlerManager eventBus, Display view, AreaTablePresenter areaManager) {
		super(rpcProvider, eventBus, view);
		this.rpcProvider = rpcProvider;
		this.view = view;
		this.areaManager = areaManager;
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
				areaManager.hideEditForm();
			}
		});
		view.getBtnCancel().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				areaManager.hideEditForm();
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
		entityDto.setName(newName);
		entityDto.setOrder(view.getIntBoxOrder().getValue());
		
		rpcProvider.getAreaService().save(entityDto.getId(), entityDto, 
				new MethodCallback<AreaDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, AreaDto result) {
				areaManager.onSavedSucess(result);
			}
		});
	}

	@Override
	protected String getClazz() {
		return AreaDto.class.getSimpleName();
	}

	@Override
	protected EntityService<AreaDto> getRpcService() {
		return rpcProvider.getAreaService();
	}

	public void setEntityDto(AreaDto dto) {
		entityDto = dto;
		updateView(dto);
	}

	private void updateView(AreaDto dto) {
		view.getLblId().setText("#" + dto.getId());
		view.getLblName().setText(dto.getName());
		view.getTextBoxName().setText(dto.getName());
		view.getIntBoxOrder().setValue(entityDto.getOrder());
	}
}
