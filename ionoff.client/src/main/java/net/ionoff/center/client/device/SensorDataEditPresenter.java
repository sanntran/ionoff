package net.ionoff.center.client.device;


import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.HasWidgets;

import gwt.material.design.client.ui.MaterialTextBox;
import net.ionoff.center.client.base.AbstractEditPresenter;
import net.ionoff.center.client.base.IEditView;
import net.ionoff.center.client.service.EntityService;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.shared.dto.SensorDataDto;

public class SensorDataEditPresenter extends AbstractEditPresenter<SensorDataDto> {

	public interface Display extends IEditView<SensorDataDto> {
		MaterialTextBox getTextBoxValue();
		MaterialTextBox getTextBoxIndex();
	}

	protected IRpcServiceProvider rpcProvider;
	
	private final Display view;
	private SensorDataDto entityDto;
	private SensorDataTablePresenter dataTablePresenter;

	public SensorDataEditPresenter(IRpcServiceProvider rpcProvider,
								  HandlerManager eventBus, Display view, SensorDataTablePresenter dataTablePresenter) {
		super(rpcProvider, eventBus, view);
		this.rpcProvider = rpcProvider;
		this.view = view;
		this.dataTablePresenter = dataTablePresenter;
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
				dataTablePresenter.hideEditForm();
			}
		});
		view.getBtnCancel().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				dataTablePresenter.hideEditForm();
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
		return;
	}

	@Override
	protected String getClazz() {
		return SensorDataDto.class.getSimpleName();
	}

	@Override
	protected EntityService<SensorDataDto> getRpcService() {
		return rpcProvider.getSensorDataService();
	}

	public void setEntityDto(SensorDataDto dto) {
		entityDto = dto;
		view.getTextBoxValue().setText(dto.getValue() + "");
		view.getTextBoxIndex().setText(dto.getIndex() + "");
	}

}
