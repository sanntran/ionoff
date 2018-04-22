package net.ionoff.center.client.device;


import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.HasWidgets;
import gwt.material.design.client.ui.MaterialDoubleBox;
import net.ionoff.center.client.base.AbstractEditPresenter;
import net.ionoff.center.client.base.IEditView;
import net.ionoff.center.client.sensor.SensorTablePresenter;
import net.ionoff.center.client.service.EntityService;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.shared.dto.SensorDataDto;

public class ScaleDataEditPresenter extends AbstractEditPresenter<SensorDataDto> {

	public interface Display extends IEditView<SensorDataDto> {
		MaterialDoubleBox getDoubleBoxValue();
	}
	protected IRpcServiceProvider rpcProvider;
	
	private final Display view;
	private SensorDataDto entityDto;
	private ScaleDataTablePresenter dataTablePresenter;

	public ScaleDataEditPresenter(IRpcServiceProvider rpcProvider,
								  HandlerManager eventBus, Display view, ScaleDataTablePresenter dataTablePresenter) {
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
		updateView(dto);
	}

	private void updateView(SensorDataDto dto) {
		view.getLblId().setText("#" + dto.getId());
		view.getLblName().setText(dto.getTime());
		view.getTextBoxName().setText(dto.getTime());
		view.getDoubleBoxValue().setValue(dto.getValue());
	}
}
