package net.ionoff.center.client.device;


import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.InlineLabel;
import com.google.gwt.user.client.ui.Label;
import gwt.material.design.client.constants.CollectionType;
import gwt.material.design.client.ui.MaterialCollection;
import gwt.material.design.client.ui.MaterialCollectionItem;
import gwt.material.design.client.ui.MaterialDoubleBox;
import net.ionoff.center.client.base.AbstractEditPresenter;
import net.ionoff.center.client.base.IEditView;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.sensor.SensorTablePresenter;
import net.ionoff.center.client.service.EntityService;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.AppToken;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.QueryCriteriaDto;
import net.ionoff.center.shared.dto.SensorDataDto;
import net.ionoff.center.shared.dto.WeighScaleDto;
import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import java.util.List;

public class ScaleDataEditPresenter extends AbstractEditPresenter<SensorDataDto> {

	public interface Display extends IEditView<SensorDataDto> {
		MaterialCollection getDataCollection();
	}

	protected IRpcServiceProvider rpcProvider;
	
	private final Display view;
	private SensorDataDto entityDto;
	private WeighScaleDto scaleDto;
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
		loadSensorData();
		view.getLblName().setText(dto.getTime() == null ? "" : dto.getTime().replaceAll(" 23:59:59", ""));
	}

	private void loadSensorData() {
		rpcProvider.getSensorDataService().searchByDay(buildSearchCriteria(),
			new MethodCallback<List<SensorDataDto>>() {
				@Override
				public void onFailure(Method method, Throwable exception) {
					ClientUtil.handleRpcFailure(method, exception, eventBus);
				}
				@Override
				public void onSuccess(Method method, List<SensorDataDto> result) {
					eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
					updateView(result);
				}
			});
	}

	protected QueryCriteriaDto buildSearchCriteria() {
		QueryCriteriaDto criteriaDto = new QueryCriteriaDto();
		criteriaDto.setProjectId(getProjectId());
		criteriaDto.setDeviceId(scaleDto.getId());
		String keySearch = entityDto.getTime().replaceAll(" 23:59:59", " 00:00:00")
				+ "-" + entityDto.getTime();
		criteriaDto.setSearchKey(keySearch);
		criteriaDto.setSearchField("time");
		return criteriaDto;
	}

	protected Long getProjectId() {
		if (AppToken.hasTokenItem(AppToken.PROJECT)) {
			return AppToken.getProjectIdLong();
		}
		return null;
	}

	public void setWeighScale(WeighScaleDto scaleDto) {
		this.scaleDto = scaleDto;
		view.getLblId().setText(scaleDto.getName());
	}

	private void updateView(List<SensorDataDto> dtos) {
		view.getDataCollection().clear();
		for (SensorDataDto dto : dtos) {
			MaterialCollectionItem item = new MaterialCollectionItem();
			Label lblTime = new InlineLabel(dto.getTime());
			lblTime.addStyleName("time");
			Label lblVal = new InlineLabel(dto.getValue() + "");
			lblVal.addStyleName("val");
			Label lblIdx = new InlineLabel(dto.getIndex() + "");
			lblIdx.addStyleName("idx");
			item.add(lblTime);
			item.add(lblVal);
			item.add(lblIdx);
			view.getDataCollection().add(item);
		}
	}
}
