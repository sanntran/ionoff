package net.ionoff.center.client.sensor;

import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.cellview.client.Column;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.view.client.AsyncDataProvider;
import com.google.gwt.view.client.HasData;
import net.ionoff.center.client.base.AbstractTablePresenter;
import net.ionoff.center.client.base.ITableView;
import net.ionoff.center.client.event.ShowMessageEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.EntityService;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.SensorDto;
import net.ionoff.center.shared.entity.SensorType;
import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import java.util.ArrayList;
import java.util.List;

public class SensorTablePresenter extends AbstractTablePresenter<SensorDto> {
	
	public interface Display extends ITableView<SensorDto> {
		Column<SensorDto, String> getNameColumn();
		Column<SensorDto, String> getControllerColumn();
		SensorEditPresenter.Display getSensorEditView();
	}
	
	private final Display view;
	private SensorEditPresenter sensorEditPresenter;
			
	public SensorTablePresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus,
			Display view) {
		super(rpcProvider, eventBus, view);
		this.view = view;
	}
	
	@Override
	public void bind() {
		super.bind();
	}

	@Override
	public void go() {
		bind();
	}

	@Override
	public void show(HasWidgets container) {
		container.clear();
		container.add(display.asWidget());
	}
	
	@Override
	protected void save() {
		rpcProvider.getSensorService().save(getUnsavedDto().getId(), getUnsavedDto(), 
				new MethodCallback<SensorDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, SensorDto result) {
				onSavedSucess(result);
			}
		});
	}
	
	@Override
	protected boolean validateBeforeSaving() {
		if (!super.validateBeforeSaving()) {
			return false;
		}
		if (!validateInputNumberValue(AdminLocale.getAdminConst().input(), getUnsavedDto().getIndex() + "")) {
			return false;
		}
		if (getUnsavedDto().getIndex() < 0) {
			final String message = AdminLocale.getAdminMessages().invalidFieldValue(AdminLocale.getAdminConst().input());
			eventBus.fireEvent(new ShowMessageEvent(message, ShowMessageEvent.ERROR));
			return false;
		}
		return true;
	}

	@Override
	protected void add() {
		List<SensorDto> sensorDtos = new ArrayList<SensorDto>();
		SensorDto newSensorDto = newSensorDto();
		sensorDtos.add(newSensorDto);
		sensorDtos.addAll(getDisplayingDtos());
		showEntities(sensorDtos, newSensorDto.getId(), 0);
		showEditForm();
		getSensorEditPresenter().setEntityDto(newSensorDto);
	}

	private SensorDto newSensorDto() {
		SensorDto newSensorDto = new SensorDto();
		newSensorDto.setId(BaseDto.DEFAULT_ID);
		newSensorDto.setType(SensorType.DIGITAL.toString());
		newSensorDto.setProjectId(getProjectId());
		newSensorDto.setName("*" + AdminLocale.getAdminConst().sensor());
		newSensorDto.setIndex(0);
		return newSensorDto;
	}

	@Override
	protected AsyncDataProvider<SensorDto> newAsyncDataProvider() {
		final AsyncDataProvider<SensorDto> provider = new AsyncDataProvider<SensorDto>() {
			@Override
			protected void onRangeChanged(HasData<SensorDto> hasData) {
				final int start = hasData.getVisibleRange().getStart();
				// int length = hasData.getVisibleRange().getLength();
				loadData(null, false, start);
			}
		};
		return provider;
	}

	@Override
	protected void fillListBoxSearchBy() {
		display.getToolBarView().getLisBoxSearchBy().addItem(AdminLocale.getAdminConst().name());
	}

	@Override
	protected boolean isSameId(SensorDto entity, Long selectedId) {
		return entity.getId() == selectedId;
	}

	@Override
	protected String getClazz() {
		return SensorDto.class + "";
	}

	@Override
	protected String getSearchBy() {
		return "name";
	}

	@Override
	protected EntityService<SensorDto> getRpcService() {
		return rpcProvider.getSensorService();
	}
	
	@Override
	protected String getSortByField(int columnIndex) {
		if (columnIndex == 0) {
			return BaseDto.NAME;
		}
		else if (columnIndex == 1) {
			return BaseDto.ORDER;
		}
		return "device";
	}
	
	public SensorEditPresenter getSensorEditPresenter() {
		if (sensorEditPresenter == null) {
			sensorEditPresenter = new SensorEditPresenter(rpcProvider, eventBus, view.getSensorEditView(), this);
			sensorEditPresenter.go();
		}
		return sensorEditPresenter;
	}

	public void hideEditForm() {
		view.hideEditForm();
	}
	
	@Override
	protected void setSelectedObject(SensorDto selectedDto) {
		getSensorEditPresenter().setEntityDto(selectedDto);
	}

	private void showEditForm() {
		view.showEditForm();
	}
	
	@Override
	protected void delete() {
		SensorDto sensor = getSelectedObject();
		if (sensor.getDeviceId() != null) {
			final String message = AdminLocale.getAdminMessages().errorDeleteSensorOfDevice(sensor.getDeviceName());
			eventBus.fireEvent(new ShowMessageEvent(message, ShowMessageEvent.ERROR));
			return;
		}
		super.delete();
	}
}
