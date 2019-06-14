package net.ionoff.center.client.device;

import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.view.client.AsyncDataProvider;
import com.google.gwt.view.client.HasData;
import net.ionoff.center.client.base.AbstractTablePresenter;
import net.ionoff.center.client.base.ITableView;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.event.ShowMessageEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.EntityService;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.*;
import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import java.util.ArrayList;
import java.util.List;

public class DeviceTablePresenter extends AbstractTablePresenter<DeviceDto>{
	
	public interface Display extends ITableView<DeviceDto> {
		DeviceEditPresenter.Display getDeviceEditView();
	}
	
	private final Display view;
	private DeviceEditPresenter deviceEditPresenter;
	
	public DeviceTablePresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus,
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
		loadAreasByProjectId();
	}

	private void loadAreasByProjectId() {
		rpcProvider.getAreaService().findByProjectId(getProjectId(), true, false, new MethodCallback<List<AreaDto>>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, List<AreaDto> result) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				getDeviceEditPresenter().setZoneOptions(result);
			}
		});
	}
	
	@Override
	protected void save() {
	}
	
	@Override
	public void setUnsavedDto(DeviceDto unsavedDto) {
	}
	
	@Override
	protected boolean validateBeforeSaving() {
		if (!super.validateBeforeSaving()) {
			return false;
		}
		
		if (getUnsavedDto().getZoneId() == null) {
			String message = AdminLocale.getAdminMessages().emptyInputValue(AdminLocale.getAdminConst().zone());
			eventBus.fireEvent(new ShowMessageEvent(message, ShowMessageEvent.ERROR));
			return false;
		}
		if (getUnsavedDto() instanceof MediaPlayerDto) {
			MediaPlayerDto playerDto = (MediaPlayerDto)getUnsavedDto();
			if (playerDto.getMac() == null || playerDto.getMac().trim().isEmpty()) {
				String message = AdminLocale.getAdminMessages().emptyInputValue(AdminLocale.getAdminConst().mac());
				eventBus.fireEvent(new ShowMessageEvent(message, ShowMessageEvent.ERROR));
				return false;
			}
		}
		return true;
	}

	@Override
	protected void add() {
		List<DeviceDto> deviceDtos = new ArrayList<DeviceDto>();
		DeviceDto newDeviceDto = newRelayLoadDto();
		newDeviceDto.setOrder(DeviceDto.DEFAULT_ORDER);
		deviceDtos.add(newDeviceDto);
		deviceDtos.addAll(getDisplayingDtos());
		showEntities(deviceDtos, newDeviceDto.getId(), 0);
		showEditForm();
		getDeviceEditPresenter().setEntityDto(newDeviceDto);
	}

	private RelayLoadDto newRelayLoadDto() {
		RelayLoadDto newRelayLoadDto = new RelayLoadDto();
		newRelayLoadDto.setId(BaseDto.DEFAULT_ID);
		newRelayLoadDto.setName("*" + AdminLocale.getAdminConst().device());
		if (!getDeviceEditPresenter().getZoneDtos().isEmpty()) {
			newRelayLoadDto.setZoneId(getDeviceEditPresenter().getZoneDtos().get(0).getId());
			newRelayLoadDto.setZoneName(getDeviceEditPresenter().getZoneDtos().get(0).getName());
		}
		return newRelayLoadDto;
	}

	@Override
	protected AsyncDataProvider<DeviceDto> newAsyncDataProvider() {
		final AsyncDataProvider<DeviceDto> provider = new AsyncDataProvider<DeviceDto>() {
			@Override
			protected void onRangeChanged(HasData<DeviceDto> hasData) {
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
		String zoneName = AdminLocale.getAdminConst().name() + " " + AdminLocale.getAdminConst().zone();
		display.getToolBarView().getLisBoxSearchBy().addItem(zoneName);
	}

	@Override
	protected boolean isSameId(DeviceDto entity, Long selectedId) {
		return entity.getId() == selectedId;
	}

	@Override
	protected String getClazz() {
		return DeviceDto.class + "";
	}

	@Override
	protected String getSearchBy() {
		int selectedSearchByIndex = display.getToolBarView().getLisBoxSearchBy().getSelectedIndex();
		if (selectedSearchByIndex == 0) {
			return "name";
		}
		else {
			return "zoneName";
		}
	}

	@Override
	protected EntityService<DeviceDto> getRpcService() {
		return rpcProvider.getDeviceService();
	}

	@Override
	protected String getSortByField(int columnIndex) {
		if (columnIndex == 1) {
			return DeviceDto.ORDER;
		}
		if (columnIndex == 2) {
			return DeviceDto.ZONE;
		}
		else return BaseDto.NAME;
	}
	
	public void hideEditForm() {
		view.hideEditForm();
	}
	
	private void showEditForm() {
		view.showEditForm();
		
	}
	
	@Override
	protected void setSelectedObject(DeviceDto selectedDto) {
		getDeviceEditPresenter().setEntityDto(selectedDto);
	} 
	
	public DeviceEditPresenter getDeviceEditPresenter() {
		if (deviceEditPresenter == null) {
			deviceEditPresenter = new DeviceEditPresenter(rpcProvider, eventBus, view.getDeviceEditView(), this);
			deviceEditPresenter.go();
		}
		return deviceEditPresenter;
	}
}
