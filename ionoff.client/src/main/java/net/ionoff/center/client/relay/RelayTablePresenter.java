package net.ionoff.center.client.relay;

import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.cell.client.FieldUpdater;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.cellview.client.Column;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.view.client.AsyncDataProvider;
import com.google.gwt.view.client.HasData;

import net.ionoff.center.client.base.AbstractTablePresenter;
import net.ionoff.center.client.base.ITableView;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.EntityService;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.AreaDto;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.RelayDto;

public class RelayTablePresenter extends AbstractTablePresenter<RelayDto>{
	
	public interface Display extends ITableView<RelayDto> {
		Column<RelayDto, String> getNameColumn();
		Column<RelayDto, String> getControllerColumn();
		RelayEditPresenter.Display getRelayEditView();
	}
	
	private final Display view;
	private RelayEditPresenter relayEditPresenter;
			
	public RelayTablePresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus,
			Display view) {
		super(rpcProvider, eventBus, view);
		this.view = view;
	}
	
	@Override
	public void bind() {
		super.bind();
		view.getEditColumn().setFieldUpdater(new FieldUpdater<RelayDto, String>() {
			@Override
			public void update(int index, RelayDto object, String value) {
				showEditForm();
			}
		});
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
		rpcProvider.getAreaService().findByProjectId(getProjectId(), true, true, new MethodCallback<List<AreaDto>>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}

			@Override
			public void onSuccess(Method method, List<AreaDto> result) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				getRelayEditPresenter().setDeviceOptions(result);
			}
		});
	}
	
	@Override
	protected void save() {
		
		rpcProvider.getRelayService().save(getUnsavedDto().getId(), getUnsavedDto(), new MethodCallback<RelayDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, RelayDto result) {
				onSavedSucess(result);
			}
		});
	}
	
	@Override
	public void setUnsavedDto(RelayDto unsavedDto) {
	}

	@Override
	protected void add() {
		// does nothing
	}
	
	@Override
	protected void delete() {
		// does nothing
	}
	
	@Override
	protected AsyncDataProvider<RelayDto> newAsyncDataProvider() {
		final AsyncDataProvider<RelayDto> provider = new AsyncDataProvider<RelayDto>() {
			@Override
			protected void onRangeChanged(HasData<RelayDto> hasData) {
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
		String deviceName = AdminLocale.getAdminConst().name() + " " + AdminLocale.getAdminConst().device();
		display.getToolBarView().getLisBoxSearchBy().addItem(deviceName);
		String driverName = AdminLocale.getAdminConst().name() + " " + AdminLocale.getAdminConst().controller();
		display.getToolBarView().getLisBoxSearchBy().addItem(driverName);
	}

	@Override
	protected boolean isSameId(RelayDto entity, Long selectedId) {
		return entity.getId() == selectedId;
	}

	@Override
	protected String getClazz() {
		return RelayDto.class + "";
	}

	@Override
	protected String getSearchBy() {
		int selectedSearchByIndex = display.getToolBarView().getLisBoxSearchBy().getSelectedIndex();
		if (selectedSearchByIndex == 0) {
			return "name";
		}
		if (selectedSearchByIndex == 1) {
			return "deviceName";
		}
		return "driverName";
	}

	@Override
	protected EntityService<RelayDto> getRpcService() {
		return rpcProvider.getRelayService();
	}

	@Override
	protected String getSortByField(int columnIndex) {
		if (columnIndex == 1) {
			return RelayDto.DRIVER;
		}
		if (columnIndex == 2) {
			return RelayDto.DEVICE;
		}
		return BaseDto.ID ;
	}
	
	public void hideEditForm() {
		view.hideEditForm();
	}
	
	private void showEditForm() {
		view.showEditForm();
		
	}
	
	@Override
	protected void setSelectedObject(RelayDto selectedDto) {
		getRelayEditPresenter().setEntityDto(selectedDto);
	} 
	
	public RelayEditPresenter getRelayEditPresenter() {
		if (relayEditPresenter == null) {
			relayEditPresenter = new RelayEditPresenter(rpcProvider, eventBus, view.getRelayEditView(), this);
			relayEditPresenter.go();
		}
		return relayEditPresenter;
	}
}
