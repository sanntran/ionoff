package net.ionoff.center.client.zone;

import java.util.ArrayList;
import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.cell.client.FieldUpdater;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.cellview.client.Column;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.view.client.AsyncDataProvider;
import com.google.gwt.view.client.HasData;

import net.ionoff.center.client.common.AbstractTablePresenter;
import net.ionoff.center.client.common.ITableView;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.EntityService;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.AreaDto;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.ZoneDto;

public class ZoneTablePresenter extends AbstractTablePresenter<ZoneDto>{
	
	public interface Display extends ITableView<ZoneDto> {
		static final int AREA_COLUMN_INDEX = 2;
		Column<ZoneDto, String> getNameColumn();
		Column<ZoneDto, String> getAreaColumn();
		ZoneEditPresenter.Display getZoneEditView();
		Column<ZoneDto, String> getOrderColumn();
	}
	
	private final Display view;
	private ZoneEditPresenter zoneEditPresenter;
	
	public ZoneTablePresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus,
			Display view) {
		super(rpcProvider, eventBus, view);
		this.view = view;
	}
	
	@Override
	public void bind() {
		super.bind();
		view.getEditColumn().setFieldUpdater(new FieldUpdater<ZoneDto, String>() {
			@Override
			public void update(int index, ZoneDto object, String value) {
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
		rpcProvider.getAreaService().findByProjectId(getProjectId(), false, false, 
				new MethodCallback<List<AreaDto>>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}

			@Override
			public void onSuccess(Method method, List<AreaDto> result) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				getZoneEditPresenter().setAreaOptions(result);
			}
		});
	}
	
	@Override
	protected void save() {
	}
	
	@Override
	protected boolean validateBeforeSaving() {
		return true;
	}

	@Override
	protected void add() {
		List<ZoneDto> zoneDtos = new ArrayList<ZoneDto>();
		ZoneDto newZoneDto = newZoneDto();
		zoneDtos.add(newZoneDto);
		zoneDtos.addAll(getDisplayingDtos());
		showEntities(zoneDtos, newZoneDto.getId(), 0);
		setUnsavedDto(newZoneDto);
		showEditForm();
		getZoneEditPresenter().setEntityDto(newZoneDto);
	}

	private ZoneDto newZoneDto() {
		ZoneDto newZoneDto = new ZoneDto();
		newZoneDto.setId(BaseDto.DEFAULT_ID);
		newZoneDto.setName("*" + AdminLocale.getAdminConst().zone());
		return newZoneDto;
	}

	@Override
	protected AsyncDataProvider<ZoneDto> newAsyncDataProvider() {
		final AsyncDataProvider<ZoneDto> provider = new AsyncDataProvider<ZoneDto>() {
			@Override
			protected void onRangeChanged(HasData<ZoneDto> hasData) {
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
		String areaName = AdminLocale.getAdminConst().name() + " " + AdminLocale.getAdminConst().area();
		display.getToolBarView().getLisBoxSearchBy().addItem(areaName);
	}

	@Override
	protected boolean isSameId(ZoneDto entity, Long selectedId) {
		return entity.getId() == selectedId;
	}

	@Override
	protected String getClazz() {
		return ZoneDto.class + "";
	}

	@Override
	protected String getSearchBy() {
		int selectedSearchByIndex = display.getToolBarView().getLisBoxSearchBy().getSelectedIndex();
		if (selectedSearchByIndex == 0) {
			return "name";
		}
		else {
			return "areaName";
		}
	}

	@Override
	protected EntityService<ZoneDto> getRpcService() {
		return rpcProvider.getZoneService();
	}
	
	@Override
	protected String getSortByField(int columnIndex) {
		if (columnIndex == 2) {
			return ZoneDto.ORDER;
		}
		if (columnIndex == 3) {
			return ZoneDto.AREA;
		}
		return BaseDto.NAME;
	}

	public ZoneEditPresenter getZoneEditPresenter() {
		if (zoneEditPresenter == null) {
			zoneEditPresenter = new ZoneEditPresenter(rpcProvider, eventBus, view.getZoneEditView(), this);
			zoneEditPresenter.go();
		}
		return zoneEditPresenter;
	}

	public void hideEditForm() {
		view.hideEditForm();
	}
	
	@Override
	protected void setSelectedObject(ZoneDto selectedDto) {
		getZoneEditPresenter().setEntityDto(selectedDto);
	}

	private void showEditForm() {
		view.showEditForm();
		
	}
}
