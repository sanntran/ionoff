package net.ionoff.center.client.schedule;

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

import net.ionoff.center.client.base.AbstractTablePresenter;
import net.ionoff.center.client.base.ITableView;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.event.ShowMessageEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.EntityService;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.AreaDto;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.ScheduleConst;
import net.ionoff.center.shared.dto.ScheduleDto;

public class ScheduleTablePresenter extends AbstractTablePresenter<ScheduleDto>{
	
	public interface Display extends ITableView<ScheduleDto> {
		Column<ScheduleDto, String> getNameColumn();
		Column<ScheduleDto, String> getDeviceColumn();
		ScheduleEditPresenter.Display getScheduleEditView();
	}
	
	private final Display view;
	private ScheduleEditPresenter scheduleEditPresenter;
	
	public ScheduleTablePresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus,
			Display view) {
		super(rpcProvider, eventBus, view);
		this.view = view;
	}
	
	@Override
	public void bind() {
		super.bind();
		view.getEditColumn().setFieldUpdater(new FieldUpdater<ScheduleDto, String>() {
			@Override
			public void update(int index, ScheduleDto object, String value) {
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
				getScheduleEditPresenter().setDeviceOptions(result);
			}
		});
	}
	
	@Override
	protected void save() {
		rpcProvider.getScheduleService().save(getUnsavedDto().getId(), getUnsavedDto(), 
				new MethodCallback<ScheduleDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, ScheduleDto result) {
				onSavedSucess(result);
			}
		});
	}
	
	@Override
	public void setUnsavedDto(ScheduleDto unsavedDto) {
		super.setUnsavedDto(unsavedDto);
	}

	@Override
	protected void add() {
		List<ScheduleDto> scheduleDtos = new ArrayList<ScheduleDto>();
		ScheduleDto newScheduleDto = newScheduleDto();
		scheduleDtos.add(newScheduleDto);
		scheduleDtos.addAll(getDisplayingDtos());
		showEntities(scheduleDtos, newScheduleDto.getId(), 0);
		showEditForm();
		getScheduleEditPresenter().setEntityDto(newScheduleDto);
	}

	private ScheduleDto newScheduleDto() {
		ScheduleDto newScheduleDto = new ScheduleDto();
		newScheduleDto.setId(BaseDto.DEFAULT_ID);
		newScheduleDto.setRepeat(ScheduleConst.REPEAT_ONCE);
		newScheduleDto.setDay("");
		newScheduleDto.setTime("00:00 AM");
		newScheduleDto.setName("*" + AdminLocale.getAdminConst().schedule());
		newScheduleDto.setProjectId(getProjectId());
		return newScheduleDto;
	}
	
	@Override
	protected AsyncDataProvider<ScheduleDto> newAsyncDataProvider() {
		final AsyncDataProvider<ScheduleDto> provider = new AsyncDataProvider<ScheduleDto>() {
			@Override
			protected void onRangeChanged(HasData<ScheduleDto> hasData) {
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
	}

	@Override
	protected boolean isSameId(ScheduleDto entity, Long selectedId) {
		return entity.getId() == selectedId;
	}

	@Override
	protected boolean validateBeforeSaving() {
		if (!super.validateBeforeSaving()) {
			return false;
		}
		if (getUnsavedDto().getDeviceId() == null) {
			String message = AdminLocale.getAdminMessages().emptyInputValue(AdminLocale.getAdminConst().device());
			eventBus.fireEvent(new ShowMessageEvent(message, ShowMessageEvent.ERROR));
			return false;
		}
		return true;
	}

	@Override
	protected String getClazz() {
		return ScheduleDto.class + "";
	}

	@Override
	protected String getSearchBy() {
		int selectedSearchByIndex = display.getToolBarView().getLisBoxSearchBy().getSelectedIndex();
		if (selectedSearchByIndex == 0) {
			return "name";
		}
		return "deviceName";
	}

	@Override
	protected EntityService<ScheduleDto> getRpcService() {
		return rpcProvider.getScheduleService();
	}
	
	@Override
	protected String getSortByField(int columnIndex) {
		if (columnIndex == 1) {
			return BaseDto.NAME;
		}
		else if(columnIndex == 2) {
			return BaseDto.ORDER;
		}
		return "device";
	}
	
	public void hideEditForm() {
		view.hideEditForm();
	}
	
	private void showEditForm() {
		view.showEditForm();
	}
	
	@Override
	protected void setSelectedObject(ScheduleDto selectedDto) {
		getScheduleEditPresenter().setEntityDto(selectedDto);
	} 
	
	public ScheduleEditPresenter getScheduleEditPresenter() {
		if (scheduleEditPresenter == null) {
			scheduleEditPresenter = new ScheduleEditPresenter(rpcProvider, eventBus, view.getScheduleEditView(), this);
			scheduleEditPresenter.go();
		}
		return scheduleEditPresenter;
	}
}
