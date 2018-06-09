package net.ionoff.center.client.mode;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.cell.client.FieldUpdater;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.cellview.client.Column;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.view.client.AsyncDataProvider;
import com.google.gwt.view.client.HasData;

import net.ionoff.center.client.base.AbstractTablePresenter;
import net.ionoff.center.client.base.ITableView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.EntityService;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.ModeDto;

public class ModeTablePresenter extends AbstractTablePresenter<ModeDto>{
	
	public interface Display extends ITableView<ModeDto> {
		@Override
		Column<ModeDto, String> getEditColumn();
		Column<ModeDto, String> getNameColumn();
		ModeEditPresenter.Display getModeEditView();
	}
	
	private final Display view;
	private ModeEditPresenter modeEditPresenter;
	
	public ModeTablePresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus,
			Display view) {
		super(rpcProvider, eventBus, view);
		this.view = view;
		sortFieldName = BaseDto.NAME;
	}
	
	@Override
	public void bind() {
		super.bind();
		view.getEditColumn().setFieldUpdater(new FieldUpdater<ModeDto, String>() {
			@Override
			public void update(int index, ModeDto object, String value) {
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
	}
	
	@Override
	protected void save() {
		
	}

	@Override
	protected void add() {
		List<ModeDto> modeDtos = new ArrayList<ModeDto>();
		ModeDto newModeDto = newModeDto();
		modeDtos.add(newModeDto);
		modeDtos.addAll(getDisplayingDtos());
		showEntities(modeDtos, newModeDto.getId(), 0);
		showEditForm();
		getModeEditPresenter().setEntityDto(newModeDto);
	}

	private ModeDto newModeDto() {
		ModeDto newModeDto = new ModeDto();
		newModeDto.setId(BaseDto.DEFAULT_ID);
		newModeDto.setName("*" + AdminLocale.getAdminConst().mode());
		newModeDto.setIsScheduled(false);
		newModeDto.setProjectId(getProjectId());
		return newModeDto;
	}
	
	@Override
	protected AsyncDataProvider<ModeDto> newAsyncDataProvider() {
		final AsyncDataProvider<ModeDto> provider = new AsyncDataProvider<ModeDto>() {
			@Override
			protected void onRangeChanged(HasData<ModeDto> hasData) {
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
		String driverName = AdminLocale.getAdminConst().name() + " " + AdminLocale.getAdminConst().relayDriver();
		display.getToolBarView().getLisBoxSearchBy().addItem(driverName);
	}

	@Override
	protected boolean isSameId(ModeDto entity, Long selectedId) {
		return entity.getId() == selectedId;
	}

	@Override
	protected String getClazz() {
		return ModeDto.class + "";
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
	protected EntityService<ModeDto> getRpcService() {
		return rpcProvider.getModeService();
	}
	
	@Override
	protected String getSortByField(int columnIndex) {
		if (columnIndex == 1) {
			return BaseDto.NAME;
		}
		return BaseDto.ORDER;
	}
	
	public void hideEditForm() {
		view.hideEditForm();
	}
	
	private void showEditForm() {
		view.showEditForm();
	}
	
	@Override
	protected void setSelectedObject(ModeDto modeDto) {
		getModeEditPresenter().setEntityDto(modeDto);
	} 
	
	public ModeEditPresenter getModeEditPresenter() {
		if (modeEditPresenter == null) {
			modeEditPresenter = new ModeEditPresenter(rpcProvider, eventBus, view.getModeEditView(), this);
			modeEditPresenter.go();
		}
		return modeEditPresenter;
	}
}
