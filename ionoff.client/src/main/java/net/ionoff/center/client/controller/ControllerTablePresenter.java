package net.ionoff.center.client.controller;

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
import net.ionoff.center.shared.dto.ControllerDto;

public class ControllerTablePresenter extends AbstractTablePresenter<ControllerDto>{
	
	public interface Display extends ITableView<ControllerDto> {
		Column<ControllerDto, String> getNameColumn();
		Column<ControllerDto, String> getIpColumn();
		ControllerEditPresenter.Display getControllerEditView();
	}
	
	private final Display view;
	private ControllerEditPresenter controllerEditPresenter;
			
	public ControllerTablePresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus,
	                                Display view) {
		super(rpcProvider, eventBus, view);
		this.view = view;
	}
	
	@Override
	public void bind() {
		super.bind();
		view.getEditColumn().setFieldUpdater(new FieldUpdater<ControllerDto, String>() {
			@Override
			public void update(int index, ControllerDto object, String value) {
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
		List<ControllerDto> controllerDtos = new ArrayList<ControllerDto>();
		ControllerDto newControllerDto = newControllerDto();
		controllerDtos.add(newControllerDto);
		controllerDtos.addAll(getDisplayingDtos());
		showEntities(controllerDtos, newControllerDto.getId(), 0);
		showEditForm();
		getControllerEditPresenter().setEntityDto(newControllerDto);
	}

	private ControllerDto newControllerDto() {
		ControllerDto newControllerDto = new ControllerDto();
		newControllerDto.setId(BaseDto.DEFAULT_ID);
		newControllerDto.setName("*" + AdminLocale.getAdminConst().controller());
		newControllerDto.setModel(ControllerModel.IONOFF_E3.toString());
		newControllerDto.setProjectId(getProjectId());
		return newControllerDto;
	}

	@Override
	protected AsyncDataProvider<ControllerDto> newAsyncDataProvider() {
		final AsyncDataProvider<ControllerDto> provider = new AsyncDataProvider<ControllerDto>() {
			@Override
			protected void onRangeChanged(HasData<ControllerDto> hasData) {
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
	protected boolean isSameId(ControllerDto entity, Long selectedId) {
		return entity.getId() == selectedId;
	}

	@Override
	protected String getClazz() {
		return ControllerDto.class + "";
	}

	@Override
	protected String getSearchBy() {
		return "name";
	}

	@Override
	protected EntityService<ControllerDto> getRpcService() {
		return rpcProvider.getControllerService();
	}

	@Override
	protected String getSortByField(int columnIndex) {
		if (columnIndex == 1) {
			return "connectedTime";
		}
		return BaseDto.NAME;
	}
	
	public void hideEditForm() {
		view.hideEditForm();
	}
	
	private void showEditForm() {
		view.showEditForm();
		
	}
	
	@Override
	protected void setSelectedObject(ControllerDto selectedDto) {
		getControllerEditPresenter().setEntityDto(selectedDto);
	} 
	
	public ControllerEditPresenter getControllerEditPresenter() {
		if (controllerEditPresenter == null) {
			controllerEditPresenter = new ControllerEditPresenter(rpcProvider, eventBus, view.getControllerEditView(), this);
			controllerEditPresenter.go();
		}
		return controllerEditPresenter;
	}
}
