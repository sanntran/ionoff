package net.ionoff.center.client.area;

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
import net.ionoff.center.shared.dto.AreaDto;
import net.ionoff.center.shared.dto.BaseDto;

public class AreaTablePresenter extends AbstractTablePresenter<AreaDto>{
	
	public interface Display extends ITableView<AreaDto> {
		Column<AreaDto, String> getNameColumn();
		AreaEditPresenter.Display getAreaEditView();
	}
	
	private final Display view;
	private AreaEditPresenter areaEditPresenter;
	
	public AreaTablePresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus,
			Display view) {
		super(rpcProvider, eventBus, view);
		this.view = view;
	}
	
	@Override
	public void bind() {
		super.bind();
		view.getEditColumn().setFieldUpdater(new FieldUpdater<AreaDto, String>() {
			@Override
			public void update(int index, AreaDto object, String value) {
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
		List<AreaDto> areaDtos = new ArrayList<AreaDto>();
		AreaDto newAreaDto = newAreaDto();
		areaDtos.add(newAreaDto);
		areaDtos.addAll(getDisplayingDtos());
		showEntities(areaDtos, newAreaDto.getId(), 0);
		showEditForm();
		getAreaEditPresenter().setEntityDto(newAreaDto);
	}

	private AreaDto newAreaDto() {
		AreaDto newAreaDto = new AreaDto();
		newAreaDto.setId(BaseDto.DEFAULT_ID);
		newAreaDto.setName("*" + AdminLocale.getAdminConst().area());
		newAreaDto.setProjectId(getProjectId());
		return newAreaDto;
	}

	@Override
	protected AsyncDataProvider<AreaDto> newAsyncDataProvider() {
		final AsyncDataProvider<AreaDto> provider = new AsyncDataProvider<AreaDto>() {
			@Override
			protected void onRangeChanged(HasData<AreaDto> hasData) {
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
	protected boolean isSameId(AreaDto entity, Long selectedId) {
		return entity.getId() == selectedId;
	}

	@Override
	protected String getClazz() {
		return AreaDto.class + "";
	}

	@Override
	protected String getSearchBy() {
		return "name";
	}

	@Override
	protected EntityService<AreaDto> getRpcService() {
		return rpcProvider.getAreaService();
	}

	@Override
	protected String getSortByField(int columnIndex) {
		if (columnIndex == 2) {
			return AreaDto.ORDER;
		}
		return BaseDto.NAME;
	}
	
	public void hideEditForm() {
		view.hideEditForm();
	}
	
	@Override
	protected void setSelectedObject(AreaDto selectedDto) {
		getAreaEditPresenter().setEntityDto(selectedDto);
	}

	private void showEditForm() {
		view.showEditForm();
	}
	
	public AreaEditPresenter getAreaEditPresenter() {
		if (areaEditPresenter == null) {
			areaEditPresenter = new AreaEditPresenter(rpcProvider, eventBus, view.getAreaEditView(), this);
			areaEditPresenter.go();
		}
		return areaEditPresenter;
	}
}
