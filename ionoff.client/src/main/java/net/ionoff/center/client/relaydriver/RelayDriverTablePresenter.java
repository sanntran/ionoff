package net.ionoff.center.client.relaydriver;

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
import net.ionoff.center.shared.dto.RelayDriverDto;
import net.ionoff.center.shared.entity.RelayDriverModel;

public class RelayDriverTablePresenter extends AbstractTablePresenter<RelayDriverDto>{
	
	public interface Display extends ITableView<RelayDriverDto> {
		Column<RelayDriverDto, String> getNameColumn();
		Column<RelayDriverDto, String> getIpColumn();
		RelayDriverEditPresenter.Display getRelayDriverEditView();
	}
	
	private final Display view;
	private RelayDriverEditPresenter relayDriverEditPresenter;
			
	public RelayDriverTablePresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus,
			Display view) {
		super(rpcProvider, eventBus, view);
		this.view = view;
	}
	
	@Override
	public void bind() {
		super.bind();
		view.getEditColumn().setFieldUpdater(new FieldUpdater<RelayDriverDto, String>() {
			@Override
			public void update(int index, RelayDriverDto object, String value) {
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
		List<RelayDriverDto> relayDriverDtos = new ArrayList<RelayDriverDto>();
		RelayDriverDto newRelayDriverDto = newRelayDriverDto();
		relayDriverDtos.add(newRelayDriverDto);
		relayDriverDtos.addAll(getDisplayingDtos());
		showEntities(relayDriverDtos, newRelayDriverDto.getId(), 0);
		showEditForm();
		getRelayDriverEditPresenter().setEntityDto(newRelayDriverDto);
	}

	private RelayDriverDto newRelayDriverDto() {
		RelayDriverDto newRelayDriverDto = new RelayDriverDto();
		newRelayDriverDto.setId(BaseDto.DEFAULT_ID);
		newRelayDriverDto.setName("*" + AdminLocale.getAdminConst().relayDriver());
		newRelayDriverDto.setModel(RelayDriverModel.IONOFF_E3);
		newRelayDriverDto.setProjectId(getProjectId());
		return newRelayDriverDto;
	}

	@Override
	protected AsyncDataProvider<RelayDriverDto> newAsyncDataProvider() {
		final AsyncDataProvider<RelayDriverDto> provider = new AsyncDataProvider<RelayDriverDto>() {
			@Override
			protected void onRangeChanged(HasData<RelayDriverDto> hasData) {
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
	protected boolean isSameId(RelayDriverDto entity, Long selectedId) {
		return entity.getId() == selectedId;
	}

	@Override
	protected String getClazz() {
		return RelayDriverDto.class + "";
	}

	@Override
	protected String getSearchBy() {
		return "name";
	}

	@Override
	protected EntityService<RelayDriverDto> getRpcService() {
		return rpcProvider.getRelayDriverService();
	}

	@Override
	protected String getSortByField(int columnIndex) {
		if (columnIndex == 2) {
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
	protected void setSelectedObject(RelayDriverDto selectedDto) {
		getRelayDriverEditPresenter().setEntityDto(selectedDto);
	} 
	
	public RelayDriverEditPresenter getRelayDriverEditPresenter() {
		if (relayDriverEditPresenter == null) {
			relayDriverEditPresenter = new RelayDriverEditPresenter(rpcProvider, eventBus, view.getRelayDriverEditView(), this);
			relayDriverEditPresenter.go();
		}
		return relayDriverEditPresenter;
	}
}
