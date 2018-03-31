package net.ionoff.center.client.relay;

import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.user.cellview.client.CellTable;
import com.google.gwt.user.cellview.client.Column;
import com.google.gwt.user.cellview.client.TextColumn;

import gwt.material.design.client.constants.IconType;
import net.ionoff.center.client.common.AbstractTableView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.locale.ProjectLocale;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.RelayDto;

public class RelayTableView extends AbstractTableView<RelayDto> implements RelayTablePresenter.Display {
	
	private Column<RelayDto, String> nameColumn;
	private Column<RelayDto, String> controllerColumn;
	private Column<RelayDto, String> editColumn;
	
	private RelayEditView relayEditView;
	
	public RelayTableView() {
		super("relays");
		getToolBarView().getLblTitle().setIconType(IconType.POWER_SETTINGS_NEW);
		getToolBarView().getLblTitle().setText(ProjectLocale.getProjectConst().relay());
		
		getToolBarView().getBtnAdd().setVisible(false);
		getToolBarView().getBtnRemove().setVisible(false);
	}

	@Override
	public CellTable<RelayDto> createCellTable() {
		CellTable<RelayDto> cellTable = new CellTable<RelayDto>();

		TextColumn<RelayDto> idColumn = createIdColumn();
		cellTable.addColumn(idColumn, ID);
		cellTable.setColumnWidth(idColumn, COLUMN_ID_WIDTH, Unit.PX);

		nameColumn = createNameColumn();
		nameColumn.setSortable(true);
		cellTable.addColumn(nameColumn, AdminLocale.getAdminConst().name());
		cellTable.setColumnWidth(nameColumn, COLUMN_NAME_WIDTH, Unit.PX);
		
		controllerColumn =  createControllerColumn();
		controllerColumn.setSortable(true);
		cellTable.addColumn(controllerColumn, AdminLocale.getAdminConst().controller());
		cellTable.setColumnWidth(controllerColumn, 150.0, Unit.PX);
		
		editColumn = createEditColumn();
		cellTable.addColumn(editColumn, "");
		
		return cellTable;
	}
	
	protected Column<RelayDto, String> createControllerColumn() {
		TextColumn<RelayDto> column = new TextColumn<RelayDto>() {
			@Override
			public String getValue(RelayDto object) {
				if (object == null) {
					return "";
				}
				return BaseDto.formatNameID(object.getControllerName(), object.getControllerId());
			}
		};
		return column;
	}
	
	@Override
	public Column<RelayDto, String> getNameColumn() { 
		return this.nameColumn;
	}

	@Override
	public Column<RelayDto, String> getControllerColumn() { 
		return this.controllerColumn;
	}


	@Override
	public Column<RelayDto, String> getEditColumn() {
		return editColumn;
	}
	
	@Override
	public RelayEditView getRelayEditView() {
		if (relayEditView == null) {
			relayEditView = new RelayEditView();
			add(relayEditView);
		}
		return relayEditView;
	}
	@Override
	public void hideEditForm() {
		super.hideEditForm();
		getRelayEditView().setStyleName("edit invisible");
	}
	
	@Override
	public void showEditForm() {
		super.showEditForm();
		getRelayEditView().setStyleName("edit visible");
	}
}
