package net.ionoff.center.client.relaydriver;


import com.google.gwt.cell.client.EditTextCell;
import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.user.cellview.client.CellTable;
import com.google.gwt.user.cellview.client.Column;
import com.google.gwt.user.cellview.client.TextColumn;

import gwt.material.design.client.constants.IconType;
import net.ionoff.center.client.base.AbstractTableView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.locale.ProjectLocale;
import net.ionoff.center.shared.dto.RelayDriverDto;

public class RelayDriverTableView extends AbstractTableView<RelayDriverDto> implements RelayDriverTablePresenter.Display {
	
	private Column<RelayDriverDto, String> nameColumn;
	private Column<RelayDriverDto, String> statusColumn;
	private Column<RelayDriverDto, String> editColumn;
	
	private RelayDriverEditView relayDriverEditView;
	
	public RelayDriverTableView() {
		super("relayDrivers");
		getToolBarView().getLblTitle().setIconType(IconType.MEMORY);
		getToolBarView().getLblTitle().setText(ProjectLocale.getProjectConst().relayDriver());
	}

	@Override
	public CellTable<RelayDriverDto> createCellTable() {
		CellTable<RelayDriverDto> cellTable = new CellTable<RelayDriverDto>();
		
		TextColumn<RelayDriverDto> idColumn = createIdColumn();
		cellTable.addColumn(idColumn, ID);
		cellTable.setColumnWidth(idColumn, COLUMN_ID_WIDTH, Unit.PX);

		nameColumn = createNameColumn();
		nameColumn.setSortable(true);
		cellTable.addColumn(nameColumn, AdminLocale.getAdminConst().name());
		cellTable.setColumnWidth(nameColumn, COLUMN_NAME_WIDTH, Unit.PX);
		
		statusColumn =  createStatusColumn();
		statusColumn.setSortable(true);
		cellTable.addColumn(statusColumn, AdminLocale.getAdminConst().status());
		cellTable.setColumnWidth(statusColumn, 150.0, Unit.PX);
		
		editColumn =  createEditColumn();
		cellTable.addColumn(editColumn, "");
		
		return cellTable;
	}
	
	protected Column<RelayDriverDto, String> createStatusColumn() {
		Column<RelayDriverDto, String> column = new Column<RelayDriverDto, String>(new EditTextCell()) {
			@Override
			public String getValue(RelayDriverDto object) {
				return Boolean.TRUE.equals(object.getIsOnline()) ? "Online" : "Offline";
			}
		};
		return column;
	}

	@Override
	public Column<RelayDriverDto, String> getNameColumn() {
		return nameColumn;
	}

	@Override
	public Column<RelayDriverDto, String> getIpColumn() {
		return statusColumn;
	}

	@Override
	public Column<RelayDriverDto, String> getEditColumn() {
		return editColumn;
	}

	@Override
	public RelayDriverEditView getRelayDriverEditView() {
		if (relayDriverEditView == null) {
			relayDriverEditView = new RelayDriverEditView();
			add(relayDriverEditView);
		}
		return relayDriverEditView;
	}
	@Override
	public void hideEditForm() {
		super.hideEditForm();
		getRelayDriverEditView().setStyleName("edit invisible");
	}
	
	@Override
	public void showEditForm() {
		super.showEditForm();
		getRelayDriverEditView().setStyleName("edit visible");
	}
}
