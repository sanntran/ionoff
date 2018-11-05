package net.ionoff.center.client.controller;


import com.google.gwt.cell.client.EditTextCell;
import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.user.cellview.client.CellTable;
import com.google.gwt.user.cellview.client.Column;
import com.google.gwt.user.cellview.client.TextColumn;

import gwt.material.design.client.constants.IconType;
import net.ionoff.center.client.base.AbstractTableView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.locale.ProjectLocale;
import net.ionoff.center.shared.dto.ControllerDto;

public class ControllerTableView extends AbstractTableView<ControllerDto> implements ControllerTablePresenter.Display {
	
	private Column<ControllerDto, String> nameColumn;
	private Column<ControllerDto, String> statusColumn;
	private Column<ControllerDto, String> editColumn;
	
	private ControllerEditView controllerEditView;
	
	public ControllerTableView() {
		super("controllers");
		getToolBarView().getLblTitle().setIconType(IconType.MEMORY);
		getToolBarView().getLblTitle().setText(ProjectLocale.getProjectConst().controller());
	}

	@Override
	public CellTable<ControllerDto> createCellTable() {
		CellTable<ControllerDto> cellTable = new CellTable<ControllerDto>();
		
		nameColumn = createNameColumn();
		nameColumn.setSortable(true);
		cellTable.addColumn(nameColumn, AdminLocale.getAdminConst().name());

		statusColumn =  createStatusColumn();
		statusColumn.setSortable(true);
		cellTable.addColumn(statusColumn, AdminLocale.getAdminConst().status());
		cellTable.setColumnWidth(statusColumn, 100.0, Unit.PX);

		editColumn =  createEditColumn();
		cellTable.addColumn(editColumn, "");
		cellTable.setColumnWidth(editColumn, COLUMN_EDIT_WIDTH, Unit.PX);

		return cellTable;
	}
	
	protected Column<ControllerDto, String> createStatusColumn() {
		Column<ControllerDto, String> column = new Column<ControllerDto, String>(new EditTextCell()) {
			@Override
			public String getValue(ControllerDto object) {
				return Boolean.TRUE.equals(object.getIsOnline()) ? "Online" : "Offline";
			}
		};
		return column;
	}

	@Override
	public Column<ControllerDto, String> getNameColumn() {
		return nameColumn;
	}

	@Override
	public Column<ControllerDto, String> getIpColumn() {
		return statusColumn;
	}

	@Override
	public Column<ControllerDto, String> getEditColumn() {
		return editColumn;
	}

	@Override
	public ControllerEditView getControllerEditView() {
		if (controllerEditView == null) {
			controllerEditView = new ControllerEditView();
			add(controllerEditView);
		}
		return controllerEditView;
	}
	@Override
	public void hideEditForm() {
		super.hideEditForm();
		getControllerEditView().setStyleName("edit invisible");
	}
	
	@Override
	public void showEditForm() {
		super.showEditForm();
		getControllerEditView().setStyleName("edit visible");
	}
}
