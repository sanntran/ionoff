package net.ionoff.center.client.area;

import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.user.cellview.client.CellTable;
import com.google.gwt.user.cellview.client.Column;
import com.google.gwt.user.cellview.client.TextColumn;

import gwt.material.design.client.constants.IconType;
import net.ionoff.center.client.base.AbstractTableView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.locale.ProjectLocale;
import net.ionoff.center.shared.dto.AreaDto;

public class AreaTableView extends AbstractTableView<AreaDto> implements AreaTablePresenter.Display {
	
	private Column<AreaDto, String> nameColumn;
	private Column<AreaDto, String> orderColumn;
	private Column<AreaDto, String> editColumn;
	
	private AreaEditView areaEditView;
	
	public AreaTableView() {
		super("areas");
		getToolBarView().getLblTitle().setIconType(IconType.BORDER_ALL);
		getToolBarView().getLblTitle().setText(ProjectLocale.getProjectConst().area());
	}

	@Override
	public CellTable<AreaDto> createCellTable() {
		CellTable<AreaDto> cellTable = new CellTable<AreaDto>();
		
		nameColumn = createNameColumn();
		nameColumn.setSortable(true);
		cellTable.addColumn(nameColumn, AdminLocale.getAdminConst().name());
		
		orderColumn = createOrderColumn();
		orderColumn.setSortable(true);
		cellTable.setColumnWidth(orderColumn, 80, Unit.PX);
		cellTable.addColumn(orderColumn, AdminLocale.getAdminConst().order());
		
		editColumn = createEditColumn();
		cellTable.addColumn(editColumn, "");
		cellTable.setColumnWidth(editColumn, COLUMN_EDIT_WIDTH, Unit.PX);

		return cellTable;
	}
	
	private Column<AreaDto, String> createOrderColumn() {
		Column<AreaDto, String> column = new TextColumn<AreaDto>() {
	    	@Override
	    	public String getValue(AreaDto object) {
	    		if (object.getOrder() == null) {
	    			return "";
	    		}
	    		return object.getOrder() + "";
	    	}
	    };
		return column;
	}

	@Override
	public Column<AreaDto, String> getNameColumn() {
		return nameColumn;
	}
	
	@Override
	public Column<AreaDto, String> getEditColumn() {
		return editColumn;
	}

	@Override
	public AreaEditView getAreaEditView() {
		if (areaEditView == null) {
			areaEditView = new AreaEditView();
			add(areaEditView);
		}
		return areaEditView;
	}
	@Override
	public void hideEditForm() {
		super.hideEditForm();
		getAreaEditView().setStyleName("edit invisible");
	}
	
	@Override
	public void showEditForm() {
		super.showEditForm();
		getAreaEditView().setStyleName("edit visible");
	}
}
