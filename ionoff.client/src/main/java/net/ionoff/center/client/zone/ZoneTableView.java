package net.ionoff.center.client.zone;

import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.user.cellview.client.CellTable;
import com.google.gwt.user.cellview.client.Column;
import com.google.gwt.user.cellview.client.TextColumn;

import gwt.material.design.client.constants.IconType;
import net.ionoff.center.client.common.AbstractTableView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.locale.ProjectLocale;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.ZoneDto;

public class ZoneTableView extends AbstractTableView<ZoneDto> implements ZoneTablePresenter.Display {
	
	private Column<ZoneDto, String> nameColumn;
	private Column<ZoneDto, String> areaColumn;
	private Column<ZoneDto, String> orderColumn;
	private Column<ZoneDto, String> editColumn;
	private ZoneEditView zoneEditView;
	
	public ZoneTableView() {
		super("zones");
		getToolBarView().getLblTitle().setIconType(IconType.CROP_3_2);
		getToolBarView().getLblTitle().setText(ProjectLocale.getProjectConst().zone());
	}

	@Override
	public CellTable<ZoneDto> createCellTable() {
		CellTable<ZoneDto> cellTable = new CellTable<ZoneDto>();
		
		TextColumn<ZoneDto> idColumn = createIdColumn();
		cellTable.addColumn(idColumn, ID);
		cellTable.setColumnWidth(idColumn, COLUMN_ID_WIDTH, Unit.PX);

		nameColumn = createNameColumn();
		nameColumn.setSortable(true);
		cellTable.addColumn(nameColumn, AdminLocale.getAdminConst().name());
		cellTable.setColumnWidth(nameColumn, COLUMN_NAME_WIDTH, Unit.PX);
		
		orderColumn = createOrderColumn();
		orderColumn.setSortable(true);
		cellTable.addColumn(orderColumn, AdminLocale.getAdminConst().order());
		cellTable.setColumnWidth(orderColumn, 80, Unit.PX);

		areaColumn =  createAreaColumn();
		areaColumn.setSortable(true);
		cellTable.addColumn(areaColumn, AdminLocale.getAdminConst().area());
		
		editColumn = createEditColumn();
		cellTable.addColumn(editColumn, "");
		
		return cellTable;
	}

	private Column<ZoneDto, String> createAreaColumn() {
		Column<ZoneDto, String> column = new TextColumn<ZoneDto>() {
	    	@Override
	    	public String getValue(ZoneDto object) {
	    		if (object.getAreaId() == null || object.getAreaName() == null) {
	    			return "";
	    		}
	    		return BaseDto.formatNameID(object.getAreaName(), object.getAreaId());
	    	}
	    };
		return column;
	}
	
	private Column<ZoneDto, String> createOrderColumn() {
		Column<ZoneDto, String> column = new TextColumn<ZoneDto>() {
	    	@Override
	    	public String getValue(ZoneDto object) {
	    		if (object.getOrder() == null) {
	    			return "";
	    		}
	    		return object.getOrder() + "";
	    	}
	    };
		return column;
	}
	
	@Override
	public Column<ZoneDto, String> getNameColumn() {
		return nameColumn;
	}
	@Override
	public Column<ZoneDto, String> getAreaColumn() {
		return areaColumn;
	}
	@Override
	public Column<ZoneDto, String> getOrderColumn() {
		return orderColumn;
	}
	@Override
	public Column<ZoneDto, String> getEditColumn() {
		return editColumn;
	}

	@Override
	public ZoneEditView getZoneEditView() {
		if (zoneEditView == null) {
			zoneEditView = new ZoneEditView();
			add(zoneEditView);
		}
		return zoneEditView;
	}
	
	@Override
	public void hideEditForm() {
		super.hideEditForm();
		getZoneEditView().setStyleName("edit invisible");
	}
	
	@Override
	public void showEditForm() {
		super.showEditForm();
		getZoneEditView().setStyleName("edit visible");
	}
}
