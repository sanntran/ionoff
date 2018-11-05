package net.ionoff.center.client.device;

import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.user.cellview.client.CellTable;
import com.google.gwt.user.cellview.client.Column;
import com.google.gwt.user.cellview.client.TextColumn;

import gwt.material.design.client.constants.IconType;
import net.ionoff.center.client.base.AbstractTableView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.locale.ProjectLocale;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.DeviceDto;

public class DeviceTableView extends AbstractTableView<DeviceDto> implements DeviceTablePresenter.Display {
	
	private Column<DeviceDto, String> nameColumn;
	private Column<DeviceDto, String> zoneColumn;
	private Column<DeviceDto, String> orderColumn;
	private Column<DeviceDto, String> editColumn;
	
	private DeviceEditView deviceEditView;
	
	public DeviceTableView() {
		super("devices");
		getToolBarView().getLblTitle().setIconType(IconType.DEVICES_OTHER);
		getToolBarView().getLblTitle().setText(ProjectLocale.getProjectConst().device());
	}

	@Override
	public CellTable<DeviceDto> createCellTable() {
		CellTable<DeviceDto> cellTable = new CellTable<DeviceDto>();

		nameColumn = createNameColumn();
		nameColumn.setSortable(true);
		cellTable.addColumn(nameColumn, AdminLocale.getAdminConst().name());
		cellTable.setColumnWidth(nameColumn, COLUMN_NAME_WIDTH, Unit.PX);
		
		orderColumn =  createOrderColumn();
		orderColumn.setSortable(true);
		cellTable.addColumn(orderColumn, AdminLocale.getAdminConst().order());
		cellTable.setColumnWidth(orderColumn, 50, Unit.PX);
		
		zoneColumn =  createZoneColumn();
		zoneColumn.setSortable(true);
		cellTable.addColumn(zoneColumn, AdminLocale.getAdminConst().zone());

		editColumn = createEditColumn();
		cellTable.addColumn(editColumn, "");
		cellTable.setColumnWidth(editColumn, COLUMN_EDIT_WIDTH, Unit.PX);

		return cellTable;
	}
	
	private Column<DeviceDto, String> createZoneColumn() {
	    Column<DeviceDto, String> column = new TextColumn<DeviceDto>() {
	    	@Override
	    	public String getValue(DeviceDto object) {
	    		if (object.getZoneId() == null || object.getZoneName() == null) {
	    			return "";
	    		}
	    		return BaseDto.formatNameID(object.getZoneName(), object.getZoneId());
	    	}
	    };
		return column;
	}
	
	private Column<DeviceDto, String> createOrderColumn() {
	    Column<DeviceDto, String> column = new TextColumn<DeviceDto>() {
	    	@Override
	    	public String getValue(DeviceDto object) {
	    		if (object.getOrder() == null) {
	    			return "";
	    		}
	    		return object.getOrder() + "";
	    	}
	    };
		return column;
	}


	@Override
	public Column<DeviceDto, String> getEditColumn() {
		return editColumn;
	}
	
	@Override
	public DeviceEditView getDeviceEditView() {
		if (deviceEditView == null) {
			deviceEditView = new DeviceEditView();
			add(deviceEditView);
		}
		return deviceEditView;
	}
	@Override
	public void hideEditForm() {
		super.hideEditForm();
		getDeviceEditView().setStyleName("edit invisible");
	}
	
	@Override
	public void showEditForm() {
		super.showEditForm();
		getDeviceEditView().setStyleName("edit visible");
	}
}
