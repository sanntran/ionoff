package net.ionoff.center.client.sensor;

import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.user.cellview.client.CellTable;
import com.google.gwt.user.cellview.client.Column;
import com.google.gwt.user.cellview.client.TextColumn;

import gwt.material.design.client.constants.IconType;
import net.ionoff.center.client.common.AbstractTableView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.locale.ProjectLocale;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.SensorDto;

public class SensorTableView extends AbstractTableView<SensorDto> implements SensorTablePresenter.Display {
	
	private Column<SensorDto, String> nameColumn;
	private Column<SensorDto, String> controllerColumn;
	private Column<SensorDto, String> editColumn;
	
	private SensorEditView sensorEditView;
			
	public SensorTableView() {
		super("sensors");
		
		getToolBarView().getLblTitle().setIconType(IconType.WIFI_TETHERING);
		getToolBarView().getLblTitle().setText(ProjectLocale.getProjectConst().sensor());
	}

	@Override
	public CellTable<SensorDto> createCellTable() {
		CellTable<SensorDto> cellTable = new CellTable<SensorDto>();
		
		TextColumn<SensorDto> idColumn = createIdColumn();
		cellTable.addColumn(idColumn, ID);
		cellTable.setColumnWidth(idColumn, COLUMN_ID_WIDTH, Unit.PX);

		nameColumn = createNameColumn();
		nameColumn.setSortable(true);
		cellTable.addColumn(nameColumn, AdminLocale.getAdminConst().name());
		cellTable.setColumnWidth(nameColumn, COLUMN_NAME_WIDTH, Unit.PX);

		controllerColumn =  createControllerColumn();
		cellTable.addColumn(controllerColumn, AdminLocale.getAdminConst().controller());
		cellTable.setColumnWidth(controllerColumn, COLUMN_NAME_WIDTH, Unit.PX);
		
		editColumn = createEditColumn();
		cellTable.addColumn(editColumn, "");
		
		return cellTable;
	}

	private Column<SensorDto, String> createControllerColumn() {
	    Column<SensorDto, String> column = new TextColumn<SensorDto>() {
	    	@Override
	    	public String getValue(SensorDto object) {
	    		if (object.getControllerId() == null || object.getControllerName() == null) {
	    			return AdminLocale.getAdminConst().none();
	    		}
	    		return BaseDto.formatNameID(object.getControllerName(), object.getControllerId());
	    	}
	    };
		return column;
	}
	
	@Override
	public Column<SensorDto, String> getNameColumn() {
		return nameColumn;
	}
	@Override
	public Column<SensorDto, String> getControllerColumn() {
		return controllerColumn;
	}

	@Override
	public Column<SensorDto, String> getEditColumn() {
		return editColumn;
	}
	
	@Override
	public SensorEditView getSensorEditView() {
		if (sensorEditView == null) {
			sensorEditView = new SensorEditView();
			add(sensorEditView);
		}
		return sensorEditView;
	}
	
	@Override
	public void hideEditForm() {
		super.hideEditForm();
		getSensorEditView().setStyleName("edit invisible");
	}
	
	@Override
	public void showEditForm() {
		super.showEditForm();
		getSensorEditView().setStyleName("edit visible");
	}
}
