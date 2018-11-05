package net.ionoff.center.client.sensor;

import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.user.cellview.client.CellTable;
import com.google.gwt.user.cellview.client.Column;
import com.google.gwt.user.cellview.client.TextColumn;

import gwt.material.design.client.constants.IconType;
import net.ionoff.center.client.base.AbstractTableView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.locale.ProjectLocale;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.SensorDto;

public class SensorTableView extends AbstractTableView<SensorDto> implements SensorTablePresenter.Display {
	
	private Column<SensorDto, String> nameColumn;
	private Column<SensorDto, String> orderColumn;
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
		
		nameColumn = createNameColumn();
		nameColumn.setSortable(true);
		cellTable.addColumn(nameColumn, AdminLocale.getAdminConst().name());
		cellTable.setColumnWidth(nameColumn, COLUMN_NAME_WIDTH, Unit.PX);

		orderColumn = createOrderColumn();
		orderColumn.setSortable(true);
		cellTable.addColumn(orderColumn, AdminLocale.getAdminConst().order());
		cellTable.setColumnWidth(orderColumn, 70, Unit.PX);

		controllerColumn =  createDeviceColumn();
		controllerColumn.setSortable(true);
		cellTable.addColumn(controllerColumn, AdminLocale.getAdminConst().controller());

		editColumn = createEditColumn();
		cellTable.addColumn(editColumn, "");
		cellTable.setColumnWidth(editColumn, COLUMN_EDIT_WIDTH, Unit.PX);

		return cellTable;
	}

	private Column<SensorDto, String> createOrderColumn() {
		Column<SensorDto, String> column = new TextColumn<SensorDto>() {
			@Override
			public String getValue(SensorDto object) {
			return object.getOrder() == null ? "" : object.getOrder().toString();
			}
		};
		return column;
	}

	private Column<SensorDto, String> createDeviceColumn() {
	    Column<SensorDto, String> column = new TextColumn<SensorDto>() {
	    	@Override
	    	public String getValue(SensorDto object) {
	    		if (object.getDriverId() == null || object.getDriverName() == null) {
	    			return AdminLocale.getAdminConst().none();
	    		}
	    		return BaseDto.formatNameID(object.getDriverName(), object.getDriverId());
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
