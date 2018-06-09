package net.ionoff.center.client.schedule;

import com.google.gwt.cell.client.ClickableTextCell;
import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.user.cellview.client.CellTable;
import com.google.gwt.user.cellview.client.Column;
import com.google.gwt.user.cellview.client.TextColumn;

import gwt.material.design.client.constants.IconType;
import net.ionoff.center.client.base.AbstractTableView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.locale.ProjectLocale;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.ScheduleDto;

public class ScheduleTableView extends AbstractTableView<ScheduleDto> implements ScheduleTablePresenter.Display {
	
	private Column<ScheduleDto, String> nameColumn;
	private Column<ScheduleDto, String> orderColumn;
	private Column<ScheduleDto, String> deviceColumn;
	private Column<ScheduleDto, String> editColumn;
	
	
	private ScheduleEditView scheduleEditView;
	
	public ScheduleTableView() {
		super("schedules");
		getToolBarView().getLblTitle().setIconType(IconType.SCHEDULE);
		getToolBarView().getLblTitle().setText(ProjectLocale.getProjectConst().schedule());
	}

	@Override
	public CellTable<ScheduleDto> createCellTable() {
		CellTable<ScheduleDto> cellTable = new CellTable<ScheduleDto>();

		TextColumn<ScheduleDto> idColumn = createIdColumn();
		cellTable.addColumn(idColumn, ID);
		cellTable.setColumnWidth(idColumn, COLUMN_ID_WIDTH, Unit.PX);

		nameColumn = createNameColumn();
		nameColumn.setSortable(true);
		cellTable.addColumn(nameColumn, AdminLocale.getAdminConst().name());
		cellTable.setColumnWidth(nameColumn, COLUMN_NAME_WIDTH, Unit.PX);

		orderColumn = createOrderColumn();
		orderColumn.setSortable(true);
		cellTable.addColumn(orderColumn, AdminLocale.getAdminConst().order());
		cellTable.setColumnWidth(orderColumn, 70, Unit.PX);

		deviceColumn = createDeviceColumn();
		cellTable.addColumn(deviceColumn, AdminLocale.getAdminConst().device());
		cellTable.setColumnWidth(deviceColumn, COLUMN_NAME_WIDTH, Unit.PX);
		
		editColumn = createEditColumn();
		cellTable.addColumn(editColumn, "");
		
		return cellTable;
	}

	private Column<ScheduleDto, String> createOrderColumn() {
		Column<ScheduleDto, String> column = new TextColumn<ScheduleDto>() {
			@Override
			public String getValue(ScheduleDto object) {
				return object.getOrder() == null ? "" : object.getOrder().toString();
			}
		};
		return column;
	}

	private Column<ScheduleDto, String> createDeviceColumn() {
		ClickableTextCell clickableTimeTextCell = new ClickableTextCell();
	    Column<ScheduleDto, String> column = new Column<ScheduleDto, String>(clickableTimeTextCell) {
	    	@Override
	    	public String getValue(ScheduleDto object) {
	    		if (object.getDeviceId() == null || object.getDeviceName() == null) {
	    			return "";
	    		}
	    		return BaseDto.formatNameID(object.getDeviceName(), object.getDeviceId());
	    	}
	    };
		return column;
	}

	@Override
	public Column<ScheduleDto, String> getNameColumn() { 
		return this.nameColumn;
	}

	@Override
	public Column<ScheduleDto, String> getDeviceColumn() {
		return deviceColumn;
	}
	
	@Override
	public Column<ScheduleDto, String> getEditColumn() {
		return editColumn;
	}
	
	@Override
	public ScheduleEditView getScheduleEditView() {
		if (scheduleEditView == null) {
			scheduleEditView = new ScheduleEditView();
			add(scheduleEditView);
		}
		return scheduleEditView;
	}
	@Override
	public void hideEditForm() {
		super.hideEditForm();
		getScheduleEditView().setStyleName("edit invisible");
	}
	
	@Override
	public void showEditForm() {
		super.showEditForm();
		getScheduleEditView().setStyleName("edit visible");
	}
}
