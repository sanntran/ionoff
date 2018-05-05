package net.ionoff.center.client.device;

import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.user.cellview.client.CellTable;
import com.google.gwt.user.cellview.client.Column;
import com.google.gwt.user.cellview.client.TextColumn;
import gwt.material.design.client.constants.IconType;
import net.ionoff.center.client.base.AbstractTableView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.shared.dto.SensorDataDto;

public class ScaleDataTableView extends AbstractTableView<SensorDataDto> implements ScaleDataTablePresenter.Display {
	
	private Column<SensorDataDto, String> timeColumn;
	private Column<SensorDataDto, String> valueColumn;
	private Column<SensorDataDto, String> indexColumn;
	private Column<SensorDataDto, String> editColumn;

	private ScaleDataEditView scaleDataEditView;

	public ScaleDataTableView() {
		super("scaledata");
		getToolBarView().getLblTitle().setIconType(IconType.DEVICES_OTHER);
		getToolBarView().getBtnAdd().setVisible(false);
		getToolBarView().getBtnRemove().setVisible(false);
		getToolBarView().getTextBoxKeyWord().setVisible(false);
		getToolBarView().getPanelDateInput().setVisible(true);
		getToolBarView().setDayPeriod(7);
	}

	@Override
	public CellTable<SensorDataDto> createCellTable() {
		CellTable<SensorDataDto> cellTable = new CellTable<SensorDataDto>();
		
		TextColumn<SensorDataDto> idColumn = createIdColumn();
		cellTable.addColumn(idColumn, ID);
		cellTable.setColumnWidth(idColumn, COLUMN_ID_WIDTH, Unit.PX);

		timeColumn = createTimeColumn();
		cellTable.addColumn(timeColumn, AdminLocale.getAdminConst().date());
		cellTable.setColumnWidth(timeColumn, COLUMN_NAME_WIDTH, Unit.PX);

		valueColumn =  createValueColumn();
		cellTable.addColumn(valueColumn, AdminLocale.getAdminConst().total());
		cellTable.setColumnWidth(valueColumn, COLUMN_NAME_WIDTH, Unit.PX);

		indexColumn =  createIndexColumn();
		cellTable.addColumn(indexColumn, AdminLocale.getAdminConst().index());
		cellTable.setColumnWidth(indexColumn, 100, Unit.PX);

		editColumn = createEditColumn();
		cellTable.addColumn(editColumn, "");

		return cellTable;
	}

	private Column<SensorDataDto, String> createTimeColumn() {
		Column<SensorDataDto, String> column = new TextColumn<SensorDataDto>() {
			@Override
			public String getValue(SensorDataDto object) {
				return object.getTime() == null ? "" : object.getTime().replaceAll(" 23:59:59", "");
			}
		};
		return column;
	}

	private Column<SensorDataDto, String> createValueColumn() {
	    Column<SensorDataDto, String> column = new TextColumn<SensorDataDto>() {
	    	@Override
	    	public String getValue(SensorDataDto object) {
				return String.valueOf(object.getValue());
	    	}
	    };
		return column;
	}

	private Column<SensorDataDto, String> createIndexColumn() {
		Column<SensorDataDto, String> column = new TextColumn<SensorDataDto>() {
			@Override
			public String getValue(SensorDataDto object) {
				return String.valueOf(object.getIndex());
			}
		};
		return column;
	}

	@Override
	public Column<SensorDataDto, String> getEditColumn() {
		return editColumn;
	}

	@Override
	public Column<SensorDataDto, String> getNameColumn() {
		return timeColumn;
	}

	@Override
	public ScaleDataEditView getScaleDataEditView() {
		if (scaleDataEditView == null) {
			scaleDataEditView = new ScaleDataEditView();
			add(scaleDataEditView);
		}
		return scaleDataEditView;
	}

	@Override
	public void hideEditForm() {
		super.hideEditForm();
		getScaleDataEditView().setStyleName("edit invisible");
	}

	@Override
	public void showEditForm() {
		super.showEditForm();
		getScaleDataEditView().setStyleName("edit visible");
	}
}
