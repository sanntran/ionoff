package net.ionoff.center.client.mode;

import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.user.cellview.client.CellTable;
import com.google.gwt.user.cellview.client.Column;
import com.google.gwt.user.cellview.client.TextColumn;

import gwt.material.design.client.constants.IconType;
import net.ionoff.center.client.base.AbstractTableView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.locale.ProjectLocale;
import net.ionoff.center.shared.dto.ModeDto;

public class ModeTableView extends AbstractTableView<ModeDto> implements ModeTablePresenter.Display {
	
	private Column<ModeDto, String> nameColumn;
	private Column<ModeDto, String> isScheduledColumn;
	private Column<ModeDto, String> editColumn;
	private ModeEditView modeEditView;
	
	public ModeTableView() {
		super("modes");
		getToolBarView().getLblTitle().setIconType(IconType.SETTINGS_BRIGHTNESS);
		getToolBarView().getLblTitle().setText(ProjectLocale.getProjectConst().mode());
		modeEditView = new ModeEditView();
		add(modeEditView);
	}

	@Override
	public CellTable<ModeDto> createCellTable() {
		CellTable<ModeDto> cellTable = new CellTable<ModeDto>();

		TextColumn<ModeDto> idColumn = createIdColumn();
		cellTable.addColumn(idColumn, ID);
		cellTable.setColumnWidth(idColumn, COLUMN_ID_WIDTH, Unit.PX);

		nameColumn = createNameColumn();
		nameColumn.setSortable(true);
		cellTable.addColumn(nameColumn, AdminLocale.getAdminConst().name());
		cellTable.setColumnWidth(nameColumn, COLUMN_NAME_WIDTH, Unit.PX);
		
		isScheduledColumn =  createIsScheduledColumn();
		cellTable.addColumn(isScheduledColumn, AdminLocale.getAdminConst().schedule());
		cellTable.setColumnWidth(isScheduledColumn, 125.0, Unit.PX);
		
		
		editColumn = createEditColumn();
		cellTable.addColumn(editColumn, "");
		
		return cellTable;
	}
	
	private Column<ModeDto, String> createIsScheduledColumn() {
	    Column<ModeDto, String> column = new TextColumn<ModeDto>() {
			@Override
			public String getValue(ModeDto object) {
				if (object.getIsScheduled() == null || object.getIsScheduled().booleanValue() == false) {
					return AdminLocale.getAdminConst().no();
				}
				return AdminLocale.getAdminConst().yes();
			}
	    };
		return column;
	}

	@Override
	public Column<ModeDto, String> getNameColumn() { 
		return this.nameColumn;
	}

	@Override
	public Column<ModeDto, String> getEditColumn() {
		return editColumn;
	}
	
	@Override
	public ModeEditView getModeEditView() {
		return modeEditView;
	}
	
	@Override
	public void hideEditForm() {
		super.hideEditForm();
		getModeEditView().setStyleName("edit invisible");
	}
	
	@Override
	public void showEditForm() {
		super.showEditForm();
		getModeEditView().setStyleName("edit visible");
	}
}
