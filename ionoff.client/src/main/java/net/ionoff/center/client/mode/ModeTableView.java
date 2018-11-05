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
	private Column<ModeDto, String> orderColumn;
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

		nameColumn = createNameColumn();
		nameColumn.setSortable(true);
		cellTable.addColumn(nameColumn, AdminLocale.getAdminConst().name());

		orderColumn = createOrderColumn();
		orderColumn.setSortable(true);
		cellTable.addColumn(orderColumn, AdminLocale.getAdminConst().order());
		cellTable.setColumnWidth(orderColumn, 70, Unit.PX);
		
		isScheduledColumn =  createIsScheduledColumn();
		cellTable.addColumn(isScheduledColumn, AdminLocale.getAdminConst().schedule());
		cellTable.setColumnWidth(isScheduledColumn, 70, Unit.PX);

		editColumn = createEditColumn();
		cellTable.addColumn(editColumn, "");
		cellTable.setColumnWidth(editColumn, COLUMN_EDIT_WIDTH, Unit.PX);

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

	private Column<ModeDto, String> createOrderColumn() {
		Column<ModeDto, String> column = new TextColumn<ModeDto>() {
			@Override
			public String getValue(ModeDto object) {
			return object.getOrder() == null ? "" : object.getOrder().toString();
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
